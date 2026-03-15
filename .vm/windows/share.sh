#!/usr/bin/env bash
# share.sh - Manage virtiofs directory sharing with the Windows VM.
#
# Usage:
#   share.sh configure              # One-time: add memoryBacking to VM config (needs restart)
#   share.sh attach [--dir PATH]    # Hotplug virtiofs share to running VM
#   share.sh detach                 # Hot-remove virtiofs share
#   share.sh status                 # Show whether virtiofs is currently attached
#
# The shared directory defaults to .vm/windows/shared/ (relative to this script).
# Pass --dir to override.
set -euo pipefail

VM_NAME="windows-2022"
MOUNT_TAG="emacs-module-rs"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DEFAULT_SHARE_DIR="$SCRIPT_DIR/shared"

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

die() { echo "Error: $*" >&2; exit 1; }

vm_is_running() {
    virsh domstate "$VM_NAME" 2>/dev/null | grep -q "^running$"
}

fs_is_attached() {
    virsh domfsinfo "$VM_NAME" 2>/dev/null | grep -q "$MOUNT_TAG"
}

# Generate a temporary <filesystem> device XML for attach/detach operations.
make_device_xml() {
    local share_dir="$1"
    local tmpfile
    tmpfile=$(mktemp /tmp/virtiofs-device-XXXXXX.xml)
    cat > "$tmpfile" <<EOF
<filesystem type='mount' accessmode='passthrough'>
  <driver type='virtiofs'/>
  <source dir='$share_dir'/>
  <target dir='$MOUNT_TAG'/>
</filesystem>
EOF
    echo "$tmpfile"
}

# ---------------------------------------------------------------------------
# Commands
# ---------------------------------------------------------------------------

cmd_configure() {
    echo "Adding <memoryBacking> to VM '$VM_NAME'..."
    echo "(This requires a VM restart to take effect.)"
    echo ""

    local tmpxml
    tmpxml=$(mktemp /tmp/win-vm-XXXXXX.xml)
    trap "rm -f '$tmpxml'" EXIT

    virsh dumpxml "$VM_NAME" > "$tmpxml"

    # Check if memoryBacking is already present.
    if grep -q "<memoryBacking>" "$tmpxml"; then
        echo "memoryBacking is already configured — nothing to do."
        return
    fi

    # Inject <memoryBacking> using Python 3 (no extra tools needed).
    python3 - "$tmpxml" <<'PYEOF'
import sys
import xml.etree.ElementTree as ET

ET.register_namespace('', '')

path = sys.argv[1]
tree = ET.parse(path)
root = tree.getroot()

mb = ET.SubElement(root, 'memoryBacking')
ET.SubElement(mb, 'source').set('type', 'memfd')
ET.SubElement(mb, 'access').set('mode', 'shared')

# ElementTree strips the XML declaration; write with it manually.
xml_str = ET.tostring(root, encoding='unicode')
with open(path, 'w') as f:
    f.write('<?xml version="1.0" encoding="UTF-8"?>\n')
    f.write(xml_str)
    f.write('\n')
PYEOF

    virsh define "$tmpxml"
    echo ""
    echo "Done. Restart the VM for memoryBacking to take effect:"
    echo "  virsh reboot $VM_NAME"
}

cmd_attach() {
    local share_dir="$DEFAULT_SHARE_DIR"

    while [[ $# -gt 0 ]]; do
        case "$1" in
            --dir) share_dir="$2"; shift 2 ;;
            --dir=*) share_dir="${1#--dir=}"; shift ;;
            *) die "Unknown option: $1" ;;
        esac
    done

    share_dir=$(realpath "$share_dir")

    vm_is_running || die "VM '$VM_NAME' is not running."
    fs_is_attached && { echo "virtiofs is already attached."; return; }

    mkdir -p "$share_dir"
    echo "Attaching virtiofs share: $share_dir -> tag '$MOUNT_TAG'"

    local devxml
    devxml=$(make_device_xml "$share_dir")
    trap "rm -f '$devxml'" EXIT

    virsh attach-device "$VM_NAME" "$devxml" --live
    echo "Done. The share is now accessible inside the VM."
    echo "(Run setup-virtiofs.ps1 on the guest if VirtIO-FS driver is not yet installed.)"
}

cmd_detach() {
    vm_is_running || die "VM '$VM_NAME' is not running."
    # This seems to incorrectly report "not attached" even when Windows sees the device.
    fs_is_attached || { echo "virtiofs is not currently attached."; return; }

    # Use any path for detach — virsh matches by mount tag.
    local devxml
    devxml=$(make_device_xml "/nonexistent")
    trap "rm -f '$devxml'" EXIT

    virsh detach-device "$VM_NAME" "$devxml" --live
    echo "virtiofs share detached."
}

cmd_status() {
    if ! vm_is_running; then
        echo "VM '$VM_NAME' is not running."
        return
    fi

    echo "VM: $VM_NAME (running)"
    echo ""
    if fs_is_attached; then
        echo "virtiofs: ATTACHED"
        echo ""
        virsh domfsinfo "$VM_NAME" 2>/dev/null || true
    else
        echo "virtiofs: not attached"
    fi

    echo ""
    echo "memoryBacking:"
    if virsh dumpxml "$VM_NAME" 2>/dev/null | grep -q "<memoryBacking>"; then
        echo "  configured (restart was required if just added)"
    else
        echo "  NOT configured — run: share.sh configure"
    fi
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

cmd="${1:-}"
shift || true

case "$cmd" in
    configure) cmd_configure "$@" ;;
    attach)    cmd_attach    "$@" ;;
    detach)    cmd_detach    "$@" ;;
    status)    cmd_status    "$@" ;;
    *)
        echo "Usage: share.sh <configure|attach|detach|status> [--dir PATH]"
        exit 1
        ;;
esac

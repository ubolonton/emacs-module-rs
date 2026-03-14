#!/usr/bin/env bash
# Creates the Windows Server 2022 VM and starts the unattended installation.
#
# virt-install returns immediately (--noautoconsole); installation runs in the
# background. Monitor progress with: virt-viewer windows-2022
#
# Prerequisites:
#   - qemu-kvm / qemu-system-x86
#   - libvirt + virt-install
#   - ovmf  (UEFI firmware: sudo apt install ovmf)
#   - arp-scan  (sudo apt install arp-scan) — for IP discovery
#
# Run in order:
#   1. download-isos.sh   — download Windows + VirtIO ISOs
#   2. prepare-iso.sh     — inject Autounattend.xml into Windows ISO
#   3. create-vm.sh       — (this script) create the VM and start installation
#   4. After install completes and VM shuts down: run boot-vm.sh
set -euo pipefail

# Physical NIC — macvtap source (no IP assigned to this interface directly).
MACVTAP_IFACE="enp7s0"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ISO_DIR="$SCRIPT_DIR/isos"

VM_NAME="windows-2022"
RAM_MB=8192
VCPUS=4
DISK_SIZE_GB=80
VOL_PATH="lroot/vm/$VM_NAME/disk0"

WIN_ISO_UNATTENDED="$ISO_DIR/windows-server-2022-eval-unattended.iso"
VIRTIO_ISO="$ISO_DIR/virtio-win.iso"

# ---------------------------------------------------------------------------
# Checks
# ---------------------------------------------------------------------------
for tool in virt-install virsh; do
    if ! command -v "$tool" &>/dev/null; then
        echo "ERROR: '$tool' not found. Install the required packages:"
        echo "  Debian/Ubuntu: sudo apt install virtinst"
        echo "  Fedora/RHEL:   sudo dnf install virt-install"
        exit 1
    fi
done

[[ -f "$WIN_ISO_UNATTENDED" ]] || { echo "ERROR: Unattended ISO not found: $WIN_ISO_UNATTENDED"; echo "Run prepare-iso.sh first."; exit 1; }
[[ -f "$VIRTIO_ISO" ]]         || { echo "ERROR: VirtIO ISO not found: $VIRTIO_ISO"; echo "Run download-isos.sh first."; exit 1; }

if virsh domstate "$VM_NAME" &>/dev/null; then
    echo "ERROR: VM '$VM_NAME' already exists. To recreate it:"
    echo "  virsh destroy $VM_NAME"
    echo "  virsh undefine $VM_NAME --remove-all-storage"
    exit 1
fi

# ---------------------------------------------------------------------------
# Create disk volume
# ---------------------------------------------------------------------------
zfs create -V "$DISK_SIZE_GB"G -p -s -o primarycache=metadata "$VOL_PATH"

# ---------------------------------------------------------------------------
# Create the VM and start installation
# ---------------------------------------------------------------------------
echo ""
echo "Creating VM: $VM_NAME"
echo "  Disk:   $VOL_PATH (${DISK_SIZE_GB}GB)"
echo "  RAM:    ${RAM_MB}MB"
echo "  vCPUs:  $VCPUS"
echo ""

virt-install \
    --name "$VM_NAME" \
    --ram "$RAM_MB" \
    --vcpus "$VCPUS" \
    --boot uefi \
    --disk type=block,source.dev="/dev/zvol/$VOL_PATH",target.bus=sata \
    --cdrom "$WIN_ISO_UNATTENDED" \
    --disk "path=$VIRTIO_ISO,device=cdrom" \
    --os-variant win2k22 \
    --network type=direct,source="$MACVTAP_IFACE",source.mode=bridge,model=e1000e,mac.address=52:54:00:cd:a3:b9 \
    --noautoconsole \
    --noreboot \
    --graphics spice,listen=127.0.0.1

echo ""
echo "================================================================="
echo " Installation started. This takes 10-20 minutes."
echo ""
echo " Monitor progress:"
echo "   virt-viewer $VM_NAME"
echo ""
echo " When the VM shuts down (installation complete), run:"
echo "   .vm/windows/boot-vm.sh"
echo "================================================================="

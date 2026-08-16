#!/usr/bin/env bash
# Ejects installation media and boots the VM after installation completes.
#
# Run this once after create-vm.sh's installation is done (VM is shut off).
# Then wait for SSH; get the IP with: ./get-vm-ip.sh
#
# Prerequisites:
#   - arp-scan  (sudo apt install arp-scan) — used by get-vm-ip.sh
set -euo pipefail

VM_NAME="windows-2022"

# ---------------------------------------------------------------------------
# Check VM state
# ---------------------------------------------------------------------------
STATE=$(virsh domstate "$VM_NAME" 2>/dev/null || true)
if [[ -z "$STATE" ]]; then
    echo "ERROR: VM '$VM_NAME' not found. Run create-vm.sh first."
    exit 1
fi
if [[ "$STATE" != "shut off" ]]; then
    echo "ERROR: VM is '$STATE', expected 'shut off'."
    echo "Wait for installation to complete (monitor with: virt-viewer $VM_NAME)"
    exit 1
fi

# ---------------------------------------------------------------------------
# Eject installation media
# ---------------------------------------------------------------------------
# Without ejecting, UEFI will try to boot from the Windows ISO again on start
# and show a "choose OS to boot" prompt.
echo "Ejecting installation media..."
for dev in $(virsh domblklist "$VM_NAME" --details 2>/dev/null \
             | awk '$2 == "cdrom" {print $3}'); do
    virsh change-media "$VM_NAME" "$dev" --eject --force 2>/dev/null || true
done

# ---------------------------------------------------------------------------
# Start the VM
# ---------------------------------------------------------------------------
echo "Starting VM for first boot..."
virsh start "$VM_NAME"

echo ""
echo "================================================================="
echo " VM is booting. Windows first-logon setup runs automatically."
echo " (OpenSSH installation, firewall rule, etc.)"
echo ""
echo " Wait a few minutes, then:"
echo "   .vm/windows/get-vm-ip.sh     # find the IP"
echo "   ssh Administrator@<ip>              # connect"
echo "================================================================="

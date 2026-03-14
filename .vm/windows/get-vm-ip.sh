#!/usr/bin/env bash
# Prints the IP address of the Windows VM by scanning the LAN with arp-scan.
#
# virsh domifaddr does not work with macvtap: the VM gets its IP directly from
# the LAN DHCP server and libvirt never sees the lease. This script looks up
# the VM's MAC from libvirt, then uses arp-scan to find the matching IP.
#
# Prerequisites:
#   - arp-scan  (sudo apt install arp-scan)
set -euo pipefail

# macvtap source (physical NIC, no IP assigned)
MACVTAP_IFACE="enp7s0"
# macvlan interface on the host (has the LAN IP; used for arp-scan subnet)
MACVLAN_IFACE="macvlan0"
VM_NAME="windows-2022"

VM_MAC=$(virsh domiflist "$VM_NAME" 2>/dev/null | awk 'NR>2 && NF {print $5}' | head -1)

if [[ -z "$VM_MAC" ]]; then
    echo "ERROR: Could not get MAC address for VM '$VM_NAME'." >&2
    echo "Is the VM defined? Check: virsh list --all" >&2
    exit 1
fi

VM_IP=$(sudo arp-scan --interface="$MACVLAN_IFACE" --localnet 2>/dev/null \
    | awk -v mac="$VM_MAC" 'tolower($2) == tolower(mac) {print $1}' \
    | head -1)

if [[ -z "$VM_IP" ]]; then
    echo "ERROR: VM not found on network (MAC: $VM_MAC)." >&2
    echo "Is the VM running? Check: virsh list" >&2
    exit 1
fi

echo "$VM_IP"

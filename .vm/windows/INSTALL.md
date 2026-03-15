# Windows VM Setup

Scripts for creating a headless Windows Server 2022 VM on Linux (KVM/libvirt),
accessible via SSH, for cross-platform Rust testing.

## One-time host setup

**macvlan interface on the host** — required for host-to-VM connectivity with
macvtap networking (see Design Decisions below). The host must have a macvlan
interface configured as a sibling to `enp7s0`, with the host's LAN IP assigned
to it rather than directly to `enp7s0`.

```shell
sudo apt install libvirt-clients libvirt-daemon qemu-system virt-install virt-viewer mtools dosfstools virtiofsd
```

## Workflow

Run the scripts in order:

```bash
.vm/windows/download-isos.sh               # 1. Download ISOs
.vm/windows/prepare-iso.sh                 # 2. Inject Autounattend.xml into Windows ISO
.vm/windows/create-vm.sh                   # 3. Create VM, start installation (returns immediately)
virt-viewer windows-2022                          # 4. Monitor — wait for VM to shut off (~15 min)
.vm/windows/boot-vm.sh                     # 5. Eject ISOs, boot into Windows
VM_IP=$(.vm/windows/get-vm-ip.sh)          # 6. Find the VM's IP (wait a minute after boot)
ssh Administrator@"$VM_IP"                        # 7. Connect
# 8. Install Scoop + Rust — see SETUP.md
```

## Design Decisions

### Autounattend.xml must live inside the Windows ISO

Windows Setup only scans its own boot media for `Autounattend.xml`. Secondary
CD-ROMs (a common approach) are ignored by Windows Server 2022 Setup. The file
must be at the root of the Windows ISO itself.

`prepare-iso.sh` handles this: extract the original ISO with `7z`, inject
`Autounattend.xml`, repack with `xorriso`.

### 7z for ISO extraction (not xorriso)

The Windows Server 2022 ISO uses UDF as its primary filesystem (ISO 9660 is
only a compatibility layer with minimal content). `xorriso`'s read mode only
parses ISO 9660 — it sees almost nothing. `7z` is UDF-aware and extracts the
full tree correctly.

### xorriso for ISO repacking

`xorriso -as mkisofs` repacks the extracted tree into a bootable ISO with two
El Torito boot entries:
- **BIOS**: `boot/etfsboot.com` (legacy boot sector)
- **EFI**: `efi/microsoft/boot/efisys_noprompt.bin` (UEFI chainloader, no prompt)

The `-V` volume label must match the original (`SSS_X64FREE_EN-US_DV9`) — some
editions of Windows Setup use it to locate the source drive.

### UEFI firmware (OVMF), not SeaBIOS

`--boot uefi` forces libvirt to use OVMF (UEFI). Without it, virt-install may
select SeaBIOS (legacy BIOS) for this hardware configuration. The repacked ISO's
BIOS boot chain (`etfsboot.com`) relies on a hybrid MBR system area that is not
preserved when repacking — it hangs at "Booting from DVD/CD". The EFI boot path
works correctly with the repacked ISO.

### SATA disk bus, not VirtIO

VirtIO block devices require a driver (`viostor`) that is not present in the
WinPE environment used by Windows Setup. Without the driver, Windows cannot
detect the disk at all, and `DiskConfiguration` in `Autounattend.xml` fails
with an error.

Using `target.bus=sata` (AHCI) avoids this: Windows has native AHCI support in
WinPE. VirtIO disk support can be added post-install if needed, but SATA
performs adequately for a dev/test VM.

The VirtIO ISO is still attached to provide drivers for the network adapter
(used during setup) and for potential future use.

### Partition layout (EFI + MSR + Primary)

UEFI installs require at least an EFI System Partition (ESP) and an MSR:

| # | Type    | Size    | Purpose                        |
|---|---------|---------|--------------------------------|
| 1 | EFI     | 200 MB  | EFI System Partition (FAT32)   |
| 2 | MSR     | 128 MB  | Microsoft Reserved             |
| 3 | Primary | rest    | Windows (NTFS, drive C:)       |

`InstallTo` must reference PartitionID **3** (the Primary partition). PartitionID
uses 1-based indexing matching the `<Order>` of `<CreatePartition>` entries.

### macvtap bridging + e1000e NIC

`--network type=direct,source=enp7s0,source.mode=bridge,model=e1000e` attaches
the VM directly to the physical NIC via macvtap. The VM gets its own LAN IP
from the existing DHCP server. `get-vm-ip.sh` finds it via `arp-scan`.

**Host connectivity via macvlan sibling**: macvtap has a kernel-level
restriction where the host cannot communicate with VMs through the parent NIC
(`enp7s0`) directly. The workaround is to configure a macvlan interface
(`macvlan0`) on the host as a sibling to `enp7s0`. The host's LAN IP is
assigned to `macvlan0`, not to `enp7s0` directly. Sibling macvlan/macvtap
interfaces can exchange traffic with each other.

`arp-scan` must use `macvlan0` (not `enp7s0`) because `enp7s0` has no IP and
`arp-scan` derives the scan subnet from the interface address.

**Why e1000e, not virtio**: VirtIO network (`model=virtio`) requires the
`NetKVM` driver, which is on the VirtIO ISO but is not automatically installed
into Windows during setup. Without it, `ipconfig` shows no adapters. The
E1000e NIC uses a driver built into Windows; no additional installation needed.

### ZFS sparse volume for VM disk

```bash
zfs create -V 80G -p -s -o primarycache=metadata lroot/vm/windows-2022/disk0
```

`-s` makes the volume sparse (thin-provisioned) — only used blocks consume pool
space. `primarycache=metadata` avoids double-caching VM guest data (the guest OS
caches it already).

### OpenSSH Server via FirstLogonCommands

`Autounattend.xml` installs and configures OpenSSH Server on first logon:
1. `Add-WindowsCapability` — installs the optional feature
2. `Set-Service sshd -StartupType Automatic; Start-Service sshd`
3. Firewall rule for TCP 22
4. Registry key to set PowerShell as the SSH default shell

This makes the VM available for SSH immediately after the unattended install
completes, without any manual interaction.

## Challenges and Solutions

### VM stuck at language selection screen — floppy approach

**Problem**: Classic approach is to pass `Autounattend.xml` via a virtual floppy
(`A:\`). OVMF (UEFI firmware) has no floppy controller, so WinPE never finds the
file.

**Solution**: Switched to injecting the file into the Windows ISO itself (see
above).

### VM stuck at language selection screen — secondary CD-ROM approach

**Problem**: Tried attaching a separate ISO containing only `Autounattend.xml`
as a second CD-ROM. Windows Server 2022 Setup does not scan secondary media.

**Solution**: The file must be in the Windows ISO itself.

### xorriso fails on Windows ISO (`-boot_image any replay`)

**Problem**: `xorriso -indev <win.iso> -outdev <out.iso> -boot_image any replay`
exits with code 32. The Windows ISO's El Torito boot images are "hidden" (not
regular files in the ISO 9660 directory tree), so xorriso can't locate them for
replay. Produced a broken 28-sector output file.

**Solution**: Use `7z x` to extract the full UDF filesystem, then `xorriso -as
mkisofs` to repack with explicit boot file paths.

### "Booting from DVD/CD..." hang

**Problem**: After switching to the injected ISO approach, the VM hung at the
BIOS boot message. The repacked ISO's BIOS boot chain requires a hybrid MBR
that is not preserved by xorriso.

**Solution**: Added `--boot uefi` to force OVMF. UEFI uses the EFI El Torito
entry (`efisys_noprompt.bin`) which works correctly with the repacked ISO.

### "Choose OS to boot" prompt on first boot after install

**Problem**: `--noreboot` shuts the VM down after installation, but the Windows
ISO and VirtIO ISO remain attached. When the VM is started again, UEFI tries to
boot from the Windows ISO and shows a boot selection prompt.

**Solution**: `boot-vm.sh` ejects all CD-ROM devices (via `virsh change-media
--eject`) before starting the VM. It checks that the VM is shut off first, so
it's safe to run only after installation completes. UEFI then boots from the HDD.

### macvtap: host cannot reach its own VMs (without macvlan)

**Problem**: macvtap was attempted for direct LAN access. The VM got an IP,
had internet access, and could ping other LAN machines — but the host machine
itself could not ping or SSH to it.

**Root cause**: The Linux kernel drops traffic between a macvtap interface and
its parent NIC at the software level. The host's IP was assigned directly to
`enp7s0`, making the host and VM unable to communicate.

**Solution**: Configure a macvlan interface (`macvlan0`) on the host as a
sibling to `enp7s0`, and assign the host's LAN IP to `macvlan0` instead of
directly to `enp7s0`. Sibling macvlan/macvtap interfaces can exchange traffic,
giving the host full connectivity to its VMs while keeping direct LAN access.

### `virsh domifaddr` / `arp-scan` finds nothing (virtio-net)

**Problem**: With `model=virtio` networking, the VM had no working NIC after
install — the VirtIO NetKVM driver is on the VirtIO ISO but not automatically
installed into Windows. No NIC driver → no DHCP request → no IP.

**Solution**: Changed NIC model to `e1000e`. Windows has a built-in E1000e
driver; the NIC works immediately after first boot.

### "Windows could not apply DiskConfiguration"

**Problem**: `Autounattend.xml` specified a VirtIO disk (`target.bus=virtio`).
WinPE has no VirtIO driver, so the disk is invisible during setup. The
`DiskConfiguration` step fails immediately.

**Solution**: Changed disk bus to `sata`. Windows has native AHCI support in
WinPE; no driver loading needed.

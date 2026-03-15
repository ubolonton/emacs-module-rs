#!/usr/bin/env pwsh
# setup-virtiofs.ps1 - One-time guest setup for virtiofs directory sharing.
#
# Installs:
#   - WinFsp (Windows FUSE layer)           via Scoop nonportable bucket
#   - VirtIO-FS driver (viofs.sys)          from the VirtIO ISO (kernel driver)
#   - VirtIO-FS service (virtiofs.exe)      from the VirtIO ISO (user-mode service)
#
# Prerequisites:
#   - bootstrap.ps1 has been run (Scoop is installed)
#   - The VirtIO drivers ISO is still mounted (run before boot-vm.sh ejects it)
#     OR re-attach the ISO manually: virsh change-media windows-2022 sdc <virtio.iso>
#   - On the host, share.sh configure + restart has been done
#   - On the host, share.sh attach is running (virtiofs device is present)
#
# Usage (from the Linux host):
#
#   VM_IP=$(.vm/windows/get-vm-ip.sh)
#   cat .vm/windows/setup-virtiofs.ps1 | ssh Administrator@"$VM_IP" \
#       "powershell -ExecutionPolicy Bypass -NonInteractive -"
#
param (
    [string]$VMHost = ""
)

# ---------------------------------------------------------------------------
# If -VMHost is given, re-run ourselves remotely via SSH
# ---------------------------------------------------------------------------
if ($VMHost -ne "") {
    $ScriptPath = $MyInvocation.MyCommand.Path
    Write-Host "Connecting to Administrator@$VMHost ..."
    Get-Content $ScriptPath | ssh `
        -o StrictHostKeyChecking=no `
        "Administrator@$VMHost" `
        "powershell -ExecutionPolicy Bypass -NonInteractive -"
    exit $LASTEXITCODE
}

# ---------------------------------------------------------------------------
# From here: running inside the Windows VM
# ---------------------------------------------------------------------------
$ErrorActionPreference = "Stop"

function Step($msg) {
    Write-Host ""
    Write-Host "==> $msg" -ForegroundColor Cyan
}

# ---------------------------------------------------------------------------
# WinFsp — required by the VirtIO-FS driver
# ---------------------------------------------------------------------------
Step "Installing WinFsp (via Scoop nonportable bucket)"

$winfspInstalled = Get-ItemProperty HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\* |
    Where-Object { $_.DisplayName -like "WinFsp*" } |
    Select-Object -First 1

if ($winfspInstalled) {
    Write-Host "  WinFsp already installed: $($winfspInstalled.DisplayVersion)"
} else {
    # Add nonportable bucket (idempotent)
    scoop bucket add nonportable 2>&1 | Out-Null
    scoop install winfsp-np
    Write-Host "  WinFsp installed."
}

# ---------------------------------------------------------------------------
# VirtIO-FS driver — from VirtIO ISO (mounted as the last CD-ROM drive)
# ---------------------------------------------------------------------------
Step "Installing VirtIO-FS (viofs) driver"

# Find the VirtIO ISO drive letter by looking for viofs directory.
$virtioLetter = $null
foreach ($drive in (Get-PSDrive -PSProvider FileSystem | Where-Object { $_.Root -match '^[D-Z]:\\$' })) {
    if (Test-Path "$($drive.Root)viofs") {
        $virtioLetter = $drive.Root
        break
    }
}

if (-not $virtioLetter) {
    Write-Host ""
    Write-Host "ERROR: VirtIO ISO not found on any CD-ROM drive." -ForegroundColor Red
    Write-Host "Re-attach the ISO on the host, then re-run this script:" -ForegroundColor Yellow
    Write-Host "  virsh change-media windows-2022 sdc <path-to-virtio-win.iso>" -ForegroundColor Yellow
    exit 1
}

Write-Host "  Found VirtIO ISO at $virtioLetter"
$infPath = $null
foreach ($osDir in @("2k22", "2k25", "2k19", "w11", "w10")) {
    $candidate = "${virtioLetter}viofs\$osDir\amd64\viofs.inf"
    if (Test-Path $candidate) { $infPath = $candidate; break }
}
if (-not $infPath) {
    Write-Host "ERROR: viofs.inf not found under ${virtioLetter}viofs\. Check the VirtIO ISO version." -ForegroundColor Red
    exit 1
}

Write-Host "  Installing driver from: $infPath"
pnputil /add-driver $infPath /install | Write-Host

# ---------------------------------------------------------------------------
# VirtIO-FS Service — user-mode service (virtiofs.exe) that maps the mount
# tag to a drive letter. Not registered automatically by pnputil — must be
# done manually. The binary ships on the VirtIO ISO alongside the driver.
# ---------------------------------------------------------------------------
Step "Installing VirtIO-FS Service (virtiofs.exe)"

$svcName = "VirtioFsSvc"
$installDir = "C:\Program Files\VirtioFS"
$exeDest = "$installDir\virtiofs.exe"

$svc = Get-Service -Name $svcName -ErrorAction SilentlyContinue
if ($svc) {
    Write-Host "  VirtioFsSvc already registered."
} else {
    # virtiofs.exe lives alongside viofs.inf in the same per-OS directory.
    $exeSrc = Join-Path (Split-Path $infPath) "virtiofs.exe"
    if (-not (Test-Path $exeSrc)) {
        Write-Host ""
        Write-Host "ERROR: virtiofs.exe not found at: $exeSrc" -ForegroundColor Red
        exit 1
    }

    Write-Host "  Copying $exeSrc -> $exeDest"
    New-Item -ItemType Directory -Force -Path $installDir | Out-Null
    Copy-Item $exeSrc $exeDest -Force

    Write-Host "  Registering service..."
    sc.exe create $svcName binPath= "`"$exeDest`"" start= auto depend= VirtioFsDrv | Write-Host
    $svc = Get-Service -Name $svcName -ErrorAction SilentlyContinue
}

if ($svc -and $svc.Status -ne "Running") {
    Write-Host "  Starting $svcName..."
    sc.exe start $svcName | Write-Host
}

# ---------------------------------------------------------------------------
# Done
# ---------------------------------------------------------------------------
Write-Host ""
Write-Host "=================================================================" -ForegroundColor Green
Write-Host " virtiofs guest setup complete." -ForegroundColor Green
Write-Host ""
Write-Host "  The shared directory will appear as a new drive letter."
Write-Host "  If no drive letter is visible yet, ensure the host has run:"
Write-Host "    .vm/windows/share.sh attach"
Write-Host "=================================================================" -ForegroundColor Green

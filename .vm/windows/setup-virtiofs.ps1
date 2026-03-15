#!/usr/bin/env pwsh
# setup-virtiofs.ps1 - One-time guest setup for virtiofs directory sharing.
#
# Installs:
#   - WinFsp (Windows FUSE layer)           via Scoop nonportable bucket
#   - VirtIO-FS driver (viofs)              from the VirtIO ISO (mounted at sdc)
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
$infPath = "${virtioLetter}viofs\w10\amd64\viofs.inf"

if (-not (Test-Path $infPath)) {
    $infPath = "${virtioLetter}viofs\2k22\amd64\viofs.inf"
}
if (-not (Test-Path $infPath)) {
    die "viofs.inf not found under ${virtioLetter}viofs\. Check the VirtIO ISO version."
}

Write-Host "  Installing driver from: $infPath"
$result = pnputil /add-driver $infPath /install 2>&1
Write-Host "  $result"

# ---------------------------------------------------------------------------
# VirtIO-FS Service — maps mount tag to a drive letter
# ---------------------------------------------------------------------------
Step "Configuring VirtIO-FS Service"

$svc = Get-Service -Name "VirtIO-FS Service" -ErrorAction SilentlyContinue
if (-not $svc) {
    # The service is registered by the driver; trigger device detection.
    Write-Host "  Service not found yet — rescanning devices..."
    pnputil /scan-devices | Out-Null
    Start-Sleep -Seconds 3
    $svc = Get-Service -Name "VirtIO-FS Service" -ErrorAction SilentlyContinue
}

if ($svc) {
    Set-Service -Name "VirtIO-FS Service" -StartupType Automatic
    if ($svc.Status -ne "Running") {
        Start-Service -Name "VirtIO-FS Service"
    }
    Write-Host "  VirtIO-FS Service is running."
} else {
    Write-Host "  WARNING: VirtIO-FS Service not found." -ForegroundColor Yellow
    Write-Host "  The virtiofs device may not be attached yet on the host." -ForegroundColor Yellow
    Write-Host "  On the host, run: .vm/windows/share.sh attach" -ForegroundColor Yellow
    Write-Host "  Then re-run this script." -ForegroundColor Yellow
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

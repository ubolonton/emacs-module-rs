#!/usr/bin/env pwsh
# bootstrap.ps1 - Post-install setup for the Windows dev VM.
#
# Installs:
#   - Scoop (package manager)
#   - Rust (via rustup)
#
# Usage (run from Linux host, after create-vm.sh reports the VM is ready):
#
#   .vm/windows/bootstrap.ps1 --host <vm-ip>
#
# This script SSHs into the VM and runs itself remotely using PowerShell.
# If you're already inside the VM (e.g. via virt-viewer), run it directly:
#
#   powershell -ExecutionPolicy Bypass -File bootstrap.ps1
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
# Scoop
# ---------------------------------------------------------------------------
Step "Installing Scoop"

if (Get-Command scoop -ErrorAction SilentlyContinue) {
    Write-Host "  Scoop already installed: $(scoop --version)"
} else {
    # -RunAsAdmin is required when running as the Administrator account
    & ([scriptblock]::Create((Invoke-RestMethod https://get.scoop.sh))) -RunAsAdmin
    # Reload PATH so 'scoop' is available in this session
    $env:PATH = [System.Environment]::GetEnvironmentVariable("PATH", "Machine") + ";" +
                [System.Environment]::GetEnvironmentVariable("PATH", "User")
    Write-Host "  Scoop installed: $(scoop --version)"
}

# ---------------------------------------------------------------------------
# Rust
# ---------------------------------------------------------------------------
Step "Installing Rust via rustup"

if (Get-Command rustc -ErrorAction SilentlyContinue) {
    Write-Host "  Rust already installed: $(rustc --version)"
} else {
    $rustupUrl = "https://win.rustup.rs/x86_64"
    $rustupExe = "$env:TEMP\rustup-init.exe"
    Write-Host "  Downloading rustup-init..."
    Invoke-WebRequest -Uri $rustupUrl -OutFile $rustupExe

    Write-Host "  Running rustup-init (stable toolchain)..."
    & $rustupExe -y --default-toolchain stable --profile minimal 2>&1

    # Reload PATH
    $env:PATH = [System.Environment]::GetEnvironmentVariable("PATH", "Machine") + ";" +
                [System.Environment]::GetEnvironmentVariable("PATH", "User")

    Write-Host "  Rust installed: $(rustc --version)"
    Write-Host "  Cargo: $(cargo --version)"
}

# ---------------------------------------------------------------------------
# Done
# ---------------------------------------------------------------------------
# Reload PATH one final time to pick up all installed tools
$env:PATH = [System.Environment]::GetEnvironmentVariable("PATH", "Machine") + ";" +
            [System.Environment]::GetEnvironmentVariable("PATH", "User")

Write-Host ""
Write-Host "=================================================================" -ForegroundColor Green
Write-Host " Bootstrap complete." -ForegroundColor Green
Write-Host ""
if (Get-Command rustc -ErrorAction SilentlyContinue) {
    Write-Host "  rustc   $(rustc --version)"
    Write-Host "  cargo   $(cargo --version)"
}
if (Get-Command scoop -ErrorAction SilentlyContinue) {
    Write-Host "  scoop   $((scoop --version) | Select-String 'v\d' | Select-Object -First 1)"
}
Write-Host "=================================================================" -ForegroundColor Green

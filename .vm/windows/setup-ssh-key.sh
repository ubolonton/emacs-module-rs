#!/usr/bin/env bash
# Installs the host's SSH public key into the Windows VM for passwordless login.
#
# Windows OpenSSH stores administrator keys in a system-wide file with strict
# ACLs, not in the user's ~/.ssh/authorized_keys:
#   C:\ProgramData\ssh\administrators_authorized_keys
#
# Run once after the VM is first accessible via SSH (password login).
#
# Usage:
#   ./setup-ssh-key.sh [--pub-key <path>] <vm-ip>
#
# If --pub-key is omitted, defaults to ~/.ssh/id_rsa.pub.
set -euo pipefail

PUB_KEY_FILE="$HOME/.ssh/id_rsa.pub"

while [[ $# -gt 0 ]]; do
    case "$1" in
        --pub-key) PUB_KEY_FILE="$2"; shift 2 ;;
        *) VM_IP="$1"; shift ;;
    esac
done

if [[ -z "${VM_IP:-}" ]]; then
    echo "Usage: $0 [--pub-key <path>] <vm-ip>"
    exit 1
fi

[[ -f "$PUB_KEY_FILE" ]] || { echo "ERROR: Public key not found: $PUB_KEY_FILE"; exit 1; }

PUB_KEY=$(cat "$PUB_KEY_FILE")

echo "Installing public key on Administrator@$VM_IP ..."

ssh "Administrator@$VM_IP" \
    "powershell -ExecutionPolicy Bypass -NonInteractive -" <<EOF
\$dir = 'C:\ProgramData\ssh'
if (-not (Test-Path \$dir)) { New-Item -ItemType Directory -Path \$dir | Out-Null }
Set-Content -Path "\$dir\administrators_authorized_keys" -Value '$PUB_KEY' -Encoding UTF8
icacls "\$dir\administrators_authorized_keys" /inheritance:r /grant 'SYSTEM:(F)' /grant 'Administrators:(F)' | Out-Null
Write-Host 'Done.'
EOF

echo ""
echo "Test with: ssh Administrator@$VM_IP"

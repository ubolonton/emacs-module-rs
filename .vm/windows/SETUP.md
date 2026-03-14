# Windows VM — Dev Environment Setup

Post-install setup steps, run after the VM is accessible via SSH
(see INSTALL.md for the installation workflow).

## Passwordless SSH

Windows OpenSSH stores administrator keys in a system-wide file with strict
ACLs — not in `~/.ssh/authorized_keys`. Run `setup-ssh-key.sh` once (requires
password auth) to install the host's public key:

```bash
VM_IP=$(.vm/windows/get-vm-ip.sh)
.vm/windows/setup-ssh-key.sh "$VM_IP"
# defaults to ~/.ssh/id_rsa.pub; override with --pub-key <path>
```

After this, `ssh Administrator@"$VM_IP"` works without a password prompt
(passphrase prompts for your local key are handled by `ssh-agent`).

## Running bootstrap.ps1

`bootstrap.ps1` installs Scoop and Rust inside the VM. Since the host is Linux
(no PowerShell), copy the script to the VM and run it there:

```bash
VM_IP=$(.vm/windows/get-vm-ip.sh)
scp .vm/windows/bootstrap.ps1 Administrator@"$VM_IP":bootstrap.ps1
ssh Administrator@"$VM_IP" "powershell -ExecutionPolicy Bypass -File bootstrap.ps1"
```

Alternatively, if `pwsh` (PowerShell Core) is installed on the host, the
`-VMHost` flag runs the script remotely without copying:

```bash
.vm/windows/bootstrap.ps1 -VMHost "$VM_IP"
```

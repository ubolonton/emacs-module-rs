#!/usr/bin/env bash
# Downloads ISOs needed for the Windows VM.
# - VirtIO drivers: fetched directly (stable public URL)
# - Windows Server 2022 Evaluation: requires a manual step (Microsoft form)
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ISO_DIR="$SCRIPT_DIR/isos"
mkdir -p "$ISO_DIR"

# ---------------------------------------------------------------------------
# VirtIO drivers (free, maintained by Fedora/Red Hat)
# ---------------------------------------------------------------------------
VIRTIO_URL="https://fedorapeople.org/groups/virt/virtio-win/direct-downloads/stable-virtio/virtio-win.iso"
VIRTIO_ISO="$ISO_DIR/virtio-win.iso"

if [[ -f "$VIRTIO_ISO" ]]; then
    echo "VirtIO ISO already present: $VIRTIO_ISO"
else
    echo "Downloading VirtIO drivers ISO..."
    curl -L --progress-bar -o "$VIRTIO_ISO" "$VIRTIO_URL"
    echo "Done: $VIRTIO_ISO"
fi

# ---------------------------------------------------------------------------
# Windows Server 2022 Evaluation ISO
# Microsoft requires a short registration form to obtain a download link.
# ---------------------------------------------------------------------------
WIN_ISO="$ISO_DIR/windows-server-2022-eval.iso"

if [[ -f "$WIN_ISO" ]]; then
    echo "Windows ISO already present: $WIN_ISO"
else
    echo ""
    echo "================================================================="
    echo " Windows Server 2022 Evaluation ISO"
    echo "================================================================="
    echo ""
    echo " Microsoft requires a brief registration form to get a download link."
    echo " Steps:"
    echo "   1. Open: https://www.microsoft.com/en-us/evalcenter/evaluate-windows-server-2022"
    echo "   2. Click 'Download the ISO' and fill in the form (name + email)."
    echo "   3. Select language and click 'Download'."
    echo "   4. Copy the resulting download URL and paste it below."
    echo ""
    read -rp "Paste the Windows ISO download URL: " WIN_URL

    # https://software-static.download.prss.microsoft.com/sg/download/888969dx5-f34g-4e03-ac9d-1f9786c66749/SERVER_EVAL_x64FRE_en-us.iso
    if [[ -z "$WIN_URL" ]]; then
        echo "No URL provided. Skipping Windows ISO download."
        echo "Re-run this script and paste the URL when ready."
        exit 1
    fi

    echo "Downloading Windows Server 2022 Evaluation ISO..."
    curl -L --progress-bar -o "$WIN_ISO" "$WIN_URL"
    echo "Done: $WIN_ISO"
fi

echo ""
echo "All ISOs are ready in: $ISO_DIR"
ls -lh "$ISO_DIR"

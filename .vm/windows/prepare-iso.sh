#!/usr/bin/env bash
# Injects Autounattend.xml into a copy of the Windows ISO.
#
# How it works:
#   Windows Setup scans only its own boot media for Autounattend.xml, so
#   the file must be in the root of the Windows ISO itself. This script:
#     1. Extracts the original ISO with 7z (UDF-aware)
#     2. Adds Autounattend.xml at the root
#     3. Repacks as a bootable UDF+ISO 9660 ISO using xorriso
#
# Prerequisites:
#   - 7z    (sudo apt install 7zip)
#   - xorriso  (sudo apt install xorriso)
#
# Usage:
#   ./prepare-iso.sh [--password <password>]
#
# Output: isos/windows-server-2022-eval-unattended.iso  (~same size as original)
# Temp:   ~5GB in isos/.tmp-winiso/ during extraction (deleted on completion)
#
# Re-run this whenever Autounattend.xml changes.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ISO_DIR="$SCRIPT_DIR/isos"

WIN_ISO="$ISO_DIR/windows-server-2022-eval.iso"
WIN_ISO_UNATTENDED="$ISO_DIR/windows-server-2022-eval-unattended.iso"
AUTOUNATTEND_XML="$SCRIPT_DIR/Autounattend.xml"
EXTRACT_DIR="$ISO_DIR/.tmp-winiso"

# ---------------------------------------------------------------------------
# Parse arguments
# ---------------------------------------------------------------------------
PASSWORD=""
while [[ $# -gt 0 ]]; do
    case "$1" in
        --password)
            PASSWORD="$2"
            shift 2
            ;;
        *)
            echo "Unknown argument: $1"
            exit 1
            ;;
    esac
done

# ---------------------------------------------------------------------------
# Checks
# ---------------------------------------------------------------------------
for tool in 7z xorriso; do
    if ! command -v "$tool" &>/dev/null; then
        echo "ERROR: '$tool' not found."
        echo "  Debian/Ubuntu: sudo apt install 7zip xorriso"
        exit 1
    fi
done

[[ -f "$WIN_ISO" ]]          || { echo "ERROR: Windows ISO not found: $WIN_ISO"; echo "Run download-isos.sh first."; exit 1; }
[[ -f "$AUTOUNATTEND_XML" ]] || { echo "ERROR: Autounattend.xml not found: $AUTOUNATTEND_XML"; exit 1; }

# ---------------------------------------------------------------------------
# Optionally patch the password
# ---------------------------------------------------------------------------
WORK_XML="$AUTOUNATTEND_XML"
if [[ -n "$PASSWORD" ]]; then
    WORK_XML="$SCRIPT_DIR/autounattend-patched.xml"
    sed "s|<Value>Passw0rd!</Value>|<Value>${PASSWORD}</Value>|g" \
        "$AUTOUNATTEND_XML" > "$WORK_XML"
    echo "Using custom password."
fi

# ---------------------------------------------------------------------------
# Extract the Windows ISO (UDF-aware)
# ---------------------------------------------------------------------------
echo "Extracting Windows ISO (~5GB, takes a minute)..."
rm -rf "$EXTRACT_DIR"
mkdir -p "$EXTRACT_DIR"
# 7z reads UDF filesystems; -bd = no progress bar; -y = yes to all
7z x "$WIN_ISO" -o"$EXTRACT_DIR" -bd -y > /dev/null
echo "Extracted to: $EXTRACT_DIR"

# ---------------------------------------------------------------------------
# Add Autounattend.xml at the root
# ---------------------------------------------------------------------------
cp "$WORK_XML" "$EXTRACT_DIR/Autounattend.xml"
[[ "$WORK_XML" != "$AUTOUNATTEND_XML" ]] && rm -f "$WORK_XML"
echo "Added Autounattend.xml"

# ---------------------------------------------------------------------------
# Repack as bootable UDF+ISO 9660 ISO
# ---------------------------------------------------------------------------
echo "Repacking ISO (~5GB, takes a few minutes)..."
rm -f "$WIN_ISO_UNATTENDED"

xorriso -as mkisofs \
    -iso-level 4 \
    -J -joliet-long \
    -b boot/etfsboot.com \
    -no-emul-boot \
    -boot-load-size 8 \
    -boot-info-table \
    -eltorito-alt-boot \
    -e efi/microsoft/boot/efisys_noprompt.bin \
    -no-emul-boot \
    -V "SSS_X64FREE_EN-US_DV9" \
    -o "$WIN_ISO_UNATTENDED" \
    "$EXTRACT_DIR" 2>&1 | grep -v '^$'

# ---------------------------------------------------------------------------
# Cleanup
# ---------------------------------------------------------------------------
rm -rf "$EXTRACT_DIR"
echo ""
echo "Done: $WIN_ISO_UNATTENDED"
echo "  $(du -sh "$WIN_ISO_UNATTENDED" | cut -f1)"

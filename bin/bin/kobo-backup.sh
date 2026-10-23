#!/usr/bin/env bash
# kobo-backup — snapshot KOReader configuration, NickelMenu config, user
# fonts and book data from a Kobo, over SSH or USB, into
# ~/kobo-backup/YYYY-MM-DD[-N].
#
# Each snapshot is complete, but unchanged files are hard-linked to the
# previous snapshot (rsync --link-dest), so repeat backups cost almost nothing.
#
# Usage:
#   kobo-backup            over SSH to host "kobo" (from ~/.ssh/config), via sshfs
#   kobo-backup HOST       over SSH to another host alias
#   kobo-backup /path      a USB mount point, e.g. /run/media/$USER/KOBOeReader
#
# SSH mode needs sshfs (pacman -S sshfs) and KOReader's SSH server running.

set -euo pipefail

TARGET=${1:-kobo}
if [[ $TARGET == /* ]]; then
	K=$TARGET
else
	command -v sshfs >/dev/null || {
		echo "sshfs not installed (pacman -S sshfs)" >&2
		exit 1
	}
	K=$(mktemp -d "${TMPDIR:-/tmp}/kobo.XXXXXX")
	cleanup() {
		fusermount3 -u "$K" 2>/dev/null || fusermount -u "$K" 2>/dev/null || true
		rmdir "$K" 2>/dev/null || true
	}
	trap cleanup EXIT
	echo "Mounting $TARGET:/mnt/onboard read-only over SSH..."
	sshfs -o ro,reconnect,ServerAliveInterval=15 "$TARGET:/mnt/onboard" "$K"
fi
DEST=${KOBO_BACKUP_DIR:-$HOME/kobo-backup}
KO="$K/.adds/koreader"

[[ -d $KO ]] || {
	echo "No KOReader found at $KO — is the Kobo mounted, or KOReader's SSH server running?" >&2
	exit 1
}

mkdir -p "$DEST"
B="$DEST/$(date +%F)"
n=1
while [[ -e $B ]]; do B="$DEST/$(date +%F)-$((n++))"; done
PREV=$(find "$DEST" -mindepth 1 -maxdepth 1 -type d | sort | tail -n1 || true)
LINK=()
[[ -n $PREV ]] && LINK=(--link-dest="$PREV")
mkdir -p "$B"

echo "Backing up to $B${PREV:+ (unchanged files linked to $(basename "$PREV"))}"

# KOReader: user settings, style tweaks, patches, all plugins (their settings
# live in settings/ and settings.reader.lua), statistics, vocabulary, history.
rsync -a "${LINK[@]/%//koreader}" --ignore-missing-args \
	--include='/settings.reader.lua' --include='/defaults.custom.lua' \
	--include='/history.lua' \
	--include='/styletweaks/***' --include='/settings/***' \
	--include='/patches/***' --include='/plugins/***' \
	--exclude='*' \
	"$KO/" "$B/koreader/"

# Your own fonts (KOReader's bundled fonts are deliberately not included).
rsync -a "${LINK[@]/%//fonts}" "$K/fonts/" "$B/fonts/"

# NickelMenu config (menu entries, e.g. the KOReader launcher).
rsync -a "${LINK[@]/%//nm}" --ignore-missing-args "$K/.adds/nm/" "$B/nm/"

# Per-book data: highlights, notes, reading position (*.sdr folders).
rsync -a "${LINK[@]/%//book-data}" --prune-empty-dirs \
	--exclude='/.adds/' --exclude='/.kobo/' \
	--include='*/' --include='*.sdr/***' --exclude='*' \
	"$K/" "$B/book-data/"

du -sh "$B" | sed 's/^/Size on disk (before linking): /'
echo "Done."

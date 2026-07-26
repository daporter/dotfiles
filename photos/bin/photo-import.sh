#!/usr/bin/env bash
#
# photo-import.sh — copy new photos straight off a USB-connected iPhone into
# the ingest inbox, skipping anything the archive already holds.
#
# Image Capture keeps no reliable record of what it has already sent, so it
# re-copies the whole camera roll every time — hours of Wi-Fi for a couple of
# hundred genuinely new photos. Mounting the phone locally lets us stat each
# file and only read the ones we don't have, so an import moves megabytes
# instead of the entire library.
#
# The prefilter keys on basename + size, the same heuristic rsync uses by
# default. Correctness still rests with photo-ingest.sh's SHA-256 dedup; this
# only decides what is worth transferring. Caveat: two genuinely different
# photos sharing both a filename and an exact byte size would be skipped and
# never imported. mtime is deliberately not part of the key — files already in
# the archive arrived via Samba, so their mtimes reflect import time rather
# than anything stable on the phone.
#
# Usage: photo-import.sh <person>       (DRY_RUN=1 to print decisions only)
#
set -o errexit
set -o nounset
set -o pipefail

MEDIA="/mnt/media"
ARCHIVE="$MEDIA/photos"
INBOX="$ARCHIVE/inbox"
LOG="$ARCHIVE/photo-ingest.log"

log() { printf '[%s] %s\n' "$(date '+%Y-%m-%d %H:%M:%S')" "$*" | tee -a "$LOG"; }

person=${1:-}
if [ -z "$person" ]; then
    echo "Usage: $0 <person>" >&2
    exit 1
fi

dest="$INBOX/$person"
archive_dir="$ARCHIVE/$person"

if ! idevice_id -l 2>/dev/null | grep -q .; then
    echo "No iPhone detected. Plug it in and unlock it." >&2
    exit 1
fi

if ! idevicepair validate >/dev/null 2>&1; then
    echo "iPhone not paired. Unlock it, then run: idevicepair pair" >&2
    exit 1
fi

mkdir -p "$dest"

# Unmount the phone and drop any half-copied file on every exit path.
MOUNT=$(mktemp -d)
mounted=0
partial=""
cleanup() {
    [ -n "$partial" ] && rm -f "$partial"
    [ "$mounted" -eq 1 ] && fusermount -u "$MOUNT" 2>/dev/null
    rmdir "$MOUNT" 2>/dev/null
    return 0
}
trap cleanup EXIT

ifuse "$MOUNT"
mounted=1

# Index what we already have, by name and size. Derived fresh from the archive
# (plus anything still queued in the inbox) so there is no separate state file
# to drift out of sync; a file deleted from the archive is simply re-imported.
declare -A known
index_dirs=()
[ -d "$archive_dir" ] && index_dirs+=("$archive_dir")
[ -d "$dest" ] && index_dirs+=("$dest")
if [ ${#index_dirs[@]} -gt 0 ]; then
    while IFS=$'\t' read -r name size; do
        known["$name/$size"]=1
        # Also register the un-suffixed name for files renamed on collision,
        # here or by photo-ingest.sh. Without this they never match their
        # phone-side original and are re-copied on every single run.
        if [[ $name =~ ^(.+)-[0-9]+(\.[^.]+)$ ]]; then
            known["${BASH_REMATCH[1]}${BASH_REMATCH[2]}/$size"]=1
        fi
    done < <(find "${index_dirs[@]}" -type f -printf '%f\t%s\n' 2>/dev/null)
fi
log "$person: indexed ${#known[@]} known files; scanning phone…"

copied=0 skipped=0
while IFS= read -r -d '' f; do
    base=$(basename "$f")
    case "$base" in .DS_Store | ._*) continue ;; esac

    size=$(stat -c '%s' "$f" 2>/dev/null) || continue
    if [ -n "${known["$base/$size"]:-}" ]; then
        skipped=$((skipped + 1))
        continue
    fi

    # Collision-safe within the inbox: DCIM subfolders (100APPLE, 101APPLE, …)
    # can recycle a name, and earlier imports may still be queued here.
    name=${base%.*}
    ext=${base##*.}
    target="$dest/$base"
    n=1
    while [ -e "$target" ]; do
        target="$dest/${name}-$n.$ext"
        n=$((n + 1))
    done

    if [ -n "${DRY_RUN:-}" ]; then
        printf 'would copy: %s (%s bytes)\n' "$base" "$size"
    else
        partial="$target"
        cp -p "$f" "$target"
        partial=""
        # Index it so a recycled name later in this same run does not collide.
        known["$base/$size"]=1
    fi
    copied=$((copied + 1))
done < <(find "$MOUNT/DCIM" -type f -print0 2>/dev/null)

if [ -n "${DRY_RUN:-}" ]; then
    log "$person: would copy=$copied skipped=$skipped (dry run)"
else
    log "$person: copied=$copied skipped=$skipped"
fi

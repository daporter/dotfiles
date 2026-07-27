#!/usr/bin/env bash
#
# photo-ingest.sh — sort new phone photos from per-person inboxes into the
# archive, de-duplicating by content hash.
#
# Idempotent by design: because de-duplication is by SHA-256, re-importing the
# whole camera roll each time only ever adds genuinely new files, and recycled
# iPhone names (IMG_0001 coming round again) never collide destructively.
#
# Layout (inbox lives under photos/ so it is reachable via the [Photos] Samba
# share — that share is the only network-writable path into the media tree):
#   /mnt/media/photos/inbox/<person>/   <- Image Capture imports land here (via Samba)
#   /mnt/media/photos/<person>/YYYY/YYYY-MM/   <- sorted archive
#   /mnt/media/photos/.<person>.sha256         <- known-hash manifest
#
set -o errexit
set -o nounset
set -o pipefail

MEDIA="/mnt/media"
ARCHIVE="$MEDIA/photos"
INBOX="$ARCHIVE/inbox"
LOG="$ARCHIVE/photo-ingest.log"

log() { printf '[%s] %s\n' "$(date '+%Y-%m-%d %H:%M:%S')" "$*" >>"$LOG"; }

shopt -s nullglob dotglob

[ -d "$INBOX" ] || exit 0

# Defer if an import may still be in progress (any inbox file touched < 60s ago).
if find "$INBOX" -type f -mmin -1 2>/dev/null | grep -q .; then
    log "recent inbox activity; deferring to next run"
    exit 0
fi

for person_dir in "$INBOX"/*/; do
    person=$(basename "$person_dir")
    dest_root="$ARCHIVE/$person"
    manifest="$ARCHIVE/.${person}.sha256"
    mkdir -p "$dest_root"
    touch "$manifest"

    # One-time bootstrap: if the manifest is empty but the archive already holds
    # files (e.g. the initial rsync of your photos), hash them first so they are
    # not re-imported as duplicates.
    if [ ! -s "$manifest" ] && [ -n "$(find "$dest_root" -type f -print -quit)" ]; then
        log "$person: bootstrapping manifest from existing archive (one-off)…"
        find "$dest_root" -type f -print0 \
            | xargs -0 sha256sum \
            | sed "s#  $MEDIA/#  #" >"$manifest"
    fi

    added=0 dupes=0 undated=0 empty=0
    while IFS= read -r -d '' f; do
        base=$(basename "$f")
        # Drop macOS metadata cruft that Samba copies produce.
        case "$base" in .DS_Store|._*) rm -f "$f"; continue ;; esac

        # A zero-byte file is a truncated copy with nothing to archive, and
        # exiftool errors on it, which used to abort the entire run.
        if [ ! -s "$f" ]; then
            rm -f "$f"
            empty=$((empty + 1))
            continue
        fi

        hash=$(sha256sum "$f" | cut -d' ' -f1)
        if grep -q "^$hash " "$manifest"; then
            rm -f "$f"
            dupes=$((dupes + 1))
            continue
        fi

        # Destination folder from capture date. exiftool prints the first of
        # these tags that exists, so ordering gives the fallback chain:
        # DateTimeOriginal (photos) -> CreateDate (videos) -> atime -> mtime.
        #
        # atime sits ahead of mtime because Image Capture rewrites mtime to the
        # import time, which would file every EXIF-less photo under the month it
        # was copied. Samba preserves the original date in atime instead, and
        # /mnt/media is mounted noatime so it stays put. EXIF still wins
        # wherever it exists, so this only affects files that would otherwise
        # fall through to a meaningless timestamp.
        #
        # `|| true` because exiftool exits non-zero on files it dislikes, and
        # under errexit+pipefail that would kill the run mid-inbox.
        sub=$(exiftool -m -d '%Y/%Y-%m' -s3 \
                -DateTimeOriginal -CreateDate -FileAccessDate -FileModifyDate "$f" 2>/dev/null \
                | head -n1) || true
        # Anything that is not YYYY/YYYY-MM is unusable: a zeroed EXIF date
        # otherwise becomes a literal "0000:00:00 00:00:00" directory.
        if ! [[ $sub =~ ^[0-9]{4}/[0-9]{4}-[0-9]{2}$ ]]; then
            sub="undated"
            undated=$((undated + 1))
        fi

        target="$dest_root/$sub"
        mkdir -p "$target"

        # Collision-safe: same filename, different content -> append -1, -2, …
        name=${base%.*}
        ext=${base##*.}
        dest="$target/$base"
        n=1
        while [ -e "$dest" ]; do
            dest="$target/${name}-$n.$ext"
            n=$((n + 1))
        done

        mv "$f" "$dest"
        printf '%s  %s\n' "$hash" "${dest#"$MEDIA"/}" >>"$manifest"
        added=$((added + 1))
    done < <(find "$person_dir" -type f -print0)

    # Tidy empty subdirectories left in the inbox.
    find "$person_dir" -mindepth 1 -type d -empty -delete 2>/dev/null || true

    # Only log runs that did something. The timer fires every 15 minutes and
    # almost always finds an empty inbox, which would bury the real entries.
    # systemd's journal is the heartbeat: it records every invocation and its
    # exit status, including failures too early for this script to log.
    if [ $((added + dupes + undated + empty)) -gt 0 ]; then
        log "$person: added=$added duplicates=$dupes undated=$undated empty=$empty"
    fi
done

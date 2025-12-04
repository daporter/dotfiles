#!/bin/bash
#
# Wrapper script for daily and offsite restic backups
# Usage: ./backup_wrapper.sh daily
#        ./backup_wrapper.sh offsite
#        ./backup_wrapper.sh all

set -o errexit
set -o nounset
set -o pipefail

# --- Configuration ---

EMAIL_TO="david@daporter.net"
EMAIL_SUBJECT="Restic Backup Summary - $(date '+%Y-%m-%d')"

# Daily backup
DAILY_MOUNT="/mnt/restic"
DAILY_LOG="$HOME/backup_daily.log"
DAILY_SOURCE=$HOME

# Offsite backup
OFFSITE_MOUNT="/mnt/restic_offsite"
OFFSITE_LOG="$HOME/backup_offsite.log"
OFFSITE_SOURCE=$DAILY_SOURCE

# --- Functions ---

TMP_LOG=$(mktemp)

log() {
    local MSG="$*"
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $MSG" | \
        tee -a "$LOGFILE" "$TMP_LOG"
}

log_fail() {
    local MSG="$*"
    # Use red ANSI colour for failures
    echo -e "[$(date '+%Y-%m-%d %H:%M:%S')] \e[31m[FAIL] $MSG\e[0m" | \
        tee -a "$LOGFILE" "$TMP_LOG"
}

fail() {
    local MSG="$*"
    log_fail "$MSG"
    SUBJECT="$EMAIL_SUBJECT - $MOUNTPOINT (FAIL)"
    sed 's/\x1b\[[0-9;]*m//g' "$TMP_LOG" | mail -s "$SUBJECT" "$EMAIL_TO"
    exit 1
}

human_readable() {
    local BYTES=$1
    if [ "$BYTES" -lt 1024 ]; then
        echo "${BYTES} B"
    elif [ "$BYTES" -lt $((1024**2)) ]; then
        echo "$((BYTES / 1024)) KB"
    elif [ "$BYTES" -lt $((1024**3)) ]; then
        echo "$((BYTES / 1024 / 1024)) MB"
    else
        echo "$((BYTES / 1024 / 1024 / 1024)) GB"
    fi
}

get_repo_stats() {
    local SNAPSHOTS=0 SIZE=0
    SNAPSHOTS=$(restic -r "$MOUNTPOINT" snapshots --quiet 2>/dev/null | wc -l)
    SIZE=$(restic -r "$MOUNTPOINT" stats --mode raw-data --json 2>/dev/null | jq -r '.total_size // 0')
    echo "$SNAPSHOTS $SIZE"
}

backup_drive() {
    MOUNTPOINT=$1
    SOURCE=$2
    LOGFILE=$3
    PRUNE_ARGS=$4                           # optional restic forget policy

    # --- Log rotation for daily backup ---
    if [[ "$LOGFILE" == *daily.log ]]; then
        MONTH=$(date '+%Y-%m')
        if [ -f "$LOGFILE" ]; then
            LAST_MOD=$(date -r "$LOGFILE" '+%Y-%m')
            if [ "$LAST_MOD" != "$MONTH" ]; then
                mv "$LOGFILE" "${LOGFILE%.log}_${LAST_MOD}.log"
            fi
        fi
    fi

    STATUS="SUCCESS"

    log "=== Starting backup on $MOUNTPOINT ==="

    # Check repository is mounted
    if ! mountpoint -q "$MOUNTPOINT"; then
        log "Repository not mounted; attempting to mount…"
        if ! mount "$MOUNTPOINT"; then
            fail "Failed to mount repository; aborting."
        fi
    fi

    # Initialize repository if empty
    if [ -z "$(ls -A "$MOUNTPOINT")" ]; then
        log "Initializing restic repository..."
        if ! restic -r "$MOUNTPOINT" init 2>&1 | \
                tee -a "$LOGFILE" "$TMP_LOG"; then
            fail "Repository initialization failed; aborting."
        fi
    fi

    # Stats before backup
    read -r SNAP_BEFORE SIZE_BEFORE <<< "$(get_repo_stats)"
    SNAP_BEFORE=${SNAP_BEFORE:-0}
    SIZE_BEFORE=${SIZE_BEFORE:-0}
    log "Repository before backup: $SNAP_BEFORE snapshots, $(human_readable "$SIZE_BEFORE")"

    # Run backup
    log "Running backup..."
    if ! restic -r "$MOUNTPOINT" backup \
         --exclude-file="$HOME/.config/restic/restic-exclude.txt"  \
         "$SOURCE" 2>&1 | \
            tee -a "$LOGFILE"; then
        fail "Backup failed; aborting."
    else
        log "Backup completed successfully."
    fi

    # Prune old snapshots if policy provided
    if [ -n "$PRUNE_ARGS" ]; then
        log "Pruning old snapshots..."
        # Deliberately use $PRUNE_ARGS unquoted since it holds separate
        # arguments.
        # shellcheck disable=SC2086
        if ! restic -r "$MOUNTPOINT" forget $PRUNE_ARGS --prune 2>&1 | \
                tee -a "$LOGFILE" "$TMP_LOG"; then
            log_fail "Prune failed"
        else
            log "Prune completed successfully."
        fi
    fi

    # Stats after backup
    read -r SNAP_AFTER SIZE_AFTER <<< "$(get_repo_stats)"
    SNAP_AFTER=${SNAP_AFTER:-0}
    SIZE_AFTER=${SIZE_AFTER:-0}
    log "Repository after backup: $SNAP_AFTER snapshots, $(human_readable "$SIZE_AFTER")"

    # Data added
    ADDED=$((SIZE_AFTER - SIZE_BEFORE))
    log "Data added during this backup: $(human_readable "$ADDED")"

    # Verify repository
    log "Verifying repository..."
    if ! restic -r "$MOUNTPOINT" check --read-data-subset=1% 2>&1 | \
            tee -a "$LOGFILE" "$TMP_LOG"; then
        log_fail "Repository verification failed."
    else
        log "Repository verification succeeded."
    fi

    # Final summary
    log "=== Final repository summary ==="
    log "Total snapshots: $SNAP_AFTER"
    log "Total repository size: $(human_readable "$SIZE_AFTER")"
    log "Backup status: $STATUS"
    log "====================================="

    # Send email without ANSI color codes
    if command -v mail >/dev/null 2>&1; then
        SUBJECT="$EMAIL_SUBJECT - $MOUNTPOINT ($STATUS)"
        # Strip ANSI escape codes for email
        sed 's/\x1b\[[0-9;]*m//g' "$TMP_LOG" | mail -s "$SUBJECT" "$EMAIL_TO"
    fi

    # Clean up temporary log
    rm -f "$TMP_LOG"
}

unmount_repo() {
    # Only unmount if it's actually mounted.
    if mountpoint -q "$MOUNTPOINT"; then
        log "Unmounting $MOUNTPOINT…"
        if ! umount "$MOUNTPOINT"; then
            log_fail "ERROR: Failed to unmount $MOUNTPOINT"
        fi
        log "Unmounted successfully."
    else
        log "Note: $MOUNTPOINT not mounted; skipping unmount."
    fi
}

backup_daily() {
    backup_drive "$DAILY_MOUNT" "$DAILY_SOURCE" "$DAILY_LOG" "--keep-daily 7 --keep-weekly 5 --keep-monthly 12 --keep-yearly 100"
}

backup_offsite() {
    backup_drive "$OFFSITE_MOUNT" "$OFFSITE_SOURCE" "$OFFSITE_LOG" "--keep-monthly 12 --keep-yearly 100"
    unmount_repo
}

# --- Main ---

case "$1" in
    daily)
        backup_daily
        ;;
    offsite)
        backup_offsite
        ;;
    all)
        backup_daily
        backup_offsite
        ;;
    *)
        echo "Usage: $0 {daily|offsite|all}"
        exit 1
        ;;
esac

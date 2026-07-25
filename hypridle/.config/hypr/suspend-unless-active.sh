#!/bin/sh
# Called by hypridle when the idle-suspend timeout elapses (hypridle.conf).
# hypridle only tracks local Wayland idle, so it would suspend the box out from
# under a remote user. Skip suspend while beilen is in use remotely:
#   1. an SSH login session, or
#   2. an in-progress Samba transfer (files currently open).
# smbstatus is root-only, so it is invoked via a scoped `doas nopass` rule
# (system/doas). Set DRY_RUN=1 to print the decision instead of suspending.

skip() { echo "suspend-unless-active: skipping ($1)" >&2; exit 0; }

# 1. Any remote (SSH) login session.
for s in $(loginctl list-sessions --no-legend --no-pager | awk '{print $1}'); do
    [ "$(loginctl show-session "$s" -p Remote --value)" = "yes" ] \
        && skip "remote session $s"
done

# 2. Any open Samba files (active transfer). Fail safe on query error.
if json=$(doas -n /usr/bin/smbstatus --json 2>/dev/null); then
    n=$(printf '%s' "$json" | jq '[.open_files // {} | length] | add // 0')
    [ "${n:-0}" -gt 0 ] 2>/dev/null && skip "samba open files: $n"
else
    skip "smbstatus query failed (assuming active)"
fi

if [ -n "$DRY_RUN" ]; then
    echo "suspend-unless-active: would suspend" >&2
else
    exec systemctl suspend
fi

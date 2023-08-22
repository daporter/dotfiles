#!/bin/sh

RESTIC_REPOSITORY=/mnt/backup
export RESTIC_REPOSITORY

RESTIC_PASSWORD_COMMAND="gpg -q --for-your-eyes-only --no-tty -d $HOME/.config/restic/restic.gpg"
export RESTIC_PASSWORD_COMMAND

# Systemd doesn’t inherit environment variables, so we must make them available
# explicitly.
systemctl --user import-environment RESTIC_REPOSITORY RESTIC_PASSWORD_COMMAND

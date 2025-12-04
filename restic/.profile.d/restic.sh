#!/bin/sh

RESTIC_REPOSITORY=/mnt/restic
export RESTIC_REPOSITORY

RESTIC_PASSWORD_FILE=$HOME/.config/restic/password
export RESTIC_PASSWORD_FILE


# Systemd doesn’t inherit environment variables, so we must make them available
# explicitly.
systemctl --user import-environment RESTIC_REPOSITORY RESTIC_PASSWORD_FILE

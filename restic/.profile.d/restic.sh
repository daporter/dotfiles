#!/bin/sh

RESTIC_REPOSITORY=rest:http://beilen:8000/
export RESTIC_REPOSITORY

RESTIC_PASSWORD_COMMAND="gpg -q --for-your-eyes-only --no-tty -d $HOME/.config/restic/restic.gpg"
export RESTIC_PASSWORD_COMMAND

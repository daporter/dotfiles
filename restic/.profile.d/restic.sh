#!/bin/sh

RESTIC_REPOSITORY=rest:http://beilen:8000/
export RESTIC_REPOSITORY

RESTIC_PASSWORD_COMMAND="op read op://Personal/restic/password"
export RESTIC_PASSWORD_COMMAND

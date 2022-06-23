#!/bin/sh
#
# Start agent for GPG and SSH keys.

eval "$(keychain --agents gpg,ssh --eval)"

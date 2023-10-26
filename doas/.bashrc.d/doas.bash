#!/bin/bash
#
# Configure Bash for doas.

# The following assumes ‘bash-completion’ is installed.
complete -F _command doas

# Enable a smooth transition from sudo to doas.
alias sudo=doas

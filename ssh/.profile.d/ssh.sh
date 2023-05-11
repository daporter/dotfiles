#!/bin/sh

# Use the 1Password SSH agent.
SSH_AUTH_SOCK=~/.1password/agent.sock
export SSH_AUTH_SOCK

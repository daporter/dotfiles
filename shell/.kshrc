#!/bin/sh

# This file is automatically executed by Korn interactive shells (when
# ENV is set to point to it).

# General Settings
# ================

# Source the global configuration.
# shellcheck source=/dev/null
. /etc/ksh.kshrc

# Set necessary environment variables for the session.
. "$HOME/.environment"

MAILPATH=/var/mail/david:$HOME/mail/personal/inbox:$HOME/mail/migadu/inbox
export MAILPATH

HISTFILE=$HOME/.sh_history
HISTSIZE=5000

# Simple prompt
if [ -n "$SSH_CONNECTION" ]; then
	PS1="$(tput bold)\\u@\\h:\\w [\\j] \$$(tput sgr0) "
else
	PS1="$(tput bold)\\w [\\j] \$$(tput sgr0) "
fi

# Enable emacs-like comand-line editing.
set -o emacs

# Functions
# =========

# Enable a single "e" to spawn my preferred editor.
e() {
	"$EDITOR" "$@"
}

# Show which shell commands I use most frequently.
frequent_cmds() {
	pattern="$1"
	if [ -z "$pattern" ]; then
		history | cut -c 8- | sort -b | less
	else
		history | cut -c 8- | grep "$pattern" | sort -b | less
	fi
}

# Aliases
# =======

alias ls='ls -F'

# List all filenames, including any hidden files except "." and "..".
alias la='ls -A'
alias ll='ls -Al'

# cd into parent directories by omitting `cd`.
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

# Set some options for `less` to make it a bit more convenient to use.
less_opts='--clear-screen --quit-if-one-screen --quit-at-eof --ignore-case'
less_opts="$less_opts --status-column --LONG-PROMPT --RAW-CONTROL-CHARS"
less_opts="$less_opts --HILITE-UNREAD --no-init"
# shellcheck disable=SC2139
alias less="less $less_opts"

# The default pager.  This is used by programs such as git.
PAGER="less $less_opts"
export PAGER

# Enable verbose output for common utilities.
alias cp='cp -v'
alias mv='mv -v'
alias rm='rm -v'

# General aliases.
alias m='neomutt'
alias ms='mbsync -a'
alias n='newsboat'

# Git commands.
if [ $(command -v git) ]; then
	alias gs='git status'
	alias gadd='git add -v'
	alias gaddp='git add --patch'
	alias gaddi='git add --interactive'
	alias gcom='git commit'
	alias gl='git log'
	alias glo='git log --oneline'
	alias gd='git diff'
	alias gds='git diff --stat --summary'
	alias gco='git checkout'
	alias gcob='git checkout -b'
	alias gbl='git branch --list'
	alias gpull='git pull'
	alias gf='git fetch'
	alias gpm='git push -u origin master'
fi

# GnuPG configuration.
GPG_TTY=$(tty)
export GPG_TTY

# Pash (a password manager) configuration
PASH_KEYID=$EMAIL
export PASH_KEYID
PASH_LENGTH=30
export PASH_LENGTH

# Load all supplementary scripts in ~/.profile.d
for sh in $HOME/.profile.d/*.sh; do
	# shellcheck source=/dev/null
	[ -e "$sh" ] && . "$sh"
done
unset -v sh

#!/bin/sh

# Korn Shell Initialisation
# ==============================================================================

# ............................................................. General settings

# shellcheck source=/dev/null
. /etc/ksh.kshrc

. "$HOME/.environment"

HISTFILE=$HOME/.sh_history
HISTSIZE=5000

# Enable emacs-like comand-line editing
set -o emacs

# ....................................................................... Prompt

# As well as the hostname and working directory, this prompt also displays the
# number of background jobs if any are running, and the exit code of the last
# process if it failed.

red=$(tput setaf 1 1 1)
cyan=$(tput setaf 6 6 6)
bold=$(tput bold)
reset=$(tput sgr0)

_jobs() {
	njobs=$(jobs | wc -l | sed 's/ *//')
	if [ "$njobs" != "0" ]; then
		print "${njobs}& "
	else
		print ""
	fi
}

_exit_code() {
	code=$?
	if [ "$code" != "0" ]; then
		print "${red}${code}?${reset} "
	else
		print ""
	fi
}

# Note that if the PS1 value is not wrapped in single quotes then the
# functions within it will be evaluated only once (when the file is sourced)
# rather than on each display of the prompt.
PS1='$bold\\h$reset \\w $(_jobs)$(_exit_code)${cyan}\\$${reset} '

# .................................................................... Functions

# Enable a single "e" to spawn my preferred editor
e() {
	"$EDITOR" "$@"
}

# Show which shell commands I use most frequently
frequent_cmds() {
	pattern="$1"
	if [ -z "$pattern" ]; then
		history | cut -c 8- | sort -b | less
	else
		history | cut -c 8- | grep "$pattern" | sort -b | less
	fi
}

# ...................................................................... Aliases

alias ls='ls -F'

# List all filenames, including any hidden files except "." and ".."
alias la='ls -A'
alias ll='ls -Al'

# cd into parent directories by omitting `cd`.
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

# Set some options for `less` to make it a bit more convenient to use.
less_opts='--clear-screen --quit-if-one-screen --quit-at-eof --ignore-case'
less_opts="$less_opts --LONG-PROMPT --RAW-CONTROL-CHARS --HILITE-UNREAD"
less_opts="$less_opts --no-init"
# shellcheck disable=SC2139
alias less="less $less_opts"

# The default pager used by programs such as git.
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
if [ "$(command -v git)" ]; then
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

# ......................................................................... Misc

# GnuPG configuration.
GPG_TTY=$(tty)
export GPG_TTY

# Pash password manager configuration
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

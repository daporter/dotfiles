#!/bin/sh
#
# If this file is pointed to by $ENV, it is sourced by each invocation
# of interactive ksh.  It is often used for function and alias
# definitions, and setting ksh options.

if [ -r /etc/ksh.kshrc ]; then
	. /etc/ksh.kshrc
fi

# Note: If HISTFILE isn't set, no history file is used.
HISTFILE=$HOME/.sh_history

# Increase number of commands saved to history list.
HISTSIZE=8192

# Avoid duplicate entries
HISTCONTROL="erasedups:ignoreboth"

if [ -d "$HOME"/.rc.d ]; then
	for sh in "$HOME"/.rc.d/*.sh; do
		. "$sh"
	done
	unset sh
fi

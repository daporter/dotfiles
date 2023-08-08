#!/bin/sh
#
# This file is sourced once by the login shell.
#
# All settings that you want to apply to all your environment should be in this
# file.

# Future interactive shell invocations will process any file pointed to by $ENV.
if [ -r "$HOME"/.kshrc ]; then
	ENV=$HOME/.kshrc
	export ENV
fi

LANG=en_AU.UTF-8
export LANG

PATH=$HOME/bin:$HOME/.local/bin:$PATH
export PATH

EDITOR=emacsclient
export EDITOR

VISUAL=$EDITOR
export VISUAL

EMAIL=david@daporter.net
export EMAIL

MAIL=/var/mail/david
export MAIL

MAILPATH=$MAIL:$HOME/Mail/inbox
export MAILPATH

# Some programs (such as df, du, and ls) display sizes in “blocks”.  You
# can adjust the block size and method of display to make sizes easier to
# read.  We'll use a block size of 1kb.
BLOCKSIZE=K
export BLOCKSIZE

# Set the style of time display used in `ls’.
TIME_STYLE=long-iso
export TIME_STYLE

# The `par` paragraph formatter.
#
# From the par man page:
#
#   par is necessarily complex. For those who wish to use it immediately
#   and understand it later, assign to the PARINIT environment variable
#   the following value:
PARINIT='78 rTbgqR B=.,?_A_a Q=_s>|'
export PARINIT

if [ -d "$HOME"/.profile.d ]; then
	for sh in "$HOME"/.profile.d/*.sh; do
		if [ -r "$sh" ]; then
			. "$sh"
		fi
	done
	unset sh
fi

# Systemd doesn't inherit environment variables, so we must make them available
# explicitly.
systemctl --user import-environment PATH

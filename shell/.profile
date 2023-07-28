#!/bin/sh
#
# This file is sourced once by the login shell.

# Future interactive shell invocations will process any file pointed to by $ENV.
if [ -r "$HOME"/.kshrc ]; then
	ENV=$HOME/.kshrc
	export ENV
fi

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

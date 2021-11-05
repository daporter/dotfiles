#!/bin/sh
#
# Environment variable settings intended to be shared by all shells.

PATH=$HOME/bin:$PATH
export PATH

LANG=en_AU.UTF-8
export LANG

EDITOR="emacsclient --tty --alternate-editor=vim"
export EDITOR

VISUAL="emacsclient --create-frame --alternate-editor=vim"
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

# The `par` paragraph formatter.
#
# From the par man page:
#
#   par is necessarily complex. For those who wish to use it immediately
#   and understand it later, assign to the PARINIT environment variable
#   the following value:
PARINIT='78 rTbgqR B=.,?_A_a Q=_s>|'
export PARINIT

# Set the style of time display used in `ls’.
TIME_STYLE=long-iso
export TIME_STYLE

for sh in "$HOME"/.profile.d/*.sh ; do
    # shellcheck source=/dev/null
    [[ -e $sh ]] && . "$sh"
done
unset -v sh

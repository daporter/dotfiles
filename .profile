#!/bin/sh
#
# From https://superuser.com/questions/183870/difference-between-bashrc-and-bash-profile?answertab=active#tab-top
#
# ~/.profile is the place to put stuff that applies to your whole session,
# such as programs that you want to start when you log in (but not graphical
# programs, they go into a different file), and environment variable
# definitions.

PATH="$HOME/bin:$HOME/.local/bin:/usr/local/sbin:$PATH"
export PATH

LC_ALL=en_AU.UTF-8
export LC_ALL
LANG=en_AU.UTF-8
export LANG

MAILPATH="$HOME/mail/personal/inbox:$HOME/mail/data61/inbox"
export MAILPATH

EMAIL=david.a.porter@gmail.com
export EMAIL

EDITOR=vim
export EDITOR

# Load all supplementary scripts in ~/.profile.d
for sh in "$HOME"/.profile.d/*.sh; do
	# shellcheck source=/dev/null
	[ -e "$sh" ] && . "$sh"
done
unset -v sh

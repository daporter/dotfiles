#!/bin/sh
#
# From https://superuser.com/questions/183870/difference-between-bashrc-and-bash-profile?answertab=active#tab-top
#
# ~/.profile is the place to put stuff that applies to your whole session,
# such as programs that you want to start when you log in (but not graphical
# programs, they go into a different file), and environment variable
# definitions.

export PATH="/usr/local/sbin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/bin:$PATH"

export LC_ALL=en_AU.UTF-8  
export LANG=en_AU.UTF-8
export EDITOR=vise
export MAILPATH="$HOME/mail/personal/inbox:$HOME/mail/data61/inbox"
export EMAIL=david.a.porter@gmail.com

# Load all supplementary scripts in ~/.profile.d
for sh in "$HOME"/.profile.d/*.sh 
do
    # shellcheck source=/dev/null
    [ -e "$sh" ] && . "$sh"
done
unset -v sh

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"

# Load RVM into a shell session *as a function*
# shellcheck source=/dev/null
[ -s "$HOME/.rvm/scripts/rvm" ] && . "$HOME/.rvm/scripts/rvm" 

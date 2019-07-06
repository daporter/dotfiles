#!/usr/bin/env sh

# From the coreutils Homebrew package:
#
# Commands also provided by MacOS have been installed with the prefix "g".
# If you need to use these commands with their normal names, you can add a
# "gnubin" directory to your PATH from your bashrc like:
PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"
MANPATH="/usr/local/opt/coreutils/share/man:$MANPATH"

PATH="/usr/local/opt/findutils/libexec/gnubin:$PATH"
MANPATH="/usr/local/opt/findutils/share/man:$MANPATH"

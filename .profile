# From https://superuser.com/questions/183870/difference-between-bashrc-and-bash-profile?answertab=active#tab-top
#
# ~/.profile is the place to put stuff that applies to your whole session,
# such as programs that you want to start when you log in (but not graphical
# programs, they go into a different file), and environment variable
# definitions.

export PATH="/usr/local/sbin:$PATH"

# GnuPG configuration.
GPG_TTY=$(tty)
export GPG_TTY

# MacTEX
export PATH="$PATH:/usr/local/texlive/2017/bin/x86_64-darwin/"

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

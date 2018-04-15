# From https://superuser.com/questions/183870/difference-between-bashrc-and-bash-profile?answertab=active#tab-top
#
# ~/.bashrc is the place to put stuff that applies only to bash itself, such
# as alias and function definitions, shell options, and prompt settings. (You
# could also put key bindings there, but for bash they normally go into
# ~/.inputrc.)
#
# Even better information here:
# https://blog.flowblok.id.au/2013-02/shell-startup-scripts.html
#
# From the Bash documentation:
#   For almost every purpose, shell functions are preferred over aliases.

shopt -s cdspell        # Correct small errors in directory names given to the `cd` builtin
shopt -s histappend     # Append history to $HISTFILE rather than overwriting it

# Load Bash-specific startup files
for sh in "$HOME"/.bashrc.d/*.bash ; do
    [[ -e $sh ]] && source "$sh"
done
unset -v sh

# From Homebrew bash-completion:
[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion

#!/usr/bin/env bash
#
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

# Correct small errors in directory names given to the `cd` builtin
shopt -s cdspell        

# Correct spelling errors during tab-completion
shopt -s dirspell 2> /dev/null

# Prepend cd to directory names automatically
shopt -s autocd 2> /dev/null

# Update window size after every command
shopt -s checkwinsize

# Enable history expansion with space. E.g. typing !!<space> will replace the !!
# with your last command
bind Space:magic-space

# Turn on recursive globbing (enables ** to recurse all directories)
shopt -s globstar 2> /dev/null

# Case-insensitive globbing (used in pathname expansion)
shopt -s nocaseglob;

# Perform file completion in a case insensitive fashion
bind "set completion-ignore-case on"

# Immediately add a trailing slash when autocompleting symlinks to directories
bind "set mark-symlinked-directories on"

# Append history to $HISTFILE rather than overwriting it
shopt -s histappend     

# Save multi-line commands as one command
shopt -s cmdhist

# Increase number of commands saved to history list.
HISTSIZE=2000

# Avoid duplicate entries
HISTCONTROL="erasedups:ignoreboth"

# Don't record uninteresting commands
export HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:clear"

# Load Bash-specific startup files
for sh in "$HOME"/.bashrc.d/*.bash ; do
    # shellcheck source=/dev/null
    [[ -e $sh ]] && . "$sh"
done
unset -v sh

# From Homebrew bash-completion:
# shellcheck source=/dev/null
[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion

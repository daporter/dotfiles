# From https://superuser.com/questions/183870/difference-between-bashrc-and-bash-profile?answertab=active#tab-top
#
# ~/.bash_profile can be used instead of ~/.profile, but it is read by bash
# only, not by any other shell. (This is mostly a concern if you want your
# initialization files to work on multiple machines and your login shell isn't
# bash on all of them.) This is a logical place to include ~/.bashrc if the
# shell is interactive.
#
# There is even better information here:
# https://blog.flowblok.id.au/2013-02/shell-startup-scripts.html

if [ -r ~/.profile ]; then . ~/.profile; fi
case "$-" in *i*) if [ -r ~/.bashrc ]; then . ~/.bashrc; fi;; esac

#!/bin/sh

# Determine which (Bourne-compatible) shell we’re running under.
if [ -n "$ZSH_VERSION" ]; then
	CURRENT_SHELL='zsh'
elif [ -n "$BASH_VERSION" ]; then
	CURRENT_SHELL='bash'
elif [ -n "$KSH_VERSION" ]; then
	CURRENT_SHELL='ksh'
elif [ -n "$FCEDIT" ]; then
	CURRENT_SHELL='ksh'
elif [ -n "$PS3" ]; then
	CURRENT_SHELL='unknown'
else
	CURRENT_SHELL='sh'
fi

if [ "$CURRENT_SHELL" != 'sh' ] && [ "$TERM" != 'dumb' ]; then
	if [ -n "$SSH_CONNECTION" ]; then
		PS1="\u@\h:\w \$ "
	else
		PS1="\w \$ "
	fi
fi

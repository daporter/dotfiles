#!/bin/sh

bold=$(tput bold)
reset=$(tput sgr0)

PROMPT_DIRTRIM=2

if [ -n "$SSH_CONNECTION" ]; then
	PS1="\[$bold\]\u@\h:\w \$\[$reset\] "
else
	PS1="\[$bold\]\w \$\[$reset\] "
fi
PS2="> "

export PS1
export PS2

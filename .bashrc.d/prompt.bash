#!/usr/bin/env bash

bold=$(tput bold)
reset=$(tput sgr0)

PROMPT_DIRTRIM=2

PS1="\n"
PS1+="\[$bold\]"
PS1+="\w"
PS1+="\n\$ "
PS1+="\[$reset\]"

export PS1

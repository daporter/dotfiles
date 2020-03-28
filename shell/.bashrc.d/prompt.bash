#!/usr/bin/env bash

bold=$(tput bold)
reset=$(tput sgr0)

PROMPT_DIRTRIM=2
PS1="\[$bold\]\w \$\[$reset\] "
export PS1

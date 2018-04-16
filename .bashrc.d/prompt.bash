#!/bin/bash

RESET="$(tput sgr0)"

BLUE="$(tput setaf 4)"
GREEN="$(tput setaf 2)"
GREY="$(tput setaf 244)"
RED="$(tput setaf 1)"
YELLOW="$(tput setaf 3)"

git_prompt() {
    BRANCH=$(git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/*\(.*\)/\1/')

    if [ ! -z "$BRANCH" ]; then
        echo -n "$GREEN$BRANCH"

    if [ ! -z "$(git status --short)" ]; then
        echo " ${RED}✗"
    fi
        fi
}

vim_prompt() {
    if [ ! -z "$VIMRUNTIME" ]; then
        echo ":${GREEN}sh ";
    fi
}

jobs_prompt() {
    if [ $(jobs -p | wc -l) -ne 0 ]; then
        echo "${GREY}\j "
    fi
}

PS1="
\[${GREY}\t ${YELLOW}\j${RESET} ${BLUE}\w$(git_prompt)\]
\[${RESET}\]\$ "

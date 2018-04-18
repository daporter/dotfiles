#!/bin/bash

RESET="$(tput sgr0)"

BLUE="$(tput setaf 4)"
GREEN="$(tput setaf 2)"
GREY="$(tput setaf 244)"
RED="$(tput setaf 1)"
YELLOW="$(tput setaf 3)"

git_prompt() {
    GIT_PROMPT=
    BRANCH=$(git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/*\(.*\)/\1/')

    if [ ! -z "$BRANCH" ]; then
        GIT_PROMPT=$GREEN$BRANCH

        if [ ! -z "$(git status --short)" ]; then
            GIT_PROMPT=" ${GIT_PROMPT} ${RED}✗"
        fi
    fi
}

vim_prompt() {
    if [ ! -z "$VIMRUNTIME" ]; then
        echo ":${GREEN}sh ";
    fi
}

# Record each line as it gets issued
PROMPT_COMMAND='history -a'

PROMPT_COMMAND=${PROMPT_COMMAND}';git_prompt'

PS1='
\[${GREY}\t ${YELLOW}\j${RESET} ${BLUE}\w${RESET}${GIT_PROMPT}\]
\[${RESET}\]\$ '

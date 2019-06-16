#!/bin/bash

# shellcheck source=/dev/null
# shellcheck disable=SC1091
[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# Use fd (https://github.com/sharkdp/fd) instead of the default find
# command for listing path candidates.
# - The first argument to the function ($1) is the base path to start traversal
# - See the source code (completion.{bash,zsh}) for the details.
_fzf_compgen_path() {
  fd --hidden --follow --exclude ".git" . "$1"
}

# Use fd to generate the list for directory completion
_fzf_compgen_dir() {
  fd --type d --hidden --follow --exclude ".git" . "$1"
}

export FZF_DEFAULT_COMMAND='fd --type f --color=never'

export FZF_DEFAULT_OPTS="
    --height 75% --multi --reverse
    --bind ctrl-f:page-down,ctrl-b:page-up
"

export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_ALT_C_COMMAND='fd --type d . --color=never'

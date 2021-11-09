#!/bin/bash

# Git aliases.

alias g='git'
alias ga='git add'
alias gb='git branch'
alias gcl='git clone'
alias gcm='git commit'
alias gco='git checkout'
alias gd='git diff'
alias gdc='git diff --cached'
alias gf='git fetch'
alias gfa='git fetch -all'
alias gl='git log'
alias gm='git merge'
alias gp='git push'
alias gpl='git pull'
alias gst='git status'

# Configure completions.

if type -t __git_complete >/dev/null 2>&1; then
    __git_complete g _git
    __git_complete ga _git_add
    __git_complete gb _git_branch
    __git_complete gcl _git_clone
    __git_complete gcm _git_commit
    __git_complete gco _git_checkout
    __git_complete gd _git_diff
    __git_complete gf _git_fetch
    __git_complete gl _git_log
    __git_complete gm _git_merge
    __git_complete gp _git_push
    __git_complete gpl _git_pull
    __git_complete gts _git_status
fi

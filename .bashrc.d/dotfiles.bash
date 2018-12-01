#!/usr/bin/env bash

# Create an alias to easily manage my dotfiles git repo.
alias dotfiles='/usr/local/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'

dcm() {
	dotfiles checkout master
}

dcd() {
	dotfiles checkout data61
}

dmm() {
	dotfiles merge master
}

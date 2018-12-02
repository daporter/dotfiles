#!/bin/sh

dotfiles() {
	/usr/local/bin/git --git-dir="$HOME/.dotfiles/" --work-tree="$HOME" "$@"
}

d() {
	if [ $# -eq 0 ]; then
		dotfiles status
	else
		dotfiles "$@"
	fi
}

dcm() {
	dotfiles checkout master
}

dcd() {
	dotfiles checkout data61
}

dmm() {
	dotfiles merge master
}

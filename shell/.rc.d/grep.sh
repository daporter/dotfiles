#!/bin/sh

# Discard GNU grep(1) environment variables if the environment set them
unset -v GREP_OPTIONS

# Define function proper
grep() {
	# Add --binary-files=without-match to gracefully skip binary files
	set -- --binary-files=without-match "$@"

	# Add --color=auto if the terminal has at least 8 colors
	[ "$({ tput colors||tput Co||echo 0; } 2>/dev/null)" -ge 8 ] &&
		set -- --color=auto "$@"

	# Add --devices=skip to gracefully skip devices
	set -- --devices=skip "$@"

	# Add --directories=skip to gracefully skip directories
	set -- --directories=skip "$@"

	# Add --exclude to ignore .gitignore and .gitmodules files
	# set -- \
		#    --exclude=.gitignore  \
		#    --exclude=.gitmodules \
		#    "$@"

	# Add --exclude-dir to ignore version control dot-directories
	set -- \
	    --exclude-dir=.cvs \
	    --exclude-dir=.git \
	    --exclude-dir=.hg  \
	    --exclude-dir=.svn \
	    "$@"

	# Run grep(1) with the concluded arguments
	command grep "$@"
}

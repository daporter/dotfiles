#!/bin/sh

# I've stolen this from Tom Ryder's config at
# https://sanctum.geek.nz/cgit/dotfiles.git/tree/sh/shrc.d/ls.sh
#
# I also took ideas from here:
# https://www.topbug.net/blog/2016/11/28/a-better-ls-command/#better-color

eval "$(dircolors)"

export COLUMNS  # Remember columns for subprocesses

# Define function proper.
ls() {
    # -q to replace control chars with '?'
    set -- -q "$@"

    # Add -k to always show the filesize in kilobytes
    set -- -k "$@"

    # Indicate file types
    set -- -F "$@"

    # Display human-readable file sizes
    set -- -h "$@"

    # Display files in natural order
    set -- -v "$@"

    set -- --author "$@"

    set -- --time-style=long-iso "$@"

    set -- --color=always "$@"

    set -- -C "$@"

    # Run ls(1) with the concluded arguments
    command ls "$@" | less -R -X -F
}

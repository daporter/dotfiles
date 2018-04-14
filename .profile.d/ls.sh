# I've stolen this from Tom Ryder's config at
# https://sanctum.geek.nz/cgit/dotfiles.git/tree/sh/shrc.d/ls.sh

# Discard GNU ls(1) environment variables if the environment set them.
unset -v LS_OPTIONS LS_COLORS

# Define function proper.
function ls() {
    # -F to show trailing indicators of the filetype
    # -q to replace control chars with '?'
    set -- -Fq "$@"

    # If output is to a terminal, add -x to format entries across, not down
    [ -t 1 ] && set -- -x "$@"

    # Add -k to always show the filesize in kilobytes
    set -- -k "$@"

    # Add -G if the terminal has at least 8 colors
    [ "$({ tput colors||tput Co||echo 0; } 2>/dev/null)" -ge 8 ] &&
        set -- -G "$@"

    # Run ls(1) with the concluded arguments
    command ls "$@"

}

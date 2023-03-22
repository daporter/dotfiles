#!/bin/sh
#
# Join strings with a given separator.

join() {
    if [ $# -eq 0 ]; then
        return
    fi

    separator="$1"
    shift

    while [ $# -gt 1 ]; do
        printf "%s%s" "$1" "$separator"
        shift
    done

    printf '%s\n' "$1"
}

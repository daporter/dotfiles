#!/bin/sh

/usr/local/bin/lockrun --lockfile ${HOME}/.mutt/tmp/.notmuch.lock --quiet -- \
    /usr/local/bin/notmuch new 2> /dev/null

#!/usr/bin/env bash

# Zappa completion.
if [ -x register-python-argcomplete ]; then
    eval "$(register-python-argcomplete zappa)"
fi

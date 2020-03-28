#!/bin/sh

# This file is automatically executed by Korn login shells.

if [ -r "$HOME/.kshrc"  ]; then
	ENV=$HOME/.kshrc
	export ENV
fi

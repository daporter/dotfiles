#!/bin/sh

eval "$(fasd --init auto)"

# Use "c" instead of "z" to do directory jumping.
alias c='fasd_cd -d'
alias ci='fasd_cd -d -i'

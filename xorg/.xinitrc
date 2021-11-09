#!/bin/sh

xsetroot -cursor_name left_ptr
xrdb -I "$HOME" -merge "$HOME"/.Xresources

picom &
dunst &
sxhkd &

exec bspwm

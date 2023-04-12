#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
	for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
		[ -x "$f" ] && . "$f"
	done
	unset f
fi

# Set the environment for the session.
if [ -r "$HOME"/.profile ]; then
	. "$HOME"/.profile
fi

if [ -r "$HOME"/.Xresources ]; then
	xrdb -I "$HOME" -merge "$HOME"/.Xresources
fi

xset b off           # no bell
xset r rate 150 55   # repeat after 150 ms, 55 char/s

xsetroot -cursor_name left_ptr

# Make the Scroll Lock key act as the Compose key.
setxkbmap -option compose:sclk

if [ -f "$HOME"/.fehbg ]; then
	"$HOME"/.fehbg &
fi

configure_monitors

lxpolkit &
redshift &
pipewire &
pipewire-pulse &
wireplumber &
dunst &
dwmbar &
sxhkd &
dropbox &

if ! pgrep kmonad > /dev/null; then
	kmonad "$HOME"/.config/kmonad/kd87.kbd &
fi

exec dwm

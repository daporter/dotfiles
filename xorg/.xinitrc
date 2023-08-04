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
xset r rate 200 55   # repeat after 200 ms, 55 char/s

xsetroot -cursor_name left_ptr

setxkbmap -option compose:menu

if [ -f "$HOME"/.fehbg ]; then
	"$HOME"/.fehbg &
fi

dbus-update-activation-environment --all --systemd

lxpolkit &
1password --silent &

~/bin/dwmbar &
~/bin/configure_monitors


exec ~/bin/startdwm

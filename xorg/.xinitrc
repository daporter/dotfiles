#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
	for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
		[ -x "$f" ] && . "$f"
	done
	unset f
fi

if [ -r "$HOME"/.Xresources ]; then
	xrdb -I "$HOME" -merge "$HOME"/.Xresources
fi

xset b off           # no bell
xset r rate 200 45   # repeat after 200 ms, 45 char/s

xsetroot -cursor_name left_ptr

# Enable ThinkPad TrackPoint scrolling by dragging with the middle
# button.
xinput set-prop "/dev/wsmouse" "WS Pointer Wheel Emulation" 1
xinput set-prop "/dev/wsmouse" "WS Pointer Wheel Emulation Button" 2
xinput set-prop "/dev/wsmouse" "WS Pointer Wheel Emulation Axes" 6 7 4 5

configure_monitors

# TODO: Make this an runit service to ensure there is not more than one
# process.
picom &

# TODO: Make this an runit service to ensure there is not more than one
# process.
redshift &

dunst &
sxhkd &

exec bspwm

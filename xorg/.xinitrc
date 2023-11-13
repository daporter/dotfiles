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
xset r rate 200 55   # repeat after 200 ms, 55 char/s
xsetroot -cursor_name left_ptr
setxkbmap -option compose:menu

picom -b
~/bin/sbar &
sxhkd &
feh --bg-fill --no-fehbg ~/.config/wallpapers/wallpaper.jpg
unclutter --fork

1password --silent &

exec ~/bin/startdwm

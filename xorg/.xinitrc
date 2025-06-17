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

xset b off                      # no bell
xset r rate 200 100             # repeat after 200 ms, 100 char/s
xsetroot -cursor_name left_ptr

# Make my trackball pointer speed a bit slower than the default.
# (Values range from -1 [slowest] to 1 [fastest]; 0 is default.)
xinput set-prop "Kensington SlimBlade Pro Trackball(Wired) Kensington SlimBlade Pro Trackball(Wired)" "libinput Accel Speed" -0.9

picom -b
redshift &
sxhkd &
xbanish &
1password --silent &
dropbox start &

/usr/bin/systemctl --user start mtstatus
exec /usr/local/bin/dwm

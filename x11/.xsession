#!/bin/sh

# Truncate a couple of common commands that are used herein.
_check() {
	command -v "$1" >/dev/null
}

# Set environment variables for the session.
. "$HOME/.environment"

# When the Korn shell is started interactively it reads the file named
# by ENV.
export ENV=$HOME/.kshrc

# When Bash is started non-interactively it reads the file named by
# BASH_ENV.
export BASH_ENV=$HOME/.environment

[ -r "$HOME/.Xresources" ] && xrdb -merge "$HOME/.Xresources"

xset b off           # no bell
xset r rate 200 45   # repeat after 200 ms, 45 char/s

xsetroot -cursor_name left_ptr # mouse pointer

# Enable ThinkPad TrackPoint scrolling by dragging with the middle
# button.
xinput set-prop "/dev/wsmouse" "WS Pointer Wheel Emulation" 1
xinput set-prop "/dev/wsmouse" "WS Pointer Wheel Emulation Button" 2
xinput set-prop "/dev/wsmouse" "WS Pointer Wheel Emulation Axes" 6 7 4 5

# Load my keyboard mappings.  Note that xcape depends on the xmodmap
# mappings being in place before it is run.
[ -r "$HOME/.Xmodmap" ] && xmodmap "$HOME/.Xmodmap"
xcape -e 'Control_L=Caps_Lock;Control_R=Return' &

# Run the tool that sets the wallpaper.  A program could be used that
# handles the desktop in general, including the option to set icons.
# But this is enough for my case.  Here is a command I tested and which
# works (using the Xfce desktop manager):
# xfdesktop --sm-client-disable --disable-wm-check &
_check feh && "$HOME/.fehbg" &

# Automatically lock the screen after 5 minutes of inactivity.
xidle -timeout 300 &

# Launch xbanish to hide the mouse cursor when a key is pressed.
_check xbanish && xbanish &

# Start the simple display compositor.  See `xcompmgr --help'.
xcompmgr -r 2.8 -o 0.55 -l 2 -t 2 -D 5 -cCfF &

# Check for an external monitor; if found, switch to it.
extmonitor connect

exec bspwm

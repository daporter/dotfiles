#!/bin/sh

# Truncate a couple of common commands that are used herein.
_check() {
	command -v "$1" > /dev/null
}

# Set environment variables for the session.
. $HOME/.environment

# When the Korn shell is started interactively it reads the file named
# by ENV.
export ENV=$HOME/.kshrc

# When Bash is started non-interactively it reads the file named by
# BASH_ENV.
export BASH_ENV=$HOME/.environment

[ -f "$HOME/.Xdefaults" ] && xrdb -merge "$HOME/.Xdefaults"

xset +dpms                     # turn off screen when idle
xset b off                     # no bell
xset r rate 200 45             # repeat after 200 ms, 45 char/s

xsetroot -cursor_name left_ptr # mouse pointer

# if you have a ThinkPad, the following lines enable TrackPoint
# scrolling by dragging with the middle button.
xinput set-prop "/dev/wsmouse" "WS Pointer Wheel Emulation" 1 
xinput set-prop "/dev/wsmouse" "WS Pointer Wheel Emulation Button" 2 
xinput set-prop "/dev/wsmouse" "WS Pointer Wheel Emulation Axes" 6 7 4 5 

# Display compositor for enabling shadow effects and transparency
# (disable it if performance is bad---also bear in mind that I do not
# use transparent areas in any of my interfaces).
_check compton && compton --config "$HOME"/.config/compton.conf &

# Run the tool that sets the wallpaper.  A program could be used that
# handles the desktop in general, including the option to set icons.
# But this is enough for my case.  Here is a command I tested and which
# works (using the Xfce desktop manager):
# xfdesktop --sm-client-disable --disable-wm-check &
_check feh && "$HOME"/.fehbg &

# Launch the Dunst notifier.
_check dunst && dunst &

# Launch xbanish to hide the mouse cursor when a key is pressed.
_check xbanish && xbanish &

exec bspwm

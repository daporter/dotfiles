#!/bin/sh
#
# Called by hypridle after resume from suspend (hypridle.conf `after_sleep_cmd`).
#
# A plain `dpms on` is a no-op here: S3 cuts power to the monitor without
# Hyprland ever marking the output off, so Hyprland still believes DP-1 is on
# and emits no new signal. The monitor stays in standby waiting for a
# transition that never arrives, and only a manual power-cycle recovers it.
#
# Toggling off -> on forces the GPU to stop and restart driving the output,
# which is the edge the monitor needs to re-illuminate.
#
# The leading delay lets amdgpu finish re-initialising before we touch DPMS;
# on resume it is still bringing the ASIC back up (PSP resume, GART, rings).

sleep 1
hyprctl dispatch 'hl.dsp.dpms({ state = "off" })'
sleep 2
hyprctl dispatch 'hl.dsp.dpms({ state = "on" })'

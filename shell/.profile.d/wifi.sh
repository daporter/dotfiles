#!/bin/sh
#
# Determine the name of the WiFi interface.

WIFI_INTERFACE=$(grep -E '^\w+:' /proc/net/wireless | cut -d: -f1)
export WIFI_INTERFACE

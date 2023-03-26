#!/bin/sh
#
# Connect to a WiFi network.

wifi () {
	iwctl station wlan0 connect "$@"
}

#!/bin/sh

xq() {
	command xbps-query "$@"
}

xi() {
	set -- --sync "$@"
	set xbps-install "$@"
	command sudo "$@"
}

xu() {
	set -- --update "$@"
	set -- --sync "$@"
	set xbps-install "$@"
	command sudo "$@"

}

xr() {
	set -- --recursive "$@"
	set -- --remove-orphans "$@"
	set -- --clean-cache "$@"
	set xbps-remove "$@"
	command sudo "$@"
}

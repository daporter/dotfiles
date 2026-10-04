# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

Personal dotfiles for an Arch Linux / Hyprland (Wayland) desktop, managed with GNU Stow.

## Package

On `waybar-git` (AUR), not `extra/waybar` — reason not recorded;
investigate before switching back to `extra/waybar`.

Cost of `-git`: rebuild with `paru -S waybar-git` after any dependency
soname bump (e.g. `jsoncpp`). Otherwise the binary fails to start (exit 127) and the systemd unit crash-loops.

## Custom module refresh signals

Custom modules that need an out-of-band refresh (rather than relying on
`interval` polling alone) use waybar's `signal` field, triggered externally
via `pkill -RTMIN+N waybar`. Offsets in use, so a new module doesn't collide
with an existing one:

| Offset (`RTMIN+N`) | Module        | Sent by                                |
| ------------------ | ------------- | -------------------------------------- |
| 8                  | `custom/mail` | `notmuch/Mail/.notmuch/hooks/post-new` |

When adding a new signal-driven custom module, pick the next unused offset
and add a row here in the same commit.

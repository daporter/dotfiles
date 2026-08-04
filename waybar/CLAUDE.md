# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

Personal dotfiles for an Arch Linux / Hyprland (Wayland) desktop, managed with GNU Stow.

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

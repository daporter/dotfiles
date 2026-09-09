# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

Personal dotfiles for an Arch Linux / Hyprland (Wayland) desktop, managed with GNU Stow.

## Package

On `waybar-git` (AUR), not `extra/waybar`, since 2026-08-21 (commit
`bdbc816f`) — reason not recorded; investigate before switching back to
`extra/waybar`.

Cost of `-git`: rebuild with `paru -S waybar-git` after any dependency
soname bump. On 2026-09-10 a `jsoncpp` update (`libjsoncpp.so.26` →
`.so.27`) left the binary unable to start (exit 127, crash-looped by the
systemd unit) until it was rebuilt.

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

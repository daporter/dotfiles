# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

Personal dotfiles for an Arch Linux / Hyprland (Wayland) desktop, managed with GNU Stow.

## Stow layout and workflow

Each top-level directory is a **Stow package**. Its contents mirror the target
tree relative to `$HOME` — e.g. `ghostty/.config/ghostty/config` deploys to
`~/.config/ghostty/config`, and `bash/.bashrc` deploys to `~/.bashrc`.

`.stowrc` pins `--target=$HOME` and `--ignore='\.NO-STOW'`, so run Stow from the
repo root:

```sh
stow <package>       # symlink a package into $HOME
stow -D <package>    # remove its symlinks
stow -R <package>    # restow (after adding/removing/renaming files)
```

- Because Stow uses **symlinks**, editing a file in this repo changes the live
  config in `$HOME` immediately — no restow needed for content edits. Restow
  (`stow -R`) only when files are **added, removed, or renamed**.
- A file or directory named `.NO-STOW` is ignored by Stow (per `.stowrc`); use
  it to keep repo-only files out of `$HOME`.

## Modular shell configuration

Shell config is assembled from fragments contributed by many packages, rather
than living in one file. `bash/.bashrc` sources, in order:

1. `~/.rc.d/*.sh` — POSIX/shell-agnostic fragments
2. `~/.bashrc.d/*.bash` — bash-specific fragments

Login-shell fragments live in `~/.profile.d/`. A package adds shell behavior by
dropping a fragment into the matching directory within its own tree (e.g.
`gnupg/.rc.d/`, `restic/.profile.d/`, `emacs/.bashrc.d/`) — it is picked up
automatically once stowed. Prefer this over editing `bash/.bashrc` directly.

## Conventions

- **Commit messages**: `<package>: <imperative summary>` (e.g.
  `btop: sort processes by CPU usage`). Enforced by `git/.gitlint`: title ≤ 50
  chars, body lines ≤ 72 chars.
- **Lua files** (notably `hyprland/.config/hypr/hyprland.lua`): run `stylua` on
  any Lua file you edit before committing. No `stylua.toml` exists, so it uses
  stylua defaults.
- Hyprland is configured in **Lua** (`hyprland.lua`), not the conventional
  `hyprland.conf`.

## Package manifests

`archlinux/pacman-list.txt` (official repos) and `archlinux/aur-list.txt` (AUR)
are hand-maintained records of installed packages — they are not applied
automatically and must be regenerated manually when the system changes.

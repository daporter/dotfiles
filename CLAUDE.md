# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

Personal dotfiles for an Arch Linux / Hyprland (Wayland) desktop, managed with GNU Stow.

## Stow layout and workflow

Each top-level directory is a **Stow package**. Its contents mirror the target
tree relative to `$HOME` — e.g. `ghostty/.config/ghostty/config.ghostty` deploys
to `~/.config/ghostty/config.ghostty`, and `bash/.bashrc` deploys to `~/.bashrc`.

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

### System-level (`/etc`) files

Root-owned config that lives outside `$HOME` (e.g. `/etc/samba/smb.conf`) can't
use the repo-wide `--target=$HOME`. Such packages live under the top-level
`system/` **stow directory**, mirror the system root (`/`), and are deployed
explicitly as root:

```sh
doas stow --dir=system --target=/ <package>       # symlink into /etc, ...
doas stow --dir=system --target=/ -R <package>    # restow after file changes
```

Keeping them under `system/` prevents a bare `stow <package>` from symlinking
them into `~/etc/...`. See `system/README.md` for details and the per-package
deploy steps. (Only safe because `/home` is on the root filesystem, so the
symlink targets are available at early boot.)

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

`archlinux/.NO-STOW/pacman-list.txt` (official repos) and
`archlinux/.NO-STOW/aur-list.txt` (AUR) record the explicitly-installed
packages. They are regenerated automatically after every `pacman` install/remove
by PostTransaction hooks in `/etc/pacman.d/hooks/` (`pacman-list.hook`,
`aur-list.hook`), which write `pacman -Qqen` / `pacman -Qqem` into these files;
no manual step is needed. They are records only — never applied back to the
system automatically. They live under `.NO-STOW/` so `stow archlinux` never
symlinks them into `$HOME`.

The hooks themselves are version-controlled as the `system/archlinux/` package
(deployed with `--target=/`; see the System-level files note above).

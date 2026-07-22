# system — Stow packages for root-owned `/` files

Packages here mirror the **system root (`/`)**, not `$HOME`. They hold
root-owned configuration under `/etc` (and similar) that GNU Stow manages by
symlinking with `--target=/` instead of the repo-wide `--target=$HOME`.

Because the repo's `.stowrc` pins `--target=$HOME`, these are kept in a separate
`system/` **stow directory** so a bare `stow <pkg>` from the repo root can never
accidentally symlink them into `~/etc/...`. Always deploy them explicitly with
`--dir=system --target=/`, as root (via `doas`).

> Safe here because `/home` is on the **root filesystem** — the symlink targets
> (under `/home/david/dotfiles`) are available as early as `/` itself. Do *not*
> use this pattern for boot-critical files if `/home` ever becomes a separate,
> late-mounted partition.

## Packages

- `samba/` — `/etc/samba/smb.conf` (standalone file server, private `[Photos]`
  LAN share) and the `smb.service` drop-in requiring the `/mnt/media` mount.
- `archlinux/` — the `PostTransaction` pacman hooks
  (`/etc/pacman.d/hooks/{pacman-list,aur-list}.hook`) that regenerate the
  package manifests under `archlinux/.NO-STOW/`.

## Deploy

```sh
cd ~/dotfiles

# First time: remove the existing real files so Stow can place its symlinks
doas rm -f /etc/samba/smb.conf \
           /etc/systemd/system/smb.service.d/require-media-mount.conf

doas stow --dir=system --target=/ samba      # symlink into /etc
doas systemctl daemon-reload                 # pick up the drop-in
doas systemctl reload smb.service            # apply smb.conf

# After renaming/adding/removing files in a package, restow:
doas stow --dir=system --target=/ -R samba
```

Edits to a file's *contents* take effect immediately (symlinks) — just
`daemon-reload` / `reload smb` as appropriate. The Samba password for `david`
lives in the tdbsam passdb, not in this repo (`doas smbpasswd -a david`).

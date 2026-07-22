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
  and `[Music]` LAN shares) and the `smb.service` drop-in requiring the
  `/mnt/media` mount.
- `archlinux/` — the `PostTransaction` pacman hooks
  (`/etc/pacman.d/hooks/{pacman-list,aur-list}.hook`) that regenerate the
  package manifests under `archlinux/.NO-STOW/`.
- `networkd/` — the `systemd-networkd` config in `/etc/systemd/network/`: the
  wired (`20-wired.network`) and wireless (`25-wireless.network`) DHCP setups,
  plus `20-wired.link`, which arms magic-packet Wake-on-LAN on the `r8169` NIC.

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

For `networkd/`, remove the existing real files first, then stow and reload:

```sh
doas rm -f /etc/systemd/network/20-wired.network \
           /etc/systemd/network/25-wireless.network

doas stow --dir=system --target=/ networkd   # symlinks all three files in
doas systemctl restart systemd-networkd      # apply the .network changes

# Arm WoL now without a reboot (the .link is applied by udev on device add):
doas udevadm trigger --action=add /sys/class/net/eno1
```

Edits to a file's *contents* take effect immediately (symlinks) — just
`daemon-reload` / `reload smb` as appropriate. The Samba password for `david`
lives in the tdbsam passdb, not in this repo (`doas smbpasswd -a david`).

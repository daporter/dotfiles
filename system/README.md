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
- `openssh/` — `/etc/ssh/sshd_config.d/10-local.conf`, the local `sshd` policy:
  public-key logins for `david` only, and only from the `192.168.178.0/24` home
  LAN. The authorized keys themselves are *not* here — they are stowed from the
  `$HOME`-side `ssh/` package as `~/.ssh/authorized_keys`.
- `networkd/` — the `systemd-networkd` config in `/etc/systemd/network/`: the
  wired DHCP setup (`20-wired.network`) plus `20-wired.link`, which arms
  magic-packet Wake-on-LAN on the `r8169` NIC. This host is wired-only.
  **Deployed by copying, not stowing** — see below.

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

### `openssh/`

`/etc/ssh/sshd_config.d/` already holds Arch's `99-archlinux.conf`, so Stow
cannot fold the directory and will symlink just the one file into it:

```sh
cd ~/dotfiles

stow -R ssh                                    # ~/.ssh/authorized_keys
doas stow --dir=system --target=/ openssh      # /etc/ssh/sshd_config.d/
doas ssh-keygen -A                             # host keys, if none yet
doas sshd -t                                   # validate before enabling
doas systemctl enable --now sshd
```

The `ssh-keygen -A` step is only needed the first time. `sshd.service` pulls in
`sshdgenkeys.service` (which runs the same command) via `Wants=`, so starting
the daemon would generate the keys by itself — but `sshd -t` is a bare binary
invocation that bypasses systemd, and it exits with `no hostkeys available`
*before* checking the config, so validation is useless until they exist.

Arch no longer ships an `sshd.socket`, so this is the plain `sshd.service`.
After editing `10-local.conf`, `doas systemctl reload sshd` — existing sessions
survive a reload, so a mistake that locks you out is still recoverable from the
session you already have open. Confirm the policy took with:

```sh
sshd -G | grep -iE 'allowusers|addressfamily|passwordauthentication'
```

Use `-G`, not `-T`: both dump the effective config, but `-T` also runs the `-t`
key sanity check, so it needs root *and* pre-existing host keys. `-G` skips
that and runs fine as an ordinary user. Keep the `grep -i` — OpenSSH 10 prints
keywords in canonical CamelCase (`AllowUsers`), not the all-lowercase form
older versions emitted, so a lowercase pattern silently matches nothing.

### `networkd/` — copy, do **not** stow

`systemd-networkd` runs as the unprivileged `systemd-network` user with
`ProtectHome=yes` in its service sandbox, which masks `/home` entirely. It
therefore **cannot follow a symlink into `/home/david/dotfiles`** — stowed
`.network` files fail with `Failed to chase '…': Permission denied`, the links
are left `unmanaged`, and the machine boots with no DHCP lease and no route.
(The `.link` file is exempt only because *udev*, running as root, applies it —
not networkd.) So this package is deployed by **copying real files** into
`/etc/systemd/network/`, not by stowing:

```sh
cd ~/dotfiles

doas cp system/networkd/etc/systemd/network/20-wired.link \
        system/networkd/etc/systemd/network/20-wired.network \
        /etc/systemd/network/
doas systemctl restart systemd-networkd      # apply the .network changes

# Arm WoL now without a reboot (the .link is applied by udev on device add):
doas udevadm trigger --action=add /sys/class/net/eth0
```

Unlike the stowed packages, edits here are **not** live: after changing a file
in the repo, re-run the `cp` above and `restart systemd-networkd` (or
`udevadm trigger` for the `.link`) to redeploy. `networkctl status eth0` should
show a `Network File:` set and state `routable (configured)`.

> `eth0` above is this NIC's *current* name, not a stable one. Because
> `20-wired.link` matches it and sets no `NamePolicy=`, it replaces
> `99-default.link` and udev never renames the device, so it keeps the kernel
> fallback `eth0` rather than its predictable name `eno1`. Config files
> therefore match it by **driver** (`20-wired.network`) or **subnet**
> (Samba's `interfaces=`) — never by name. Check the live name with
> `networkctl list` before using it in an ad-hoc command.

---

The stowed packages differ: edits to a file's *contents* take effect
immediately (symlinks) — just `daemon-reload` / `reload smb` as appropriate. The
Samba password for `david` lives in the tdbsam passdb, not in this repo
(`doas smbpasswd -a david`).

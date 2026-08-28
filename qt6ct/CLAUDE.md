# qt6ct package

Qt6 widget theming for apps that honour `QT_QPA_PLATFORMTHEME=qt6ct` (set in
`hyprland/.config/hypr/hyprland.lua`) — Anki, GoldenDict, etc.

Key choices in `qt6ct.conf`:

- `style=Adwaita` with `custom_palette=false` — the Adwaita style supplies its
  own light palette, matching the light GTK desktop (`gsettings` `color-scheme`
  = `prefer-light`). Not `Adwaita-Dark`, which produced near-black buttons on
  light surfaces.
- No `icon_theme` key — falls back to the system default (Adwaita), matching
  `gtk-icon-theme-name` in the `gtk/` package.
- `force_raster_widgets=1` — kept from prior setup.

The `[SettingsWindow]` section (window geometry) is intentionally omitted: qt6ct
rewrites it whenever its GUI is opened. If that section reappears after opening
the GUI, discard it (`git checkout`) or commit the one line.

`~/.config/qt6ct/` holds only this file. Stow with `stow --no-folding qt6ct`.

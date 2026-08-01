# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

Personal dotfiles for an Arch Linux / Hyprland (Wayland) desktop, managed with GNU Stow.

## Conventions

- Hyprland is configured in **Lua** (`hyprland.lua`), not the older `hyprland.conf`.
- `.config/hypr/keybindings.html` is a standalone cheatsheet (opened via
  `SUPER+SLASH`, see the `hl.bind` for it in `hyprland.lua`) and is **not**
  generated from `hyprland.lua` — it's a hand-maintained mirror. Whenever a
  `hl.bind(...)` is added, removed, or rebound in `hyprland.lua`, update the
  matching row in `keybindings.html` in the same commit:
  - Match the `<h2>` section to the `hl.bind` calls' comment header above them
    in `hyprland.lua` (Window Focus, Window Cycling, Workspaces, Window
    Movement, Move to Workspace, Applications, Window Resizing, Layout,
    Scratchpad, Hyprland).
  - Render the key combo in Title Case joined with `+`, e.g.
    `SUPER+SHIFT+A` → `Super+Shift+A`.
  - Render mouse buttons by name per the Mouse Buttons legend at the bottom
    of the page, e.g. `mouse:272` → `LMB`, `mouse:273` → `RMB`.

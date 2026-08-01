# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

Personal dotfiles for an Arch Linux / Hyprland (Wayland) desktop, managed with GNU Stow.

## Keybinding policy

`init.el` layers four keybinding mechanisms deliberately, not redundantly.
`which-key` is active and is the discoverability substrate the other three
sit on top of — it pops up available continuations for any prefix sequence,
so discoverability by itself is not a reason to route a command into a menu
system instead of a plain bind.

When adding a new personal keybinding, pick the bucket by what the command
depends on, not by habit:

- **Bare, unprefixed chord** (e.g. `C-,`, `C-.`, `C-*`) — reserve for
  commands fired constantly enough to be muscle memory. There's no prefix
  for which-key to show a popup for, so this is the one bucket where
  discoverability really is the cost of a direct bind.
- **Prefixed sequence, unconditionally relevant** — a plain mode-map or
  global bind. which-key already makes it discoverable; wrapping it in a
  Transient menu adds an extra keystroke and a redundant label for no gain.
- **Prefixed sequence, conditionally relevant on mode/buffer state** — an
  entry in `my/dispatch-menu` (defined via `:if-derived`/`:if-non-nil`
  conditions), or a `casual-suite` menu if the mode already has vendor
  coverage there (don't duplicate it). Transient earns its keep here
  specifically because it can hide entries that don't apply right now —
  which-key can only list, not filter.
- **Target-typed action** — operates on "a region," "an identifier," "a
  completion candidate," etc. regardless of major mode. Goes in the matching
  Embark action map (e.g. `embark-region-map`, `embark-identifier-map`).
  This is the only bucket keyed on the type of the thing at point rather
  than mode or prefix, and the only one that works from inside the
  minibuffer/completion UI before a candidate has become a buffer.

The three menu systems (`embark`, `casual-suite`, `my/dispatch-menu`) are
kept side by side intentionally: the Transient-based ones (dispatcher,
Casual) key off buffer/mode state, Embark keys off the type of the thing at
point — different axes, not competing implementations of the same idea.

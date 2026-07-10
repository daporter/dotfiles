hl.monitor({
	output = "DP-1",
	mode = "2560x1440@120Hz",
	position = "0x0",
	scale = 1,
})

hl.on("hyprland.start", function()
	hl.exec_cmd("hyprctl setcursor Adwaita 24")
	hl.exec_cmd("hyprpaper")
	hl.exec_cmd("waybar")
	hl.exec_cmd("hypridle")
	hl.exec_cmd("wlsunset -l -35.3 -L 149.1 -t 3700 -T 5500")
	hl.exec_cmd("swaync")
	hl.exec_cmd("systemctl --user import-environment WAYLAND_DISPLAY XDG_CURRENT_DESKTOP")
	hl.exec_cmd("dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP=Hyprland")

	-- From instructions at https://wiki.hypr.land/Hypr-Ecosystem/hyprpolkitagent/
	hl.exec_cmd("systemctl --user start hyprpolkitagent")

	hl.exec_cmd("1password --silent")
	hl.exec_cmd("emacs --daemon")
	hl.exec_cmd("gnome-keyring-daemon --start --components=secrets")
	hl.exec_cmd("goldendict --group-name Main --popup-group-name Popup")

	-- Launch a terminal on the scratchpad.
	hl.exec_cmd("ghostty --title=scratchpad")
end)

hl.env("SSH_AUTH_SOCK", "$XDG_RUNTIME_DIR/gcr/ssh")
hl.env("GOLDENDICT_FORCE_WAYLAND", "1")
hl.env("QT_QPA_PLATFORMTHEME", "qt6ct")

-- Force all apps to use Wayland
hl.env("GDK_BACKEND", "wayland,x11,*")
hl.env("QT_QPA_PLATFORM", "wayland;xcb")
hl.env("SDL_VIDEODRIVER", "wayland")
hl.env("MOZ_ENABLE_WAYLAND", "1")
hl.env("ELECTRON_OZONE_PLATFORM_HINT", "wayland")
hl.env("OZONE_PLATFORM", "wayland")
hl.env("XDG_SESSION_TYPE", "wayland")

--Allow better support for screen sharing (Google Meet, Discord, etc)
hl.env("XDG_CURRENT_DESKTOP", "Hyprland")
hl.env("XDG_SESSION_DESKTOP", "Hyprland")

-- Font rendering
hl.env("FREETYPE_PROPERTIES", "truetype:interpreter-version=38")
hl.env("GDK_SCALE", "1")
hl.env("GDK_DPI_SCALE", "1")
hl.env("QT_AUTO_SCREEN_SCALE_FACTOR", "1")
hl.env("QT_SCALE_FACTOR", "1")

hl.config({
	general = {
		gaps_in = 3,
		gaps_out = 6,
		border_size = 2,
		resize_on_border = true,
		col = {
			active_border = "rgba(93a1a1ff)",
			inactive_border = "rgba(eee8d5ff)",
		},
		layout = "dwindle",
	},

	input = {
		kb_options = "compose:menu",
		repeat_rate = 90,
		repeat_delay = 300,

		natural_scroll = true,
	},

	misc = {
		force_default_wallpaper = 0,
		animate_manual_resizes = true,
		animate_mouse_windowdragging = true,
		focus_on_activate = true,
	},

	cursor = {
		hide_on_key_press = true,
	},

	dwindle = {
		force_split = 2, -- to the right or below
	},
})

-- Kensington SlimBlade Pro trackball: use adaptive acceleration so fast
-- movement (crossing the screen) and slow movement (grabbing a scrollbar)
-- both feel comfortable, rather than one fixed sensitivity for both.
-- The trackball exposes a second HID interface for buttons
-- (...-keyboard-1), but `libinput list-devices` shows it has no motion
-- capability (Accel profiles: n/a), so only this interface needs it.
hl.device({
	name = "kensington-slimblade-pro-trackball(wired)-kensington-slimblade-pro-trackball(wired)",
	sensitivity = -0.7,
	accel_profile = "adaptive",
})

--
-- KEYBINDINGS
--

-- Window focus
hl.bind("SUPER+A", hl.dsp.focus({ direction = "l" }))
hl.bind("SUPER+E", hl.dsp.focus({ direction = "u" }))
hl.bind("SUPER+I", hl.dsp.focus({ direction = "d" }))
hl.bind("SUPER+H", hl.dsp.focus({ direction = "r" }))

-- Workspaces
hl.bind("SUPER+1", hl.dsp.focus({ workspace = "1" }))
hl.bind("SUPER+2", hl.dsp.focus({ workspace = "2" }))
hl.bind("SUPER+3", hl.dsp.focus({ workspace = "3" }))
hl.bind("SUPER+4", hl.dsp.focus({ workspace = "4" }))
hl.bind("SUPER+5", hl.dsp.focus({ workspace = "5" }))
hl.bind("SUPER+6", hl.dsp.focus({ workspace = "6" }))
hl.bind("SUPER+7", hl.dsp.focus({ workspace = "7" }))
hl.bind("SUPER+8", hl.dsp.focus({ workspace = "8" }))
hl.bind("SUPER+9", hl.dsp.focus({ workspace = "9" }))
hl.bind("SUPER+K", hl.dsp.focus({ workspace = "+1" }))
hl.bind("SUPER+U", hl.dsp.focus({ workspace = "-1" }))
hl.bind("SUPER+Tab", hl.dsp.focus({ workspace = "previous" }))
hl.bind("SUPER+mouse_up", hl.dsp.focus({ workspace = "+1" }))
hl.bind("SUPER+mouse_down", hl.dsp.focus({ workspace = "-1" }))

-- Window movement
hl.bind("SUPER+SHIFT+A", hl.dsp.window.swap({ direction = "l" }))
hl.bind("SUPER+SHIFT+E", hl.dsp.window.swap({ direction = "u" }))
hl.bind("SUPER+SHIFT+I", hl.dsp.window.swap({ direction = "d" }))
hl.bind("SUPER+SHIFT+H", hl.dsp.window.swap({ direction = "r" }))
hl.bind("SUPER+mouse:272", hl.dsp.window.drag(), { mouse = true, drag = true })

-- Move to workspace
hl.bind("SUPER+SHIFT+1", hl.dsp.window.move({ workspace = "1" }))
hl.bind("SUPER+SHIFT+2", hl.dsp.window.move({ workspace = "2" }))
hl.bind("SUPER+SHIFT+3", hl.dsp.window.move({ workspace = "3" }))
hl.bind("SUPER+SHIFT+4", hl.dsp.window.move({ workspace = "4" }))
hl.bind("SUPER+SHIFT+5", hl.dsp.window.move({ workspace = "5" }))
hl.bind("SUPER+SHIFT+6", hl.dsp.window.move({ workspace = "6" }))
hl.bind("SUPER+SHIFT+7", hl.dsp.window.move({ workspace = "7" }))
hl.bind("SUPER+SHIFT+8", hl.dsp.window.move({ workspace = "8" }))
hl.bind("SUPER+SHIFT+9", hl.dsp.window.move({ workspace = "9" }))
hl.bind("SUPER+SHIFT+K", hl.dsp.window.move({ workspace = "+1" }))
hl.bind("SUPER+SHIFT+U", hl.dsp.window.move({ workspace = "-1" }))

-- Applications
hl.bind("SUPER+SPACE", hl.dsp.exec_cmd("wofi --show drun"))
hl.bind("SUPER+CTRL+SPACE", hl.dsp.exec_cmd("1password --quick-access"))
hl.bind("SUPER+RETURN", hl.dsp.exec_cmd("ghostty"))
hl.bind("SUPER+B", hl.dsp.exec_cmd("firefox --new-window"))
hl.bind("SUPER+CTRL+D", hl.dsp.exec_cmd("goldendict --popup $(wl-paste --primary --no-newline)"))
hl.bind("SUPER+M", hl.dsp.exec_cmd("emacsclient --create-frame --no-wait --alternate-editor='' --quiet"))
hl.bind("SUPER+CTRL+S", hl.dsp.exec_cmd("hyprshot -m window"))
hl.bind("SUPER+CTRL+SHIFT+S", hl.dsp.exec_cmd("hyprshot -m region"))
hl.bind("SUPER+SLASH", hl.dsp.exec_cmd("firefox --new-window ~/.config/hypr/keybindings.html"))
hl.bind("SUPER+CTRL+SLASH", hl.dsp.exec_cmd("imv -s full -w 'Meow Keybindings' ~/dotfiles/emacs/keymap_meow.svg"))

-- Window resizing
hl.bind("SUPER+CTRL+A", hl.dsp.window.resize({ x = -20, y = 0, relative = true }))
hl.bind("SUPER+CTRL+E", hl.dsp.window.resize({ x = 0, y = -20, relative = true }))
hl.bind("SUPER+CTRL+I", hl.dsp.window.resize({ x = 0, y = 20, relative = true }))
hl.bind("SUPER+CTRL+H", hl.dsp.window.resize({ x = 20, y = 0, relative = true }))
hl.bind("SUPER+mouse:273", hl.dsp.window.resize(), { mouse = true })

-- Layout (Dwindle)
hl.bind("SUPER+F", hl.dsp.window.fullscreen("maximized", "toggle"))
hl.bind("SUPER+CTRL+F", hl.dsp.window.fullscreen("fullscreen", "toggle"))
hl.bind("SUPER+SHIFT+SPACE", hl.dsp.window.float())
hl.bind("SUPER+SHIFT+mouse:272", hl.dsp.window.float())
hl.bind("SUPER+W", hl.dsp.window.kill())
hl.bind("SUPER+P", hl.dsp.window.pin())

-- Scratchpad
hl.bind("SUPER+S", hl.dsp.workspace.toggle_special("magic"))
hl.bind("SUPER+CTRL+T", hl.dsp.exec_cmd("ghostty --title=scratchpad"))

-- Quit Hyprland
hl.bind("SUPER+SHIFT+ESCAPE", hl.dsp.exec_cmd("hyprshutdown"))

--
-- WINDOW RULES
--

hl.window_rule({ match = { class = ".*" }, suppress_event = "maximize" })

hl.window_rule({ match = { class = "^(obsidian)$" }, workspace = "3" })

-- GoldenDict popup: float and centre
hl.window_rule({ match = { class = ".*goldendict_ng$", title = ".* - GoldenDict-ng$" }, float = true })
hl.window_rule({ match = { class = ".*goldendict_ng$", title = ".* - GoldenDict-ng$" }, center = true })
hl.window_rule({ match = { class = ".*goldendict_ng$", title = ".* - GoldenDict-ng$" }, size = { 800, 600 } })

-- Move a terminal with the class "scratchpad" to the special workspace. The
-- "magic" tag corresponds to the keybinding above.
hl.window_rule({
	match = { class = "^(com\\.mitchellh\\.ghostty)$", title = "^(scratchpad)$" },
	workspace = "special:magic",
})
hl.window_rule({ match = { class = "^(com\\.mitchellh\\.ghostty)$", title = "^(scratchpad)$" }, float = true })
hl.window_rule({
	match = { class = "^(com\\.mitchellh\\.ghostty)$", title = "^(scratchpad)$" },
	size = { "monitor_w * 0.5", "monitor_h * 0.5" },
})
hl.window_rule({ match = { class = "^(com\\.mitchellh\\.ghostty)$", title = "^(scratchpad)$" }, center = true })

-- Popup window with the Hyprland keybindings cheatsheet.
-- Note that these rules to make the window float don’t currently work. AIUI,
-- this is due to how Firefox sets the window title. Nevertheless, I’ll leave the
-- rules here in case it happens to start working one day.
hl.window_rule({ match = { class = "^(firefox)$", title = "^(Hyprland Keybindings Reference)$" }, float = true })
hl.window_rule({
	match = { class = "^(firefox)$", title = "^(Hyprland Keybindings Reference)$" },
	size = { "monitor_w * 0.6", "monitor_h * 0.8" },
})
hl.window_rule({ match = { class = "^(firefox)$", title = "^(Hyprland Keybindings Reference)$" }, center = true })

-- Popup window with the Meow keybindings cheatsheet.
hl.window_rule({ match = { class = "^(imv)$", title = "^(Meow Keybindings)$" }, float = true })
hl.window_rule({
	match = { class = "^(imv)$", title = "^(Meow Keybindings)$" },
	size = { "monitor_w * 0.4", "monitor_h * 0.4" },
})
hl.window_rule({ match = { class = "^(imv)$", title = "^(Meow Keybindings)$" }, center = true })

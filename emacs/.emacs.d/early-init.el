;; -*- lexical-binding: t; -*-

;; Activate installed packages now, before anything below needs them
;; -- normally this waits until after early-init.el finishes.
(package-activate-all)

;; Recompile init.el (and any other Elisp we load) whenever its
;; source is newer than its .elc, checked right at load time, so
;; edits made outside Emacs (e.g. `git pull', an external editor)
;; are covered too, not just `save-buffer'. This has to run this
;; early to also protect init.el's own `load' just after early-init
;; finishes. See the matching `use-package auto-compile' block in
;; init.el, which installs the package on a machine that doesn't
;; have it yet -- `require' here is allowed to fail silently for
;; that first run.
(when (require 'auto-compile nil t)
  (auto-compile-on-load-mode 1)
  (auto-compile-on-save-mode 1))

;; Belt and braces: even before auto-compile's first install, or if
;; a compile attempt ever fails, never let a stale .elc silently
;; shadow a newer .el.
(setq load-prefer-newer t)

(setq frame-resize-pixelwise t)
(setq frame-inhibit-implied-resize t)
(setq use-dialog-box t)
(setq use-file-dialog nil)
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)
(setq inhibit-x-resources t)
(setq inhibit-startup-echo-area-message user-login-name)
(setq inhibit-startup-buffer-menu t)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

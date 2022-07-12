;;; early-init --- Initialise installed packages

;;; Commentary:

;;; Code:

;; Initialise installed packages
(setq package-enable-at-startup t)

;; Allow loading from the package cache
(setq package-quickstart t)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; Disable GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq use-dialog-box t)                 ; only for mouse events
(setq use-file-dialog nil)

(setq inhibit-startup-echo-area-message user-login-name) ; read the docstring
(setq inhibit-startup-screen t)
(setq inhibit-startup-buffer-menu t)

(setq native-comp-async-report-warnings-errors 'silent) ; emacs28 with native compilation

;;; early-init.el ends here

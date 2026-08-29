;; -*- lexical-binding: t; -*-

;; Don't pop up a buffer when async native-compilation of a package
;; emits warnings.
(setq native-comp-async-report-warnings-errors 'silent)

;; Never let a stale .elc shadow a newer .el (e.g. a leftover from a
;; past build, or one dropped in by `git pull').
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

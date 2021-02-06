;;; flymake-markdownlint.el --- Flymake backend for markdownlint -*- lexical-binding: t; -*-

;;; Commentary:

;; This package adds support for markdownlint
;; (https://github.com/DavidAnson/markdownlint) to Flymake.

;; Once installed, the backend can be enabled with:
;; (add-hook 'markdown-mode-hook #'flymake-markdownlint-setup)

;;; Code:

(require 'flymake)
(require 'flymake-quickdef)

(flymake-quickdef-backend
  flymake-markdownlint-backend
  :pre-let ((markdownlint-exec (executable-find "markdownlint")))
  :pre-check (unless markdownlint-exec (user-error "Executable markdownlint not found in PATH"))
  :write-type 'pipe
  :proc-form (list markdownlint-exec "-s")
  :search-regexp "^.+:\\([[:digit:]]+\\):\\([[:digit:]]+\\) \\(.+\\)$"
  :prep-diagnostic (let* ((lnum (string-to-number (match-string 1)))
                          (lcol (string-to-number (match-string 2)))
                          (msg (match-string 3))
                          (pos (flymake-diag-region fmqd-source lnum lcol))
                          (beg (car pos))
                          (end (cdr pos)))
                     (list fmqd-source beg end :warning msg)))

;;;###autoload
(defun flymake-markdownlint-setup ()
  "Enable Flymake backend markdownlint."
  (add-hook 'flymake-diagnostic-functions #'flymake-markdownlint-backend nil t))

(provide 'flymake-markdownlint)

;;; flymake-markdownlint.el ends here

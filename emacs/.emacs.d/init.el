;; Emacs Configuration
;; =============================================================================

;; ................................................................. use-package

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(unless package--initialized (package-initialize))

;; Set up `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

;; ............................................................... Base settings

;; Modeline "lighters"
(use-package delight
  :ensure t
  :after use-package)

;; Store customisation settings in a separate file
(use-package cus-edit
  :config
  (setq custom-file "~/.emacs.d/custom.el")

  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))

  (load custom-file))

;; Base typeface configurations
(use-package emacs
  :config
  (setq x-underline-at-descent-line nil)
  (setq underline-minimum-offset 0)
  (setq line-spacing 0.1)
  
  (defconst dap/fixed-pitch-font "Hack"
    "The default fixed-pitch typeface.")

  (defconst dap/fixed-pitch-params ":hintstyle=hintslight"
    "Fontconfig parameters for the fixed-pitch typeface.")

  (defun dap/default-font (family size)
    "Set frame font to FAMILY at SIZE."
    (set-frame-font
     (concat family "-" (number-to-string size) dap/fixed-pitch-params) t t))

  (defun dap/laptop-fonts ()
    "Fonts for the small laptop screen.

Pass desired argument to `dap/font-sizes' for use on my
small laptop monitor."
    (interactive)
    (when window-system
      (dap/default-font dap/fixed-pitch-font 9)))

  (defun dap/desktop-fonts ()
    "Fonts for the larger desktop screen.

Pass desired argument to `dap/font-sizes' for use on my larger
 desktop monitor (external display connected to my laptop)."
    (interactive)
    (when window-system
      (dap/default-font dap/fixed-pitch-font 9)))

  (defun dap/reading-fonts ()
    "Fonts for focused reading sessions."
    (interactive)
    (when window-system
      (dap/default-font dap/fixed-pitch-font 10.5)))

  (defun dap/fonts-per-monitor ()
    "Use font settings based on screen size.

Choose between `dap/laptop-fonts' and `dap/desktop-fonts'
depending on the width of the monitor.  The calculation is based
on the maximum width of my laptop's screen.  So if an external
display is attached, then iit is considered a desktop scenario.

While this function is interactive, it is best to run it with the
`after-init-hook' or perhaps some other event that tracks
monitor-related events."
    (interactive)
    (when window-system
      (if (<= (display-pixel-width) 1600)
          (dap/laptop-fonts)
        (dap/desktop-fonts))))

  :hook (after-init . dap/fonts-per-monitor))

;; ..................................... Selection candidates and search methods

;; wgrep

(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t))

;; ripgrep

(use-package rg
  :ensure t
  :after wgrep
  :config
  (setq rg-group-result t)
  (setq rg-hide-command t)
  (setq rg-show-columns nil)
  (setq rg-show-header t)
  (setq rg-custom-type-aliases nil)
  (setq rg-default-alias-fallback "all")

  (rg-define-search dap/grep-vc-or-dir
    :query ask
    :format regexp
    :files "everything"
    :dir (let ((vc (vc-root-dir)))
           (if vc
               vc                         ; search root project dir
             default-directory))          ; or from the current dir
    :confirm prefix
    :flags ("--hidden -g !.git"))

  (defun dap/rg-save-search-as-name ()
    "Save `rg' buffer, naming it after the current search query.

This function is meant to be mapped to a key in `rg-mode-map'."
    (interactive)
    (let ((pattern (car rg-pattern-history)))
      (rg-save-search-as-name (concat "«" pattern "»"))))

  :bind (("M-s g" . dap/grep-vc-or-dir)
         :map rg-mode-map
         ("s" . dap/rg-save-search-as-name)
         ("C-n" . next-line)
         ("C-p" . previous-line)
         ("M-n" . rg-next-file)
         ("M-p" . rg-prev-file)))

;; ....................................................................... Theme

;; Disable GUI components
(use-package emacs
  :init
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  :config
  (setq use-file-dialog nil)
  (setq use-dialog-box t)		; only for mouse events
  (setq inhibit-splash-screen t)
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z"))
  (global-unset-key (kbd "C-h h")))

;; Themes
(use-package modus-operandi-theme
  :ensure t)

(use-package modus-vivendi-theme
  :ensure t)

(use-package emacs
  :config
  (setq custom-safe-themes t)

  (defun dap/modus-operandi ()
    "Enable some `modus-operandi' variables and load the theme."
    (setq modus-operandi-theme-slanted-constructs t
          modus-operandi-theme-bold-constructs nil
          modus-operandi-theme-visible-fringes t
          modus-operandi-theme-subtle-diffs t
          modus-operandi-theme-distinct-org-blocks nil
          modus-operandi-theme-proportional-fonts nil
          modus-operandi-theme-scale-headings t
          modus-operandi-theme-scale-1 1.05
          modus-operandi-theme-scale-2 1.1
          modus-operandi-theme-scale-3 1.15
          modus-operandi-theme-scale-4 1.2)
    (load-theme 'modus-operandi t))

  (defun dap/modus-vivendi ()
    "Enable some `modus-vivendi' variables and load the theme."
    (setq modus-vivendi-theme-slanted-constructs t
          modus-vivendi-theme-bold-constructs nil
          modus-vivendi-theme-visible-fringes t
          modus-vivendi-theme-subtle-diffs t
          modus-vivendi-theme-distinct-org-blocks nil
          modus-vivendi-theme-proportional-fonts nil
          modus-vivendi-theme-scale-headings t
          modus-vivendi-theme-scale-1 1.05
          modus-vivendi-theme-scale-2 1.1
          modus-vivendi-theme-scale-3 1.15
          modus-vivendi-theme-scale-4 1.2)
    (load-theme 'modus-vivendi t))

  (defcustom dap/modus-themes-toggle-hook nil
    "Hook that runs after `dap/modus-themes-toggle' is invoked."
    :type 'hook)

  (defun dap/modus-themes-toggle ()
    "Toggle between `modus-operandi' and `modus-vivendi' themes.

Also run `dap/modus-themes-toggle-hook'."
    (interactive)
    (if (eq (car custom-enabled-themes) 'modus-operandi)
        (dap/modus-vivendi)
      (dap/modus-operandi))
    (run-hooks 'dap/modus-themes-toggle-hook))

  (defun dap/describe-face (pos)
    "Print message with name of face at point.

A lightweight alternative to `what-cursor-position' and its
prefixed variant."
    (interactive "d")
    (let ((face (or (get-char-property pos 'face)
                    (get-char-property pos 'read-cf-name))))
      (message "%s" (or face
                        "No face at point"))))

  :bind (("<f5>" . dap/modus-themes-toggle)
         ("s-=" . dap/describe-face))
  :hook (after-init . dap/modus-operandi))

;; ................................................................... Mode line

(use-package emacs
  :config
  (setq mode-line-percent-position '(-3 "%p"))
  (setq mode-line-defining-kbd-macro
        (propertize "Macro" 'face 'mode-line-emphasis))
  (setq-default mode-line-format
                '("%e"
                  mode-line-front-space
                  mode-line-mule-info
                  mode-line-client
                  mode-line-modified
                  mode-line-remote
                  mode-line-frame-identification
                  mode-line-buffer-identification
                  "  "
                  mode-line-position
                  " "
                  mode-line-modes
                  " "
                  mode-line-misc-info
                  mode-line-end-spaces)))

(use-package battery
  :config
  (setq battery-mode-line-format " [%b%p%%]")
  (setq battery-mode-line-limit 97)
  (setq battery-update-interval 180)
  (setq battery-load-low 20)
  (setq battery-load-critical 10)
  :hook (after-init . display-battery-mode))

(use-package time
  :config
  (setq display-time-format "%Y-%m-%d  %H:%M")
  ;;;; Covered by `display-time-format'
  ;; (setq display-time-24hr-format t)
  ;; (setq display-time-day-and-date t)
  (setq display-time-interval 60)
  (setq display-time-mail-directory nil)
  (setq display-time-default-load-average nil)
  :hook (after-init . display-time-mode))

;; .............................................. Window elements and indicators

(use-package emacs
  :config
  (setq window-divider-default-right-width 1)
  (setq window-divider-default-bottom-width 1)
  (setq window-divider-default-places 'right-only)
  :hook (after-init . window-divider-mode))

(use-package fringe
  :config
  (fringe-mode '(8 . 8))
  (setq-default fringes-outside-margins nil)
  (setq-default indicate-buffer-boundaries nil)
  (setq-default indicate-empty-lines nil)
  (setq-default overflow-newline-into-fringe t))

(use-package diff-hl
  :ensure t
  :after vc
  :config
  (setq diff-hl-draw-borders nil)
  (setq diff-hl-side 'left)
  :hook ((after-init . global-diff-hl-mode)))

(use-package hl-line
  :config
  (setq hl-line-sticky-flag nil))

(use-package emacs
  :config
  (defun dap/toggle-invisibles ()
    "Toggles the display of indentation and space characters."
    (interactive)
    (if (bound-and-true-p whitespace-mode)
        (whitespace-mode -1)
      (whitespace-mode)))

  (defun dap/toggle-line-numbers ()
    "Toggles the display of line numbers.  Applies to all buffers."
    (interactive)
    (if (bound-and-true-p display-line-numbers-mode)
        (display-line-numbers-mode -1)
      (display-line-numbers-mode)))
  :bind (("<f6>" . dap/toggle-invisibles)
         ("<f7>" . dap/toggle-line-numbers)))

(use-package olivetti
  :ensure t
  :delight
  :config
  (setq olivetti-body-width 100)
  (setq olivetti-minimum-body-width 72)
  (setq olivetti-recall-visual-line-mode-entry-state t)

  (defun dap/toggle-olivetti-mode ()
    "Toggle `olivetti-mode' without fringes and larger fonts."
    (interactive)
    (if olivetti-mode
        (progn
          (olivetti-mode -1)
          (fringe-mode '(8 . 8))
          (dap/fonts-per-monitor))
      (olivetti-mode 1)
      (fringe-mode '(0 . 0))
      (dap/reading-fonts)))
  :bind ("C-c o" . dap/toggle-olivetti-mode))

(use-package rainbow-blocks
  :ensure t
  :delight
  :commands rainbow-blocks-mode
  :config
  (setq rainbow-blocks-highlight-braces-p t)
  (setq rainbow-blocks-highlight-brackets-p t)
  (setq rainbow-blocks-highlight-parens-p t))

;; ........................................ Language settings for prose and code

(use-package emacs
  :config
  (setq-default fill-column 72)
  (setq sentence-end-double-space t)
  (setq sentence-end-without-period nil)
  (setq colon-double-space nil)
  :hook (after-init . column-number-mode))

(use-package subword
  :delight
  :hook (prog-mode . subword-mode))

(use-package emacs
  :hook (text-mode . (lambda ()
                       (turn-on-auto-fill)
                       (delight 'auto-fill-function nil t)
                       (setq adaptive-fill-mode t))))

(use-package newcomment
  :config
  (setq comment-empty-lines t)
  (setq comment-fill-column nil)
  (setq comment-multi-line t)
  (setq comment-style 'multi-line)

  (defun dap/comment-dwim (&optional arg)
    "Alternative to `comment-dwim': offers a simple wrapper
around `comment-line' and `comment-dwim'.

If the region is active, then toggle the comment status of the
region or, if the major mode defines as much, of all the lines
implied by the region boundaries.

Else toggle the comment status of the line at point."
    (interactive "*P")
    (if (use-region-p)
        (comment-dwim arg)
      (save-excursion
        (comment-line arg))))

  :bind (("C-;" . dap/comment-dwim)
         ("C-:" . comment-kill)
         ("M-;" . comment-indent)
         ("C-x C-;" . comment-box)))

(use-package flyspell
  :commands (ispell-change-dictionary
             ispell-word
             flyspell-buffer
             flyspell-mode
             flyspell-region)
  :config
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  (setq ispell-program-name "aspell")
  (setq ispell-dictionary "en_GB")

  (defun dap/ispell-toggle-dictionaries ()
    "Toggle between English and French dictionaries."
    (interactive)
    (if (string= ispell-current-dictionary "en")
        (ispell-change-dictionary "fr")
      (ispell-change-dictionary "en")))

  (defun dap/flyspell-dwim (&optional beg end)
    "Run `flyspell-region' on the active region, else toggle the
ispell dictionaries with `dap/ispell-toggle-dictionaries'."
    (interactive "r")
    (if (use-region-p)
        (flyspell-region beg end)
      (dap/ispell-toggle-dictionaries)))

  :bind (("M-$" . dap/flyspell-dwim)
         ("C-M-$" . dap/ispell-toggle-dictionaries)
         :map flyspell-mode-map
         ("C-;" . nil)))

(use-package flycheck
  :ensure t
  :commands flycheck-mode
  :config
  (setq flycheck-check-syntax-automatically
        '(save mode-enabled)))

(use-package flycheck-indicator
  :ensure t
  :after flycheck
  :hook (flycheck-mode . flycheck-indicator-mode))

(use-package eldoc
  :config
  (global-eldoc-mode -1))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode))

(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

(use-package emacs
  :mode (("sxhkdrc" . conf-mode)
         ("Xmodmap" . conf-xdefaults-mode)))

;; Configure 'electric' behaviour

(use-package electric
  :config
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  (setq electric-pair-preserve-balance t)
  (setq electric-pair-pairs
        '((8216 . 8217)
          (8220 . 8221)
          (171 . 187)))
  (setq electric-pair-skip-self 'electric-pair-default-skip-self)
  (setq electric-pair-skip-whitespace nil)
  (setq electric-pair-skip-whitespace-chars
        '(9
          10
          32))
  (setq electric-quote-context-sensitive t)
  (setq electric-quote-paragraph t)
  (setq electric-quote-string nil)
  (setq electric-quote-replace-double t)
  :hook (after-init . (lambda ()
                        (electric-indent-mode 1)
                        (electric-pair-mode -1)
                        (electric-quote-mode -1))))

;; Paretheses

(use-package paren
  :config
  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-in-periphery t)
  (setq show-paren-when-point-inside-paren nil)
  :hook (after-init . show-paren-mode))

;; Tabs, indentation, and the TAB key

;; I believe tabs, in the sense of inserting the tab character, are best
;; suited for indentation. While spaces are superior at precisely
;; aligning text. However, I understand that elisp uses its own
;; approach, which I do not want to interfere with. Also, Emacs tends to
;; perform alignments by mixing tabs with spaces, which can actually
;; lead to misalignments depending on certain variables such as the size
;; of the tab. As such, I am disabling tabs by default.

;; If there ever is a need to use different settings in other modes, we
;; can customise them via hooks. This is not an issue I have encountered
;; yet and am therefore refraining from solving a problem that does not
;; affect me.

(use-package emacs
  :config
  (setq-default tab-always-indent t)
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil))

;; ............................................................ Custom movements

;; Mark by semantic unit

(use-package expand-region
  :ensure t
  :pin gnu                              ; Prefer ELPA version
  :config
  (setq expand-region-smart-cursor t)
  :bind (("C-=" . er/expand-region)
         ("C-M-=" . er/mark-outside-pairs)
         ("C-+" . er/mark-symbol)))

;; Go to last change

(use-package goto-last-change
  :ensure t
  :bind ("C-z" . goto-last-change))

;; ............................................................. Version control

;; VC

(use-package vc-dir
  :config
  (defun dap/vc-dir-project ()
    "Unconditionally display `vc-diff' for the current project."
    (interactive)
    (vc-dir (vc-root-dir)))

  (defun dap/vc-dir-jump ()
    "Jump to present directory in a `vc-dir' buffer."
    (interactive)
    (vc-dir default-directory))

  :bind (("C-x v p" . dap/vc-dir-project)
         ("C-x v j" . dap/vc-dir-jump))) ; similar to `dired-jump'

(use-package vc-git
  :config
  (setq vc-git-diff-switches "--patch-with-stat")
  (setq vc-git-print-log-follow t))

(use-package vc-annotate
  :config
  (setq vc-annotate-display-mode 'scale) ; scale to oldest
  :bind (("C-x v a" . vc-annotate)       ; `vc-update-change-log' is not in git
         :map vc-annotate-mode-map
         ("t" . vc-annotate-toggle-annotation-visibility)))

(use-package diff
  :config
  (setq diff-default-read-only t)
  (setq diff-advance-after-apply-hunk t)
  (setq diff-update-on-the-fly t))

(use-package log-edit
  :config
  (setq log-edit-confirm 'changed)
  (setq log-edit-keep-buffer nil)
  (setq log-edit-require-final-newline t)
  (setq log-edit-setup-add-author nil))

;; .................................................................... Org mode

(use-package org
  :config
  ;; agenda and basic directory structure
  (setq org-directory "~/org")
  (setq org-default-notes-file "~/org/notes.org")
  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k nil)
  (setq org-catch-invisible-edits 'show)
  (setq org-hide-emphasis-markers t)
  (setq org-return-follows-link nil)
  (setq org-fontify-done-headline t)
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-track-ordered-property-with-tag t)
    ;; log
  (setq org-log-done 'time)
  (setq org-log-note-clock-out nil)
  (setq org-log-redeadline nil)
  (setq org-log-reschedule nil)
  (setq org-reverse-note-order t)
  (setq org-read-date-prefer-future 'time))

;; ....................................................................... Email

;; Base email settings

(use-package auth-source
  :config
  (setq auth-sources '("~/.authinfo.gpg" "~/.authinfo"))
  (setq user-full-name "David Porter")
  (setq user-mail-address "david@daporter.net"))

(use-package epa-file
  :config
  (setq epa-file-cache-passphrase-for-symmetric-encryption t)
  (setq epa-pinentry-mode 'loopback))

(use-package message
  :config
  (setq message-directory "~/mail/")
  (setq mail-user-agent 'message-user-agent)
  (setq compose-mail-user-agent-warnings nil)
  (setq message-mail-user-agent nil)    ; default is `gnus'
  (setq message-citation-line-format "%f [%Y-%m-%d, %R %z]:\n")
  (setq message-citation-line-function
        'message-insert-formatted-citation-line)
  (setq message-confirm-send nil)
  (setq message-kill-buffer-on-exit t)
  (setq message-wide-reply-confirm-recipients t)
  (setq message-default-charset 'utf-8)
  (add-to-list 'mm-body-charset-encoding-alist '(utf-8 . base64))

  (defun dap/message-header-add-gcc ()
    "While `gnus' is running, add a Gcc header, if missing.

The Gcc header places a copy of the outgoing message to the
appropriate maildir directory.

In the absence of a Gcc header, the outgoing message will not
appear in the appropriate directory, though it will still be
sent.

Add this function to `message-header-setup-hook'."
    (if (gnus-alive-p)
        (progn
          (when (message-fetch-field "Gcc")
            (message-remove-header "Gcc")
            (message-add-header "Gcc: nnimap+migadu:Sent")))
      (message "Gnus is not running. No GCC field inserted.")))

  :hook ((message-header-setup . dap/message-header-add-gcc)
         (message-setup . message-sort-headers)))

;; Gnus

(use-package gnus
  :config
  (setq gnus-directory "~/news")
  ;; accounts
  (setq gnus-select-method '(nnnil))
  (setq gnus-secondary-select-methods
        '((nntp "news.gwene.org")
          (nnimap "migadu"
                  (nnimap-address "imap.migadu.com"
                                  (nnimap-stream ssl)
                                  (nnimap-authinfo-file "~/.authinfo.gpg")))))

  (setq gnus-parameters '(("migadu" (posting-style
                                     (gcc "nnimap+migadu:Sent")))))
  (setq gnus-gcc-mark-as-read t)
  (setq gnus-agent t)
  (setq gnus-novice-user nil)
  ;; checking sources
  (setq gnus-check-new-newsgroups 'ask-server)
  (setq gnus-read-active-file 'some)
  ;; dribble
  (setq gnus-use-dribble-file t)
  (setq gnus-always-read-dribble-file t)
  :bind ("C-c m" . gnus))

(use-package nnmail
  :config
  (setq nnmail-expiry-wait 30))

(use-package mm-encode
  :config
  (setq mm-encrypt-option 'guided))

(use-package mml-sec
  :config
  (setq mml-secure-openpgp-encrypt-to-self t)
  (setq mml-secure-openpgp-sign-with-sender t)
  (setq mml-secure-smime-encrypt-to-self t)
  (setq mml-secure-smime-sign-with-sender t))

(use-package gnus-agent
  :after gnus
  :config
  (setq gnus-agent-article-alist-save-format 1)  ; uncompressed
  (setq gnus-agent-cache t)
  (setq gnus-agent-confirmation-function 'y-or-n-p)
  (setq gnus-agent-consider-all-articles nil)
  (setq gnus-agent-directory "~/news/agent/")
  (setq gnus-agent-enable-expiration 'ENABLE)
  (setq gnus-agent-expire-all nil)
  (setq gnus-agent-expire-days 30)
  (setq gnus-agent-mark-unread-after-downloaded t)
  (setq gnus-agent-queue-mail t)        ; queue if unplugged
  (setq gnus-agent-synchronize-flags nil))

(use-package gnus-art
  :after gnus
  :demand
  :config
  (setq gnus-article-browse-delete-temp 'ask)
  (setq gnus-article-over-scroll nil)
  (setq gnus-article-show-cursor t)
  (setq gnus-article-sort-functions
        '((not gnus-article-sort-by-number)
          (not gnus-article-sort-by-date)))
  (setq gnus-article-truncate-lines nil)
  (setq gnus-html-frame-width 80)
  (setq gnus-html-image-automatic-caching t)
  (setq gnus-inhibit-images t)
  (setq gnus-max-image-proportion 0.3)
  (setq gnus-treat-display-smileys nil)
  (setq gnus-article-mode-line-format "%G %S %m")
  (setq gnus-visible-headers
        '("^From:" "^To:" "^Cc:" "^Newsgroups:" "^Subject:" "^Date:"
          "Followup-To:" "Reply-To:" "^Organization:" "^X-Newsreader:"
          "^X-Mailer:"))
  (setq gnus-sorted-header-list gnus-visible-headers)
  :bind (:map gnus-article-mode-map
              ("s" . gnus-mime-save-part)
              ("o" . gnus-mime-copy-part)))

(use-package gnus-async
  :after gnus
  :config
  (setq gnus-asynchronous t)
  (setq gnus-use-article-prefetch 15))

(use-package gnus-group
  :after gnus
  :demand
  :config
  (setq gnus-level-subscribed 6)
  (setq gnus-level-unsubscribed 7)
  (setq gnus-level-zombie 8)
  (setq gnus-list-groups-with-ticked-articles nil)
  (setq gnus-group-sort-function
        '((gnus-group-sort-by-unread)
          (gnus-group-sort-by-alphabet)
          (gnus-group-sort-by-rank)))
  (setq gnus-group-mode-line-format "%%b")
  :hook
  (gnus-group-mode . hl-line-mode)
  (gnus-select-group-hook . gnus-group-set-timestamp)
  :bind (:map gnus-agent-group-mode-map
              ("M-n" . gnus-topic-goto-next-topic)
              ("M-p" . gnus-topic-goto-previous-topic)))

(use-package gnus-topic
  :after (gnus gnus-group)
  :config
  (setq gnus-topic-display-empty-topics nil)
  :hook
  (gnus-group-mode . gnus-topic-mode))

(use-package gnus-sum
  :after (gnus gnus-group)
  :demand
  :config
  (setq gnus-auto-select-first nil)
  (setq gnus-summary-ignore-duplicates t)
  (setq gnus-suppress-duplicates t)
  (setq gnus-summary-goto-unread nil)
  (setq gnus-summary-make-false-root 'adopt)
  (setq gnus-summary-thread-gathering-function
        'gnus-gather-threads-by-subject)
  (setq gnus-thread-sort-functions
        '(gnus-thread-sort-by-number
          gnus-thread-sort-by-date))
  (setq gnus-subthread-sort-functions
        'gnus-thread-sort-by-date)
  (setq gnus-thread-hide-subtree nil)
  (setq gnus-thread-ignore-subject nil)
  (setq gnus-user-date-format-alist
        '(((gnus-seconds-today) . "Today at %R")
          ((+ 86400 (gnus-seconds-today)) . "Yesterday, %R")
          (t . "%Y-%m-%d %R")))
  (setq gnus-summary-line-format
        "%U%R%z %-16,16&user-date;  %4L:%-30,30f  %B%S\n")
  (setq gnus-summary-mode-line-format "%p")
  (setq gnus-sum-thread-tree-false-root "─┬➤ ")
  (setq gnus-sum-thread-tree-indent "  ")
  (setq gnus-sum-thread-tree-leaf-with-other "├─➤ ")
  (setq gnus-sum-thread-tree-root "")
  (setq gnus-sum-thread-tree-single-leaf "└─➤ ")
  (setq gnus-sum-thread-tree-vertical "│")
  :hook
  (gnus-summary-mode . hl-line-mode)
  (gnus-summary-exit-hook . gnus-topic-sort-groups-by-alphabet)
  (gnus-summary-exit-hook . gnus-group-sort-groups-by-rank)
  :bind (:map gnus-agent-summary-mode-map
              ("<delete>" . gnus-summary-delete-article)
              ("n" . gnus-summary-next-article)
              ("p" . gnus-summary-prev-article)
              ("N" . gnus-summary-next-unread-article)
              ("P" . gnus-summary-prev-unread-article)
              ("M-n" . gnus-summary-next-thread)
              ("M-p" . gnus-summary-prev-thread)
              ("C-M-n" . gnus-summary-next-group)
              ("C-M-p" . gnus-summary-prev-group)
              ("C-M-^" . gnus-summary-refer-thread)))

(use-package gnus-srvr
  :after gnus
  :hook
  ((gnus-browse-mode gnus-server-mode) . hl-line-mode))

(use-package gnus-dired
  :after (gnus dired)
  :hook (dired-mode . gnus-dired-mode))

;; Elfeed

(use-package elfeed
  :ensure t
  :commands elfeed
  :config
  (setq elfeed-use-curl t)
  (setq elfeed-curl-max-connections 10)
  (setq elfeed-db-directory "~/.emacs.d/elfeed")
  (setq elfeed-enclosure-default-dir "~/downloads")
  (setq elfeed-search-clipboard-type 'CLIPBOARD)
  (setq elfeed-search-title-max-width (current-fill-column))
  (setq elfeed-search-title-max-width 100)
  (setq elfeed-search-title-min-width 30)
  (setq elfeed-search-trailing-width 16)
  (setq elfeed-show-truncate-long-urls t)
  (setq elfeed-show-unique-buffers t)
  (setq elfeed-sort-order 'ascending)

  (defun dap/feeds ()
    "Loads a file with RSS/Atom feeds.  This file contains valid
syntax for use by the `elfeed' package."
    (let ((feeds "~/.emacs.d/feeds.el.gpg"))
      (when (file-exists-p feeds)
        (message "* loading feeds")
        (load-file feeds))))

  (defun ambrevar/elfeed-play-with-mpv ()
    "Play entry link with mpv."
    (interactive)
    (let ((entry (if (eq major-mode 'elfeed-show-mode)
                     elfeed-show-entry (elfeed-search-selected :single)))
          (quality-arg "")
          (quality-val (completing-read "Resolution: "
                                        '("480" "720" "1080")
                                        nil nil)))
      (setq quality-val (string-to-number quality-val))
      (message "Opening %s with height≤%s..."
               (elfeed-entry-link entry) quality-val)
      (when (< 0 quality-val)
        (setq quality-arg
              (format "--ytdl-format=[height<=?%s]" quality-val)))
      (start-process "elfeed-mpv" nil "mpv"
                     quality-arg (elfeed-entry-link entry))))
  
  :hook (after-init . dap/feeds)
  :bind (:map elfeed-search-mode-map
              ("v" . (lambda ()
                       (interactive)
                       (ambrevar/elfeed-play-with-mpv)
                       (elfeed-search-untag-all-unread)))
              ("w" . elfeed-search-yank)
              ("g" . elfeed-update)
              ("G" . elfeed-search-update--force)
              :map elfeed-show-mode-map
              ("v" . ambrevar/elfeed-play-with-mpv)
              ("w" . elfeed-show-yank)))

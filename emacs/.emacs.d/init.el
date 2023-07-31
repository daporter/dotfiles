(require 'use-package)
;;(setq use-package-compute-statistics t)

(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.  We display
  ;; [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple
              :filter-args #'crm-indicator)

  :custom
  (initial-buffer-choice t)             ; always start with *scratch*
  (frame-title-format '("%b"))
  (use-short-answers t)

  ;; Improve the spacing of underlines.
  (x-use-underline-position-properties nil)

  (backup-directory-alist
   `(("." . ,(concat user-emacs-directory "backup/"))))
  (backup-by-copying t)
  (version-control t)
  (delete-old-versions t)
  (kept-new-versions 6)
  (create-lockfiles nil)

  (window-divider-default-right-width 2)
  (window-divider-default-bottom-width 2)
  (window-divider-default-places t)

  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; Prefer tree-sitter-enabled modes.
  (major-mode-remap-alist
   '((bash-mode . bash-ts-mode)
     (c-mode . c-ts-mode)
     (c++-mode . c++-ts-mode)
     (css-mode . css-ts-mode)
     (js2-mode . js-ts-mode)
     (json-mode . json-ts-mode)
     (python-mode . python-ts-mode)
     (typescript-mode . typescript-ts-mode)
     (yaml-mode . yaml-ts-mode)))

  ;; Emacs 28: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers.
  (read-extended-command-predicate
   #'command-completion-default-include-p)

  (isearch-lazy-count t)
  (lazy-count-prefix-format "(%s/%s) ")
  (lazy-count-suffix-format nil)

  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (enable-recursive-minibuffers t)
  (send-mail-function 'smtpmail-send-it)

  (user-full-name "David Porter")
  (mail-host-address "daporter.net")

  (custom-file (concat user-emacs-directory "emacs-custom.el"))

  :config
  (dolist (cmd '(upcase-region
                 downcase-region
                 narrow-to-region
                 set-goal-column))
    (put cmd 'disabled nil))

  ;; Display windows with 2 columns of margin.
  (setq-default left-margin-width 2)
  (setq-default right-margin-width 2)

  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (add-hook 'package-menu-mode-hook #'hl-line-mode)
  (server-start)
  (repeat-mode 1)
  (column-number-mode 1)
  (delete-selection-mode 1)
  (savehist-mode 1)
  (electric-pair-mode 1)
  (electric-quote-mode 1)
  (fringe-mode 0)
  (window-divider-mode 1)
  (winner-mode 1)
  (auto-insert-mode t)

  (load custom-file)

  (global-set-key (kbd "C-z") 'undo)
  (global-set-key (kbd "C-S-z") 'undo-redo)
  (global-set-key (kbd "M-o") 'other-window)
  (global-set-key (kbd "s-b") 'switch-to-buffer)
  (global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)
  (global-set-key (kbd "C-M-?") 'hippie-expand)

  (defun my/insert-date-time (prefix)
    "Insert the current date and time.
  With PREFIX, use `ID' format, e.g. 20230323113431."
    (interactive "P")
    (let ((format (if (equal prefix '(4))
                      "%Y%m%d%H%M%S"
                    "%Y-%m-%d %H:%M:%S")))
      (insert (format-time-string format))))
  (global-set-key (kbd "C-c d") #'my/insert-date-time)

  ;; The following functions were copied from
  ;; https://git.sr.ht/~protesilaos/dotfiles/tree/master/item/emacs/.emacs.d/prot-lisp/prot-simple.el

  (defun my/simple-new-line-above (&optional arg)
    "Create an empty line above the current one.
Move the point to the absolute beginning.  Adapt indentation by
passing optional prefix ARG (\\[universal-argument])."
    (interactive "P")
    (let ((indent (or arg nil)))
      (if (or (bobp)
              (line-number-at-pos (point-min)))
          (progn
            (beginning-of-line)
            (newline)
            (forward-line -1))
        (forward-line -1)
        (my/simple-new-line-below indent))))
  (global-set-key (kbd "<C-S-return>") #'my/simple-new-line-above)

  (defun my/simple-new-line-below (&optional arg)
    "Create an empty line below the current one.
Move the point to the absolute beginning.  Adapt indentation by
passing optional prefix ARG (\\[universal-argument]).  Also see
`my/simple-new-line-above'."
    (interactive "P")
    (end-of-line)
    (if arg
        (newline-and-indent)
      (newline)))
  (global-set-key (kbd "<C-return>") #'my/simple-new-line-below)

  (global-set-key (kbd "<C-return>") #'my/simple-new-line-below))

(use-package whole-line-or-region
  :ensure t)

(use-package dired
  :custom
  (dired-recursive-copies 'always)
  (dired-dwim-target t))                ; try to guess target directory for copy

(use-package package
  :defer t
  :init
  (add-hook 'package-menu-mode-hook #'hl-line-mode)
  :custom
  (package-archives
   '(("elpa" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")))

  ;; Highest number gets priority (what is not mentioned has
  ;; priority 0)
  (package-archive-priorities
   '(("elpa" . 2)
     ("nongnu" . 1))))

(use-package modus-themes
  :ensure t
  :init
  (require 'modus-themes)
  (setq modus-themes-bold-constructs   t)
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-mixed-fonts       t)
  (setq modus-themes-prompts           '(semibold))
  (setq modus-themes-variable-pitch-ui t)
  (setq modus-themes-headings          '((1 . (light variable-pitch 1.5))))

  (setq modus-themes-common-palette-overrides
        `((comment                   yellow-faint)
          (bg-region                 bg-blue-subtle)
          (bg-completion             bg-inactive)
          (fg-mode-line-active       fg-main)
          (bg-mode-line-active       bg-dim)
          (border-mode-line-active   bg-dim)
          (fg-mode-line-inactive     border)
          (bg-mode-line-inactive     bg-dim)
          (border-mode-line-inactive bg-inactive)
          ,@modus-themes-preset-overrides-faint))

  (defun my/modus-themes-pad-mode-line ()
    "Pad mode-line via a box that has the background color"
    (modus-themes-with-colors
      (custom-set-faces
       `(hl-line
         ((,c :background ,bg-cyan-nuanced)))
       `(mode-line
         ((,c :box (:line-width 5 :color ,bg-mode-line-active))))
       `(mode-line-inactive
         ((,c :box (:line-width 5 :color ,bg-mode-line-inactive))))
       `(window-divider ((,c :foreground ,border)))
       `(window-divider-first-pixel ((,c :foreground ,border)))
       `(window-divider-last-pixel ((,c :foreground ,border)))
       `(fill-column-indicator ((,c :background ,bg-inactive)))
       `(auto-dim-other-buffers-face ((,c :background ,bg-dim))))))
  (add-hook 'modus-themes-after-load-theme-hook
            #'my/modus-themes-pad-mode-line)

  (setq modus-themes-to-toggle '(modus-operandi modus-vivendi))

  ;; Defer loading the theme until Emacs in initialised
  (defun my/modus-themes-init ()
    (load-theme (car modus-themes-to-toggle))
    (my/modus-themes-pad-mode-line))

  :hook (after-init . my/modus-themes-init))

(use-package fontaine
  :ensure t
  :defer t
  :init
  (setq fontaine-latest-state-file
        (locate-user-emacs-file "fontaine-latest-state.eld"))
  (setq fontaine-presets
        '((regular
           :default-family "Iosevka Term"
           :fixed-pitch-family "Iosevka Slab Extended"
           :fixed-pitch-height 0.9
           :fixed-pitch-serif-family "Iosevka Slab Extended"
           :fixed-pitch-serif-height 0.9
           :variable-pitch-family "Elstob"
           :variable-pitch-height 1.15)))
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)
  (fontaine-set-preset
   (or (fontaine-restore-latest-preset) 'regular)))

(use-package auto-dim-other-buffers
  :ensure t
  :after modus-themes
  :init
  (auto-dim-other-buffers-mode 1))

(use-package apropos
  :defer t
  :custom
  (apropos-sort-by-scores t))

(use-package ispell
  :defer t
  :custom (ispell-dictionary "australian-w_accents"))

(use-package bookmark
  :defer t
  :custom
  ;; Save bookmarks on each modification.
  (bookmark-save-flag 1))

(use-package saveplace
  :defer t
  :init (save-place-mode 1))

(use-package savehist
  :defer t
  :init (savehist-mode 1))

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map ("M-m" . marginalia-cycle))
  :init (marginalia-mode 1))

(use-package orderless
  :ensure t
  :defer t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

(use-package vertico
  :ensure t
  :defer t
  :init
  (vertico-mode 1)
  :custom
  (add-hook 'rfn-eshdadow-update-overlay-hook #'vertico-directory-tidy))

(use-package corfu
  :ensure t
  :defer t
  :bind (:map corfu-map
              ("SPC" . corfu-insert-separator)) ; to work well with orderless
  :init
  (global-corfu-mode 1)
  (corfu-echo-mode 1)
  (corfu-history-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package cape
  :ensure t
  :defer t
  :bind ("C-c p i" . cape-ispell))

(use-package lin
  :ensure t
  :init
  (lin-global-mode 1))

(use-package vterm
  :ensure t
  :commands vterm)

(use-package whitespace
  :defer t
  :custom
  (whitespace-style '(face
                      trailing
                      tabs
                      spaces
                      lines-tail
                      newline
                      missing-newline-at-eof
                      empty
                      indentation
                      space-before-tab
                      space-mark
                      tab-mark
                      newline-mark))
  (whitespace-line-column nil)
  (add-to-list 'write-file-functions #'delete-trailing-whitespace))

(use-package smart-tabs-mode
  :ensure t
  :commands smart-tabs-mode
  :config
  (smart-tabs-add-language-support sh sh-mode-hook
    ((smie-indent-line . sh-basic-offset)))
  (smart-tabs-add-language-support mhtml mhtml-mode-hook
    ((mhtml-indent-line . sgml-basic-offset)))
  (smart-tabs-add-language-support css css-mode-hook
    ((smie-indent-line . css-indent-offset)))
  (smart-tabs-insinuate 'sh 'mhtml 'nxml 'css))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package titlecase
  :ensure t
  :commands (titlecase-region
             titlecase-line
             titlecase-sentence
             titlecase-dwim)
  :custom (titlecase-style 'chicago))

(use-package flymake
  :defer t
  :hook ((prog-mode text-mode) . flymake-mode-on)
  :bind (:map flymake-mode-map
              ("M-p" . flymake-goto-prev-error)
              ("M-n" . flymake-goto-next-error))
  :custom
  (flymake-fringe-indicator-position nil))

(use-package eglot
  :ensure t
  :commands (eglot eglot-ensure))

(use-package prog-mode
  :defer t
  :init
  (defun my/set-fill-column ()
    (setq-local fill-column 80))
  (defun my/turn-on-display-fill-column-indicator-mode ()
    (display-fill-column-indicator-mode 1))
  (add-hook 'prog-mode-hook #'my/set-fill-column)
  (add-hook 'prog-mode-hook #'my/turn-on-display-fill-column-indicator-mode))

(use-package text-mode
  :defer t
  :config
  ;; For some reason, the following doesn't work with :bind
  (define-key text-mode-map (kbd "C-M-i") #'completion-at-point)
  (define-key text-mode-map (kbd "C-c P") #'repunctuate-sentences)
  (defun my/disable-indent-tabs-mode ()
    (setq-local indent-tabs-mode nil))
  (defun my/configure-capfs-text-mode ()
    (require 'cape)
    (setq-local completion-at-point-functions
                '(cape-file cape-dabbrev cape-ispell)))
  (add-hook 'text-mode-hook #'my/disable-indent-tabs-mode)
  (add-hook 'text-mode-hook #'my/configure-capfs-text-mode))

(use-package flymake-vale
  :load-path "lisp/flymake-vale"
  :commands flymake-vale-load)

(use-package visual-line-mode
  :hook (text-mode . visual-line-mode))

(use-package visual-fill-column
  :ensure t
  :hook visual-line-mode)

(use-package adaptive-wrap
  :ensure t
  :hook (visual-line-mode . adaptive-wrap-prefix-mode))

(use-package unfill
  :ensure t
  :commands (unfill-region unfill-paragraph)
  :bind ("C-c q" . unfill-paragraph))

(use-package lorem-ipsum
  :ensure t
  :defer t)

(use-package elisp-mode
  :defer t
  :config
  (defun my/configure-capfs-emacs-lisp-mode ()
    (require 'cape)
    (setq-local completion-at-point-functions
                (list (cape-super-capf #'elisp-completion-at-point
                                       #'cape-dabbrev)
                      #'cape-ispell)))
  (add-hook 'emacs-lisp-mode-hook #'my/disable-indent-tabs-mode)
  (add-hook 'emacs-lisp-mode-hook #'my/configure-capfs-emacs-lisp-mode))

(use-package c-ts-mode
  :after eglot
  :defer t
  :init
  (add-hook 'c-ts-mode-hook 'eglot-ensure)
  :custom
  (c-ts-mode-indent-style "linux")
  (comment-style 'extra-line))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode))
  :custom
  (markdown-command "pandoc")
  :config
  (define-auto-insert
    "/.*/Dropbox/Reference/.*\\.md\\'"
    '(nil "---" n
          "title: " (capitalize (file-name-base buffer-file-name)) n
          "uid: " (format-time-string "%Y%m%d%H%M%S") n
          "created: " (format-time-string "%Y-%m-%d %H:%M:%S") n
          "---\n\n")))

(use-package flymake-markdownlint
  :ensure t
  :hook (markdown-mode . flymake-markdownlint-setup))

(use-package sh-script
  :defer t
  :config
  (defun my/configure-tab-width-sh-mode ()
    (setq tab-width sh-basic-offset))
  (add-hook 'sh-mode-hook #'my/configure-tab-width-sh-mode)

  (use-package whitespace)
  (defun my/configure-whitespace-sh-mode ()
    (setq-local whitespace-style
                (remove 'indentation whitespace-style)))
  (add-hook 'whitespace-mode-hook #'my/configure-whitespace-sh-mode)

  (defun my/configure-capfs-sh-mode ()
    (require 'cape)
    (setq-local completion-at-point-functions
                (list #'cape-file
                      (cape-super-capf #'sh-completion-at-point-function
                                       #'cape-dabbrev)
                      #'cape-ispell)))
  (add-hook 'sh-mode-hook #'my/configure-capfs-sh-mode))

(use-package python-ts-mode
  :ensure t
  :defer t
  :config
  (defun my/configure-tab-width-python-ts-mode ()
    (setq tab-width python-indent-offset))
  (add-hook 'python-ts-mode-hook #'my/configure-tab-width-python-ts-mode)

  (use-package whitespace)
  (defun my/configure-whitespace-python-ts-mode ()
    (setq-local whitespace-style
                (remove 'indentation whitespace-style)))
  (add-hook 'python-ts-mode-hook #'my/configure-whitespace-python-ts-mode))

(use-package mhtml-mode
  :mode "\\.html\\'"
  :config
  (defun my/configure-tab-width-mhtml-mode ()
    (setq tab-width sgml-basic-offset))
  (add-hook 'mhtml-mode-hook #'my/configure-tab-width-mhtml-mode)

  (use-package whitespace)
  (defun my/configure-whitespace-mhtml-mode ()
    (setq-local whitespace-style
                (remove 'indentation whitespace-style)))
  (add-hook 'mhtml-mode-hook #'my/configure-whitespace-mhtml-mode))

(use-package nxml-mode
  :defer t
  :config
  (defun my/configure-tab-width-nxml-mode ()
    (setq tab-width nxml-child-indent))
  (add-hook 'nxml-mode-hook #'my/configure-tab-width-nxml-mode)

  (use-package whitespace)
  (defun my/configure-whitespace-nxml-mode ()
    (setq-local whitespace-style
                (remove 'indentation whitespace-style)))
  (add-hook 'nxml-mode-hook #'my/configure-whitespace-nxml-mode))

(use-package css-ts-mode
  :defer t
  :config
  (defun my/configure-tab-width-css-ts-mode ()
    (setq css-indent-offset 2)
    (setq tab-width css-indent-offset))
  (add-hook 'css-ts-mode-hook #'my/configure-tab-width-css-ts-mode)

  (use-package whitespace)
  (defun my/configure-whitespace-css-ts-mode ()
    (setq-local whitespace-style
                (remove 'indentation whitespace-style)))
  (add-hook 'css-ts-mode-hook #'my/configure-whitespace-css-ts-mode))

(use-package conf-mode
  :defer t
  :bind ("C-M-i" . completion-at-point))

(use-package yaml-ts-mode
  :ensure t
  :mode "\\.ya?ml\\'")

(use-package sxhkdrc-mode
  :ensure t
  :mode "sxhkdrc\\'")

(use-package olivetti
  :ensure t
  :commands olivetti)

(use-package magit
  :ensure t
  :commands magit-status)

;; From the Notmuch documentation:
;;
;;   Due to the dependency on the command line interface, the Notmuch
;;   Emacs interface version must be compatible with the Notmuch
;;   version. On Linux, the easiest way to ensure this is to use the
;;   package(s) in your distribution's package repository.
;;
;;   It is not recommended to install Notmuch Emacs from the Emacs
;;   Lisp Package Archive (ELPA), as the version there is likely not
;;   in sync with the command line interface.
;;
(use-package notmuch
  :load-path "/usr/share/emacs/site-lisp"
  :commands notmuch
  :bind ("C-c m" . notmuch)
  :custom
  (notmuch-show-logo nil)
  (mail-user-agent #'notmuch-mua-new-mail)
  (notmuch-identities '("David Porter <david@daporter.net>"))
  (notmuch-fcc-dirs "Sent")
  (notmuch-wash-wrap-lines-length 80)
  (notmuch-saved-searches
   '((:name "Unread (Inbox)"     :query "tag:unread and tag:inbox"       :key "u")
     (:name "Unread All"         :query "tag:unread not tag:archived"    :key "U")
     (:name "Inbox"              :query "tag:inbox"                      :key "i")
     (:name "Drafts"             :query "tag:draft"                      :key "d")
     (:name "denote"             :query "tag:list/denote"                :key "ld"  :count-query "tag:list/denote and tag:unread")
     (:name "emacs-humanities"   :query "tag:list/emacs-humanities"      :key "leh" :count-query "tag:list/emacs-humanities and tag:unread")
     (:name "emacs-paris"        :query "tag:list/emacs-paris"           :key "lep" :count-query "tag:list/emacs-paris and tag:unread")
     (:name "great-conversation" :query "tag:list/great-conversation"    :key "lg"  :count-query "tag:list/great-conversation and tag:unread")
     (:name "Flagged"            :query "tag:flagged"                    :key "f")
     (:name "Reference"          :query "tag:reference not tag:archived" :key "r")
     (:name "Sent"               :query "tag:sent"                       :key "s"   :sort-order newest-first)
     (:name "Archived"           :query "tag:archived"                   :key "a"   :sort-order newest-first)
     (:name "All Mail"           :query "*"                              :key "A")
     (:name "Deleted"            :query "tag:deleted"                    :key "D")))
  (notmuch-archive-tags '("-inbox" "-unread" "+archived"))
  (notmuch-draft-folder "Drafts")
  (notmuch-tagging-keys
   '(("r" notmuch-show-mark-read-tags "Mark read")
     ("a" notmuch-archive-tags "Archive")
     ("f" ("+flagged") "Flag")
     ("s" ("+spam" "-inbox") "Mark as spam")
     ("d" ("+deleted" "-inbox") "Delete")))
  (notmuch-mua-user-agent-function #'notmuch-mua-user-agent-full)
  :config
  (add-hook 'notmuch-message-mode-hook #'turn-off-auto-fill)
  (add-hook 'notmuch-mua-send-hook #'notmuch-mua-attachment-check))

(use-package sendmail
  :custom
  (send-mail-function 'sendmail-send-it))

(use-package dictionary
  :bind ("M-#" . dictionary-lookup-definition)
  :custom
  (dictionary-server "localhost"))

(use-package kbd-mode
  :load-path "lisp"
  :mode "\\.kbd\\'"
  :custom
  (kbd-mode-kill-kmonad "pkill -9 kmonad")
  (kbd-mode-start-kmonad "kmonad ~/.config/kmonad/kd87.kbd"))

(use-package pdf-tools
  :ensure t
  :magic ("%PDF" . pdf-view-mode)
  :hook
  ((pdf-view-mode . pdf-history-minor-mode)
   (pdf-view-mode . pdf-view-fit-page-to-window)
   (pdf-view-mode . pdf-view-auto-slice-minor-mode))
  :config
  (pdf-loader-install))

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode))

(use-package org-noter
  :ensure t
  :commands org-noter
  :custom
  (org-noter-notes-search-path '("~/Dropbox/bibliography"))
  (org-noter-auto-save-last-location t))

(use-package keyfreq
  :ensure t
  :init
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package org-agenda
  :defer t
  :custom
  (org-agenda-todo-ignore-scheduled 'future))

(use-package gnus
  :commands gnus
  :custom
  (gnus-select-method '(nntp "news.gmane.io"))
  (gnus-article-mode-line-format "%G %S %m")
  (gnus-visible-headers
   '("^Subject:" "^From:" "^Date:" "^To:" "^Cc:" "^Newsgroups:"
     "Followup-To:" "Reply-To:"
     "^Organization:" "^Organisation:"
     "^X-Newsreader:" "^X-Mailer:" "^User-Agent:"))
  (gnus-sorted-header-list gnus-visible-headers)
  (gnus-user-date-format-alist
   '(((gnus-seconds-today) . "Today at %R")
     ((+ (* 60 60 24) (gnus-seconds-today)) . "Yesterday, %R")
     (t . "%Y-%m-%d %R")))
  (gnus-summary-make-false-root 'dummy)
  (gnus-summary-dummy-line-format
   (concat "   "
           "                      "
           "                            "
           "• %S\n"))
  (gnus-summary-line-format
   (concat "%0{%U%R%z%}"
           "%-16,16&user-date;  "
           "%-30,30f  "
           "%B" "%s\n"))
  (gnus-summary-mode-line-format "[%U] %p")
  (gnus-sum-thread-tree-single-indent   "• ")
  (gnus-sum-thread-tree-false-root      "  ")
  (gnus-sum-thread-tree-root            "• ")
  (gnus-sum-thread-tree-vertical        "│ ")
  (gnus-sum-thread-tree-leaf-with-other "├─➤ ")
  (gnus-sum-thread-tree-single-leaf     "└─➤ ")
  (gnus-sum-thread-tree-indent          "  "))

(use-package sendmail
  :defer t
  :custom
  (send-mail-function 'sendmail-send-it)
  (sendmail-program "msmtp")
  (mail-specify-envelope-from t)
  (message-sendmail-envelope-from 'header)
  (mail-envelope-from 'header))

(use-package yasnippet
  :ensure t)

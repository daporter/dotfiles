(add-to-list 'default-frame-alist '(internal-border-width . 6))

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backup/"))))
(setq backup-by-copying t)
(setq version-control t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq create-lockfiles nil)

(setq frame-title-format '("%b"))
(setq use-short-answers t)

(setq custom-file (concat user-emacs-directory "emacs-custom.el"))
(load custom-file)

(defun my/insert-date-time (prefix)
  "Insert the current date and time.
With PREFIX, use `ID' format, e.g. 20230323113431."
  (interactive "P")
  (let ((format (if (equal prefix '(4))
                    "%Y%m%d%H%M%S"
                  "%Y-%m-%d %H:%M:%S")))
    (insert (format-time-string format))))

(define-key global-map (kbd "C-c d") #'my/insert-date-time)

(require 'use-package)
;;(setq use-package-compute-statistics t)

(use-package package
  :defer t
  :init
  (add-hook 'package-menu-mode-hook #'hl-line-mode)
  :custom
  (package-archives
   '(("elpa" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")))

  ;; Highest number gets priority (what is not mentioned has priority 0)
  (package-archive-priorities
   '(("elpa" . 2)
     ("nongnu" . 1))))

(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  :custom
  (initial-buffer-choice t)             ; always start with *scratch*

  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (read-extended-command-predicate #'command-completion-default-include-p)

  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (enable-recursive-minibuffers t)
  (send-mail-function 'smtpmail-send-it)

  :config
  (dolist (cmd '(upcase-region
                 downcase-region
                 narrow-to-region))
    (put cmd 'disabled nil))

  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (add-hook 'package-menu-mode-hook #'hl-line-mode)
  (server-start)
  (column-number-mode 1)
  (delete-selection-mode 1)
  (savehist-mode 1)
  (electric-pair-mode 1)
  (electric-quote-mode 1)
  (auto-insert-mode t)

  (global-set-key (kbd "s-o") 'other-window)
  (global-set-key (kbd "s-b") 'switch-to-buffer)
  (global-set-key (kbd "C-M-?") 'hippie-expand))

(use-package ef-themes
  :ensure t
  :defer t
  :init
  (setq ef-themes-mixed-fonts t)
  (setq ef-themes-variable-pitch-ui t)
  (setq ef-themes-region '(neutral))
  (setq ef-themes-to-toggle '(ef-duo-light ef-autumn))
  (setq ef-duo-light-palette-overrides '((cursor red-warmer)))

  (defun my/ef-themes-configure-underlines ()
    "Configure the style of underlines."
    (ef-themes-with-colors
      (custom-set-faces
       `(ef-themes-underline-error
         ((,c :underline (:style line :color ,underline-err))))
       `(ef-themes-underline-info
         ((,c :underline (:style line :color ,underline-info))))
       `(ef-themes-underline-warning
         ((,c :underline (:style line :color ,underline-warning)))))))
  (defun my/ef-themes-boxed-mode-line ()
    "Tweak the style of the mode lines."
    (ef-themes-with-colors
      (custom-set-faces
       `(mode-line
         ((,c :background ,bg-mode-line :foreground ,fg-mode-line
              :box (:line-width 1 :color ,fg-dim))))
       `(mode-line-inactive
         ((,c :box (:line-width 1 :color ,bg-active)))))))
  ;;(add-hook 'ef-themes-post-load-hook #'my/ef-themes-boxed-mode-line)
  (add-hook 'ef-themes-post-load-hook #'my/ef-themes-configure-underlines)
  (ef-themes-select 'ef-duo-light))

(use-package fontaine
  :ensure t
  :defer t
  :init
  (setq fontaine-latest-state-file
        (locate-user-emacs-file "fontaine-latest-state.eld"))
  (setq fontaine-presets '((regular
                            :default-family "Jetbrains Mono"
                            :default-height 90
                            :fixed-pitch-family "Jetbrains Mono"
                            :fixed-pitch-serif-family "IBM Plex Mono"
                            :variable-pitch-family "Source Sans 3"
                            :variable-pitch-height 1.2
                            :bold-weight semibold)))
  (use-package ef-themes)
  (add-hook 'ef-themes-post-load-hook #'fontaine-apply-current-preset)
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular)))

(use-package ispell
  :defer t
  :custom (ispell-dictionary "australian-w_accents"))

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
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :ensure t
  :defer t
  :init (vertico-mode 1))

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

(use-package flymake
  :defer t
  :custom
  (flymake-fringe-indicator-position nil))

(use-package prog-mode
  :defer t
  :config
  (add-hook 'prog-mode-hook #'flymake-mode-on))

(use-package flymake-vale
  :load-path "lisp/flymake-vale"
  :commands flymake-vale-load)

(use-package text-mode
  :defer t
  :config
  ;; For some reason, the following doesn't work with :bind
  (define-key text-mode-map (kbd "C-M-i") #'completion-at-point)
  (defun my/disable-indent-tabs-mode ()
    (setq-local indent-tabs-mode nil))
  (defun my/configure-capfs-text-mode ()
    (require 'cape)
    (setq-local completion-at-point-functions
                '(cape-file cape-dabbrev cape-ispell)))
  (add-hook 'text-mode-hook #'my/disable-indent-tabs-mode)
  (add-hook 'text-mode-hook #'my/configure-capfs-text-mode)
  (add-hook 'text-mode-hook #'flymake-mode-on)
  (add-hook 'text-mode-hook #'flymake-vale-load))

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

(use-package puni
  :ensure t
  :defer t
  :init
  ;; The autoloads of Puni are set up so you can enable `puni-mode` or
  ;; `puni-global-mode` before `puni` is actually loaded. Only after
  ;; you press any key that calls Puni commands, it's loaded.
  (puni-global-mode 1))

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
  :commands flymake-markdownlint-setup
  :config
  (add-hook 'markdown-mode-hook #'flymake-markdownlint-setup))

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

(use-package flymake-shellcheck
  :ensure t
  :hook (sh-mode . flymake-shellcheck-load))

(use-package python-mode
  :ensure t
  :defer t
  :config
  (defun my/configure-tab-width-python-mode ()
    (setq tab-width python-indent-offset))
  (add-hook 'python-mode-hook #'my/configure-tab-width-python-mode)

  (use-package whitespace)
  (defun my/configure-whitespace-python-mode ()
    (setq-local whitespace-style
                (remove 'indentation whitespace-style)))
  (add-hook 'python-mode-hook #'my/configure-whitespace-python-mode))

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

(use-package css-mode
  :defer t
  :config
  (defun my/configure-tab-width-css-mode ()
    (setq css-indent-offset 2)
    (setq tab-width css-indent-offset))
  (add-hook 'css-mode-hook #'my/configure-tab-width-css-mode)

  (use-package whitespace)
  (defun my/configure-whitespace-css-mode ()
    (setq-local whitespace-style
                (remove 'indentation whitespace-style)))
  (add-hook 'css-mode-hook #'my/configure-whitespace-css-mode))

(use-package conf-mode
  :defer t
  :bind ("C-M-i" . #'completion-at-point))

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

(use-package adaptive-wrap
  :ensure t
  :commands adaptive-wrap-prefix-mode
  :config
  (add-hook text-mode-hook #'adaptive-wrap-prefix-mode))

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
  :bind ("C-c m" . #'notmuch)
  :custom
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
     (:name "Archived"           :query "tag:archived"                   :key "a")
     (:name "All Mail"           :query "*"                              :key "A")))
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
  (add-hook 'notmuch-mua-send-hook #'notmuch-mua-attachment-check))

(use-package sendmail
  :defer t
  :autoload smtpmail-send-it)

(use-package which-key
  :ensure t
  :defer t
  :init
  (which-key-mode))

(use-package dictionary
  :bind ("M-#" . dictionary-lookup-definition)
  :custom
  (dictionary-server "localhost"))

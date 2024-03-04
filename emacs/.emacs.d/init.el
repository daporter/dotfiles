;; -*- lexical-binding: t; -*-

;;; Utility functions

(defun my/set-cursor-type-bar ()
  "Set the cursor type to bar in the current buffer."
  (setq-local cursor-type 'bar))

(defun my/disable-indent-tabs-mode ()
  (setq-local indent-tabs-mode nil))

;;; Package definitions

(use-package package
  :hook (package-menu-mode . hl-line-mode)
  :custom
  (package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")))

  ;; Highest number gets priority (what is not mentioned has
  ;; priority 0)
  (package-archive-priorities
   '(("gnu" . 2)
     ("nongnu" . 1)))

  ;; Allow built-in packages to be upgraded via a package archive.
  (package-install-upgrade-built-in t)

  (use-package-compute-statistics nil))

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

  (defun buffer-mode (&optional buffer-or-name)
    "Returns the major mode associated with a buffer.
If buffer-or-name is nil return current buffer's mode."
    (buffer-local-value 'major-mode
                        (if buffer-or-name
                            (get-buffer buffer-or-name)
                          (current-buffer))))

  :custom
  (line-spacing 0.15)
  (cursor-type 'box)
  (initial-buffer-choice t)             ; always start with *scratch*
  (frame-title-format '("%b"))
  (use-short-answers t)

  ;; Improve the spacing of underlines.
  (x-use-underline-position-properties nil)

  ;; Necessary for visual-fill-column-mode:
  (mode-line-right-align-edge 'right-fringe)

  (backup-directory-alist
   `(("." . ,(concat user-emacs-directory "backup/"))))
  (backup-by-copying t)
  (version-control t)
  (delete-old-versions t)
  (kept-new-versions 6)
  (create-lockfiles nil)

  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))

  (comment-multi-line t)

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

  :config
  (setq custom-file (concat user-emacs-directory "emacs-custom.el"))
  (load custom-file)

  (dolist (cmd '(upcase-region
                 downcase-region
                 narrow-to-region
                 set-goal-column))
    (put cmd 'disabled nil))

  (modify-all-frames-parameters '((internal-border-width . 10)
                                  (scroll-bar-width      .  5)))

  ;; Specify the fonts to use for displaying emoji.
  (set-fontset-font t 'emoji
                    (cond
                     ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
                     ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
                     ((member "Symbola" (font-family-list)) "Symbola")))

  (load-theme 'my-iceberg t)

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

  (defun my/simple-new-line-above (n)
    "Create N empty lines above the current one.
When called interactively without a prefix numeric argument, N is
1."
    (interactive "p")
    (let ((point-min (point-min)))
      (if (or (bobp)
              (eq (point) point-min)
              (eq (line-number-at-pos point-min) 1))
          (progn
            (goto-char (line-beginning-position))
            (dotimes (_ n) (insert "\n"))
            (forward-line (- n)))
        (forward-line (- n))
        (my/simple-new-line-below n))))
  (global-set-key (kbd "<C-S-return>") #'my/simple-new-line-above)

  (defun my/simple-new-line-below (n)
    "Create N empty lines below the current one.
When called interactively without a prefix numeric argument, N is
1."
    (interactive "p")
    (goto-char (line-end-position))
    (dotimes (_ n) (insert "\n")))
  (global-set-key (kbd "<C-return>") #'my/simple-new-line-below)

  (defun my/open-line-and-indent ()
    "Like `newline-and-indent', but do not move the point."
    (interactive)
    (save-excursion
      (newline-and-indent)))
  (global-set-key (kbd "C-o") #'my/open-line-and-indent))

(use-package window
  :custom
  (display-buffer-alist
   `((,(rx (| "*shell*" "*eshell*"))
      (display-buffer-in-side-window)
      (side . bottom)
      (slot . -1)
      (window . root)
      (window-height . 0.33))

     ((or . ((derived-mode . messages-buffer-mode)
             (derived-mode . backtrace-mode)
             ,(rx "*Warnings*")))
      (display-buffer-in-side-window)
      (side . bottom)
      (slot . 0)
      (window-height . 0.33))

     ((derived-mode . help-mode)
      (display-buffer-in-side-window)
      (side . right)
      (slot . -1)
      (window-width . 80)
      (window-height . shrink-window-if-larger-than-buffer))

     (,(rx "*info*")
      (display-buffer-in-side-window)
      (side . right)
      (slot . 0)
      (window-width . 80))

     ((or . ((derived-mode . Man-mode)
             (derived-mode . woman-mode)
             ,(rx (: (| "*Man" "*woman"))
                  (0+ anychar))))
      (display-buffer-in-side-window)
      (side . right)
      (slot . 1)
      (window-width . 80))

     (,(rx "*Faces*")
      (display-buffer-reuse-window
       display-buffer-use-some-window)
      (dedicated . t)
      (window-height . fit-window-to-buffer)
      (window-min-height . 20)
      (window-width . 80)
      (body-function . select-window))

     ((or . ((derived-mode . occur-mode)
             (derived-mode . grep-mode)))
      (display-buffer-reuse-mode-window
       display-buffer-below-selected)
      (dedicated . t)
      (body-function . select-window))

     (,(rx (| "*Org Select*" "*Org Note*" "*Agenda Commands*"))
      (display-buffer-in-side-window)
      (side . bottom)
      (slot . 0)
      (dedicated . t)
      (window-parameters . ((mode-line-format . none))))

     (,(rx (: (| "*Output Preview" "*Register Preview")
              (0+ anychar)))
      (display-buffer-reuse-mode-window
       display-buffer-at-bottom))))

  (switch-to-buffer-in-dedicated-window 'pop)
  (switch-to-buffer-obey-display-actions t))

(use-package faces
  :config
  (set-face-attribute 'default           nil :font "Iosevka-10")
  (set-face-attribute 'fixed-pitch       nil :font "Iosevka-10")
  (set-face-attribute 'fixed-pitch-serif nil :font "Iosevka Slab-10")
  (set-face-attribute 'variable-pitch    nil :font "XCharter-10.5"))

(use-package paren
  :custom
  (show-paren-context-when-offscreen 'overlay))

(use-package compile
  :bind (:map prog-mode-map
              ("C-c c" . compile))
  :custom
  (compilation-auto-jump-to-first-error 'if-location-known))

(use-package dired
  :defer t
  :custom
  (dired-recursive-copies 'always)
  (dired-dwim-target t))                ; try to guess target directory for copy

(use-package ediff
  :defer t
  :custom
  (ediff-keep-variants         nil)
  (ediff-show-clashes-only     t)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package nerd-icons
  :ensure t
  :defer t)

(use-package nerd-icons-dired
  :ensure t
  :hook dired-mode)

(use-package nerd-icons-ibuffer
  :ensure t
  :hook ibuffer-mode)

(use-package nerd-icons-completion
  :ensure t
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode))

(use-package apropos
  :defer t
  :custom
  (apropos-sort-by-scores t))

(use-package ispell
  :defer t
  :custom (ispell-dictionary "australian-w_accents"))

(use-package hippie-exp
  :bind
  ("M-/" . hippie-expand)
  :custom
  (hippie-expand-try-functions-list '(try-expand-dabbrev-visible
                                      try-expand-dabbrev
                                      try-expand-dabbrev-all-buffers
                                      try-complete-file-name-partially
                                      try-complete-file-name
                                      try-expand-all-abbrevs
                                      try-expand-list
                                      try-expand-line
                                      try-expand-dabbrev-from-kill
                                      try-complete-lisp-symbol-partially
                                      try-complete-lisp-symbol)))

(use-package bookmark
  :defer t
  :custom
  ;; Save bookmarks on each modification.
  (bookmark-save-flag 1))

(use-package proced
  :defer t
  :custom
  (proced-auto-update-flag t)
  (proced-enable-color-flag t))

(use-package saveplace
  :defer t
  :init (save-place-mode 1))

(use-package savehist
  :defer t
  :init (savehist-mode 1))

(use-package marginalia
  :ensure t
  :init (marginalia-mode 1)
  :bind (:map minibuffer-local-map ("M-m" . marginalia-cycle)))

(use-package orderless
  :ensure t
  :defer t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

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

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Some notes CAPFs.
;;
;; (Taken from https://www.reddit.com/r/emacs/comments/td0nth/comment/i0i8hi7/?context=3&share_id=CfzOVcILIBvpQKfRmTanK)
;;
;; You've always been able to add multiple CAPFs to the variable
;; `completion-at-point-functions'.  This isn't CAPE-specific (other than the
;; fact that CAPE provides a few simple CAPFs for easy use).  But this
;; "multi-CAPF" setup may not function like you expect.  The reason is that CAPF
;; completion is a two-step process.
;;
;; Step 1: Each CAPF on the list is asked first whether it can complete at this
;; location.  So if you are in a string, a CAPF might say "no, I can't complete
;; in strings", and just return nil.  The first CAPF on the list to say it can
;; in principle provide some completions "wins".  But it hasn't yet actually
;; checked for completions!
;;
;; Step 2: The winning CAPF is asked for the completions at point.  But maybe it
;; returns no completions at all!  If so, completion is over.
;;
;; This is fine if, say; the first CAPF on your list works outside of strings,
;; and the next works inside of strings; they'll dovetail nicely.  But what if
;; they both work "in the same place"?  CAPFs can themselves set a property
;; `:exclusive 'no', which means "if Step 2 fails, go back to Step 1" and try
;; the next CAPF, but in practice none do, since Emacs has some bugs related to
;; this.  To me it's strange to let the CAPFs themselves decide this; this
;; should be a user choice.  Cape makes that possible.

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file t)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t)
  (add-to-list 'completion-at-point-functions #'cape-dict t)

  (defun my/cape-capf-setup-elisp ()
    (setq-local completion-at-point-functions '(elisp-completion-at-point t)))

  (defun my/cape-capf-setup-eglot ()
    (setq-local completion-at-point-functions
                (list (cape-capf-properties
                       (cape-capf-inside-code
                        (cape-capf-super #'eglot-completion-at-point
                                         #'cape-keyword))
                       :exclusive)
                      t)))

  (defun my/cape-capf-setup-org ()
    (setq-local completion-at-point-functions
                (list #'pcomplete-completions-at-point
                      #'cape-elisp-block
                      t)))

  (defun my/cape-capf-setup-eshell ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super #'pcomplete-completions-at-point
                                       #'comint-completion-at-point
                                       #'cape-history)
                      t)))

  (defun my/cape-capf-setup-sh ()
    (setq-local completion-at-point-functions
                (list #'sh-completion-at-point-function
                      t)))

  :hook ((emacs-lisp-mode    . my/cape-capf-setup-elisp)
         (eglot-managed-mode . my/cape-capf-setup-eglot)
         (org-mode           . my/cape-capf-setup-org)
         (eshell-mode        . my/cape-capf-setup-eshell)
         (sh-mode            . my/cape-capf-setup-sh)))

(use-package consult
  :ensure t
  :bind (([remap switch-to-buffer]              . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
         ([remap project-switch-to-buffer]      . consult-project-buffer)
         ([remap yank-pop]                      . consult-yank-pop)
         ("C-c m"                               . consult-man)
         ("C-c i"                               . consult-info)
         ([remap Info-search]                   . consult-info)
         ("C-c o"                               . consult-outline)))

(use-package embark
  :ensure t
  :bind (("C-," . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package popper
  :ensure t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        (append my/help-modes-list
                my/man-modes-list
                my/repl-modes-list
                my/repl-names-list
                my/occur-grep-modes-list
                '(Custom-mode
                  compilation-mode
                  messages-buffer-mode)
                '(("^\\*Warnings\\*$" . hide)
                  ("^\\*Compile-Log\\*$" . hide)
                  ;; "^\\*Messages\\*$"
                  "^\\*Backtrace\\*"
                  "^\\*Apropos"
                  "^Calc:"
                  "^\\*eldoc\\*"
                  "^\\*TeX errors\\*"
                  "^\\*TeX Help\\*"
                  "^\\*ielm\\*"
                  "^\\*ChatGPT\\*"
                  "^\\*gptel-quick\\*"
                  "\\*Shell Command Output\\*"
                  ("\\*Async Shell Command\\*" . hide)
                  ("\\*Detached Shell Command\\*" . hide)
                  "\\*Completions\\*"
                  ;; "\\*scratch.*\\*$"
                  "[Oo]utput\\*")))

  ;; Group by project.el project root, with fall back to default-directory
  (setq popper-group-function #'popper-group-by-directory)

  :custom
  (popper-display-control nil)

  :config
  (popper-mode +1)
  (popper-echo-mode +1))


(use-package avy
  :ensure t
  :bind   (("C-." . avy-goto-char-2)
           :map isearch-mode-map
           ("C-." . avy-isearch))
  :custom
  (avy-keys '(?d ?a ?n ?e ?s ?r ?u ?c ?g))
  (avy-dispatch-alist '((?m . avy-action-mark)
                        (?i . avy-action-ispell)
                        (?  . avy-action-mark-to-char)
                        (?z . avy-action-zap-to-char)
                        (?, . avy-action-embark)
                        (?x . avy-action-exchange)

                        (11 . avy-action-kill-line)
                        (25 . avy-action-yank-line)

                        (?w . avy-action-easy-copy)
                        (?k . avy-action-kill-stay)
                        (?y . avy-action-yank)
                        (?t . avy-action-teleport)

                        (?W . avy-action-copy-whole-line)
                        (?K . avy-action-kill-whole-line)
                        (?Y . avy-action-yank-whole-line)
                        (?T . avy-action-teleport-whole-line)))

  :config
  (defun avy-action-easy-copy (pt)
    (unless (require 'easy-kill nil t)
      (user-error "Easy Kill not found, please install."))
    (goto-char pt)
    (cl-letf (((symbol-function 'easy-kill-activate-keymap)
               (lambda ()
                 (let ((map (easy-kill-map)))
                   (set-transient-map
                    map
                    (lambda ()
                      ;; Prevent any error from activating the keymap forever.
                      (condition-case err
                          (or (and (not (easy-kill-exit-p this-command))
                                   (or (eq this-command
                                           (lookup-key map (this-single-command-keys)))
                                       (let ((cmd (key-binding
                                                   (this-single-command-keys) nil t)))
                                         (command-remapping cmd nil (list map)))))
                              (ignore
                               (easy-kill-destroy-candidate)
                               (unless (or (easy-kill-get mark) (easy-kill-exit-p this-command))
                                 (easy-kill-save-candidate))))
                        (error (message "%s:%s" this-command (error-message-string err))
                               nil)))
                    (lambda ()
                      (let ((dat (ring-ref avy-ring 0)))
                        (select-frame-set-input-focus
                         (window-frame (cdr dat)))
                        (select-window (cdr dat))
                        (goto-char (car dat)))))))))
      (easy-kill)))

  (defun avy-action-exchange (pt)
    "Exchange sexp at PT with the one at point."
    (set-mark pt)
    (transpose-sexps 0))

  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (defun avy-action-kill-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-line))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)

  (defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)

  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt)))

(use-package transpose-frame
  :ensure t
  :config
  (defvar-keymap my/transpose-frame-map
    :doc "Keymap for transpose-frame"
    :repeat t
    "f" #'flop-frame
    "t" #'transpose-frame)
  (keymap-set global-map
              "C-c f" (cons "Transpose Frame" my/transpose-frame-map)))

(use-package pulsar
  :ensure t
  :hook
  ((next-error       . pulsar-pulse-line)
   (minibuffer-setup . pulsar-pulse-line)
   (imenu-after-jump . pulsar-recenter-top)
   (imenu-after-jump . pulsar-reveal-entry))
  :config
  (pulsar-global-mode 1))

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

(use-package vterm
  :ensure t
  :commands vterm)

(use-package whitespace
  :defer t
  :bind (:map prog-mode-map
              ("C-c w" . whitespace-cleanup)
              :map text-mode-map
              ("C-c w" . whitespace-cleanup))
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

(use-package whole-line-or-region
  :ensure t
  :config
  ;; I don’t like the changed behaviour of comment-dwim.
  (define-key whole-line-or-region-local-mode-map [remap comment-dwim] nil)
  (whole-line-or-region-global-mode 1))

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

(use-package prog-mode
  :defer t
  :init
  (defun my/set-fill-column ()
    (setq-local fill-column 80)
    (display-fill-column-indicator-mode 1))

  :hook
  (prog-mode . my/set-cursor-type-bar)
  (prog-mode . my/set-fill-column))

(use-package eldoc
  :custom
  (eldoc-echo-area-use-multiline-p nil))

(use-package treesit
  :init
  (defun my/remap-treesitter-modes ()
    ;; Prefer tree-sitter-enabled modes.
    (setq major-mode-remap-alist
          '((bash-mode       . bash-ts-mode)
            (c-mode          . c-ts-mode)
            (c++-mode        . c++-ts-mode)
            (css-mode        . css-ts-mode)
            (js2-mode        . js-ts-mode)
            (json-mode       . json-ts-mode)
            (typescript-mode . typescript-ts-mode)
            (yaml-mode       . yaml-ts-mode))))

  :hook
  (after-init . my/remap-treesitter-modes)

  :custom
  (treesit-font-lock-level 4))

(use-package eglot
  :init
  (defun my/eglot-disable-hints ()
    (eglot-inlay-hints-mode 0))

  :hook
  ((c-ts-mode          . eglot-ensure)
   (css-ts-mode        . eglot-ensure)
   (eglot-managed-mode . my/eglot-disable-hints))

  :bind
  (:map eglot-mode-map
        ("C-c l f" . eglot-format)
        ("C-c l h" . eldoc)
        ("C-c l o" . eglot-code-action-organize-imports)
        ("C-c l q" . eglot-code-action-quickfix)
        ("C-c l r" . eglot-rename)))

(use-package text-mode
  :defer t
  :config
  ;; For some reason, the following doesn't work with :bind
  (define-key text-mode-map (kbd "C-M-i") #'completion-at-point)
  (define-key text-mode-map (kbd "C-c P") #'repunctuate-sentences)

  :hook
  (text-mode . my/set-cursor-type-bar)
  (text-mode . my/disable-indent-tabs-mode))

(use-package flymake-vale
  :load-path "lisp/flymake-vale"
  :commands flymake-vale-load)

(use-package visual-line-mode
  :hook (text-mode))

(use-package adaptive-wrap
  :ensure t
  :hook (visual-line-mode . adaptive-wrap-prefix-mode))

(use-package unfill
  :ensure t
  :commands (unfill-region unfill-paragraph)
  :bind (:map text-mode-map
              ("C-c q" . unfill-paragraph)))

(use-package lorem-ipsum
  :ensure t
  :defer t)

(use-package elisp-mode
  :defer t
  :hook (emacs-lisp-mode  . my/disable-indent-tabs-mode)
  :bind ("C-x x e" . eval-buffer))

(use-package c-ts-mode
  :after eglot
  :defer t
  :bind
  (:map c-ts-mode-map
        ("C-c o" . ff-find-other-file))
  :custom
  (c-ts-mode-indent-style 'linux)
  (c-ts-mode-indent-offset 8))

(use-package markdown-mode
  :ensure t
  :init
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode))
  :hook (markdown-mode . variable-pitch-mode)
  :custom
  (markdown-command "pandoc")
  (markdown-asymmetric-header t)
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
  (add-hook 'whitespace-mode-hook #'my/configure-whitespace-sh-mode))

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
  :hook (text-mode . my/set-cursor-type-bar)
  :bind ("C-M-i" . completion-at-point))

(use-package yaml-ts-mode
  :ensure t
  :mode "\\.ya?ml\\'")

(use-package eshell
  :defer t
  :bind
  (:map eshell-hist-mode-map ("C-<up>" . nil)) ; used by windmove
  :config
  (dolist (module '(eshell-smart eshell-tramp))
    (add-to-list 'eshell-modules-list module)))

(use-package sxhkdrc-mode
  :ensure t
  :mode "sxhkdrc\\'")

(use-package olivetti
  :ensure t
  :commands olivetti)

(use-package magit
  :ensure t
  :commands magit-status
  :bind (:map project-prefix-map
              ("m" . magit-project-status))
  :custom
  (magit-display-buffer-function
   'magit-display-buffer-same-window-except-diff-v1)
  (git-commit-summary-max-length 50)
  (git-commit-style-convention-checks '(non-empty-second-line
                                        overlong-summary-line))
  :config
  (add-to-list 'project-switch-commands
               '(magit-project-status "Magit" "m")))

(use-package hl-todo
  :ensure t
  :hook prog-mode)

(use-package magit-todos
  :ensure t
  :after magit
  :config
  (magit-todos-mode 1))

(use-package denote
  :ensure t
  :commands (denote denote-rename-file)
  :custom
  (denote-directory "~/Dropbox/journal"))

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
  :bind ("C-c n" . notmuch)
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

  ;; # After each tagging operation, signal the window manager status bar
  ;; # updater.
  (notmuch-after-tag-hook
   (lambda ()
     (shell-command-to-string "kill -SIGRTMIN+1 $(cat /tmp/mtstatus.pid)")))

  :config
  (add-hook 'notmuch-message-mode-hook #'turn-off-auto-fill)
  (add-hook 'notmuch-mua-send-hook #'notmuch-mua-attachment-check)
  (add-hook 'notmuch-show-mode-hook
            (lambda () (variable-pitch-mode 1))))

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
   (pdf-view-mode . pdf-view-auto-slice-minor-mode)
   (pdf-view-mode . pdf-view-midnight-minor-mode))
  :custom
  (pdf-view-midnight-colors '("#ffffff" . "#121212"))
  :config
  (pdf-loader-install))

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode))

(use-package org
  :hook (org-mode . variable-pitch-mode)
  :custom
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis "…"))

(use-package org-indent
  :hook (org-mode))

(use-package org-modern
  :ensure t
  :after org
  :config (global-org-modern-mode 1))

(use-package org-noter
  :ensure t
  :commands org-noter
  :custom
  (org-noter-notes-search-path '("~/Dropbox/bibliography"))
  (org-noter-auto-save-last-location t))

(use-package org-agenda
  :defer t
  :bind ("C-c t" . org-todo-list)
  :custom
  (org-agenda-todo-ignore-scheduled 'future)
  (org-agenda-sorting-strategy
   '((agenda habit-down time-up priority-down category-keep)
     (todo scheduled-up)
     (tags priority-down category-keep)
     (search category-keep)))
  :config
  ;; Don’t steal my keybindings!
  (keymap-unset org-mode-map "C-," t))

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

(use-package youtube-dl
  :load-path "lisp/youtube-dl-emacs"
  :commands (youtube-dl youtube-dl-list)
  :custom
  (youtube-dl-program "yt-dlp")
  (youtube-dl-directory "~/Downloads"))

(use-package windmove
  :config
  (windmove-default-keybindings 'control)
  (windmove-delete-default-keybindings 'none '(control shift))
  (windmove-swap-states-default-keybindings '(meta shift))
  (defvar-keymap windmove-repeat-map
    :doc "Keymap for windmove operations"
    :repeat t
    "<left>"    #'windmove-left
    "S-<left>"  #'windmove-swap-states-left
    "<right>"   #'windmove-right
    "S-<right>" #'windmove-swap-states-right
    "<up>"      #'windmove-up
    "S-<up>"    #'windmove-swap-states-up
    "<down>"    #'windmove-down
    "S-<down>"  #'windmove-swap-states-down)
  (map-keymap
   (lambda (_key cmd)
     (when (symbolp cmd)
       (put cmd 'repeat-map 'windmove-repeat-map)))
   windmove-repeat-map))

(use-package winner
  :config
  (winner-mode 1))

(use-package elec-pair
  :config
  (electric-pair-mode 1))

(use-package electric
  :defer t
  :config
  (electric-quote-mode 1))

(use-package repeat
  :config
  :hook (after-init . repeat-mode)
  :bind ("C-_" . repeat))                        ; reuse one of the ‘undo’ bindings

(use-package server
  :config
  (server-start))

(use-package simple
  :config
  (column-number-mode 1))

(use-package delsel
  :config
  (delete-selection-mode 1))

(use-package fringe
  :config
  (fringe-mode 10))

(use-package autoinsert
  :config
  (auto-insert-mode t))

(use-package pixel-scroll
  :config
  (pixel-scroll-precision-mode 1))

(use-package find-func
  :config
  (find-function-setup-keys))

(use-package dabbrev
  :custom
  (dabbrev-check-all-buffers nil)
  :config
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode))

(use-package pcmpl-args
  :ensure t
  :after pcomplete)

(use-package page-break-lines
  :ensure t
  :hook (after-init . global-page-break-lines-mode))

(use-package hl-line
  :hook (prog-mode text-mode conf-mode notmuch-search))

(use-package desc-text
  :bind ("C-c p" . describe-text-properties))

(use-package face-remap
  :init
  (defun my/variable-pitch-set-line-spacing ()
    ;; The font XCharter needs extra line spacing.
    (if (and buffer-face-mode
             (eq buffer-face-mode-face 'variable-pitch))
        (setq-local line-spacing 0.25)
      (kill-local-variable 'line-spacing))) ; revert to global setting

  :hook
  (buffer-face-mode . my/variable-pitch-set-line-spacing))

(use-package recentf
  :hook (after-init))

(use-package ligature
  :ensure t
  :config
  ;; Enable all Iosevka ligatures in programming modes
  (ligature-set-ligatures
   '(prog-mode text-mode)
   '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->"
     "<---->" "<!--" "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>"
     "<==>" "<===>" "<====>" "<!---" "<~~" "<~" "~>" "~~>" "::" ":::" "=="
     "!=" "===" "!==" ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:"
     "-:" "=:" "<******>" "++" "+++"))
  (global-ligature-mode t))

(use-package olivetti
  :ensure t
  :commands olivetti-mode)

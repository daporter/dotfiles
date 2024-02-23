;; -*- lexical-binding: t; -*-

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

  (use-package-compute-statistics 1))

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

  ;; Necessary for visual-fill-column-mode:
  (mode-line-right-align-edge 'right-fringe)

  (display-buffer-alist
   '(
     ("^\\*[Ee]shell [Ee]xport: .*\\*$"
      (display-buffer-reuse-window display-buffer-use-some-window))

     ;; ----------------------------------------------------------------
     ;; Windows on top
     ;; ----------------------------------------------------------------

     ("\\*\\(?:Org Select\\|Agenda Commands\\)\\*"
      (display-buffer-below-selected
       display-buffer-in-side-window)
      (body-function . select-window)
      (window-height . (lambda (win) (fit-window-to-buffer win nil 12)))
      (side . top)
      (slot . -2)
      (preserve-size . (nil . t))
      (window-parameters . ((mode-line-format . nil))))

     ("\\*Buffer List\\*" (display-buffer-in-side-window)
      (side . top)
      (slot . 0)
      (window-height . shrink-window-if-larger-than-buffer))

     ((lambda (buf act) (member (buffer-mode buf) my/occur-grep-modes-list))
      (display-buffer-reuse-mode-window
       display-buffer-in-direction
       display-buffer-in-side-window)
      (side . top)
      (slot . 5)
      (window-height . (lambda (win) (fit-window-to-buffer win 20 10)))
      (direction . above)
      (body-function . select-window))

     ;; ----------------------------------------------------------------
     ;; Windows on the side
     ;; ----------------------------------------------------------------

     ("\\*Faces\\*" (display-buffer-in-side-window)
      (window-width . 0.25)
      (side . right)
      (slot . -2)
      (window-parameters . ((no-other-window . t)
                            ;; (mode-line-format . (:eval (my/helper-window-mode-line-format)))
                            )))

     ;; ----------------------------------------------------------------
     ;; Windows at the bottom
     ;; ----------------------------------------------------------------

     ("\\*Backtrace\\*" (display-buffer-in-side-window)
      (window-height . 0.20)
      (side . bottom)
      (slot . -9)
      ;; (preserve-size . (nil . t))
      ;; (window-parameters . (;; (mode-line-format . (:eval (my/helper-window-mode-line-format)))
      ;;                       ))
      )

     ("\\*RefTex" (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . -9)
      ;; (preserve-size . (nil . t))
      ;; (window-parameters . (;; (mode-line-format . (:eval (my/helper-window-mode-line-format)))
      ;;                       ))
      )

     ;; ("\\*scratch\\*"
     ;;  display-buffer-in-side-window
     ;;  (body-function . select-window)
     ;;  ;; (window-width 35)
     ;;  (window-height . (lambda (win) (fit-window-to-buffer win 20 nil 85)))
     ;;  (side . bottom)
     ;;  (slot . -8))

     ((lambda (buf act) (member (buffer-mode buf) my/message-modes-list))
      (display-buffer-at-bottom display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . -6)
      ;; (preserve-size . (nil . t))
      ;; (window-parameters . ((no-other-window . #'ignore)
      ;;                       ;; (mode-line-format . (:eval (my/helper-window-mode-line-format)))
      ;;                       ))
      )

     ("\\*\\(?:Warnings\\|Compile-Log\\|Messages\\)\\*" ;\\|Tex Help\\|TeX errors
      (display-buffer-at-bottom display-buffer-in-side-window display-buffer-in-direction)
      (window-height . (lambda (win) (fit-window-to-buffer
                                      win
                                      (floor (frame-height) 5))))
      (side . bottom)
      (direction . below)
      (slot . -5)
      ;; (preserve-size . (nil . t))
      (window-parameters . ((split-window . #'ignore)
                            ;; (no-other-window . t)
                            ;; (mode-line-format . (:eval (my/helper-window-mode-line-format)))
                            )))

     ("[Oo]utput\\*" display-buffer-in-side-window
      (window-height . (lambda (win)
                         (fit-window-to-buffer win (floor (frame-height) 2.5))))
      (side . bottom)
      (slot . -4)
      ;; (preserve-size . (nil . t))
      ;; (window-parameters . ((no-other-window . t)
      ;;                       ;; (mode-line-format . (:eval (my/helper-window-mode-line-format)))
      ;;                       ))
      )

     ("\\*Async Shell Command\\*" display-buffer-in-side-window
      (window-height . 0.20)
      (side . bottom)
      (slot . -4)
      ;; (preserve-size . (nil . t))
      (window-parameters . ((no-other-window . t)
                            ;; (mode-line-format . (:eval (my/helper-window-mode-line-format)))
                            )))

     ("\\*\\(Register Preview\\).*" (display-buffer-in-side-window)
      (window-height . 0.20)       ; See the :hook
      (side . bottom)
      (slot . -3)
      (window-parameters . ((no-other-window . t)
                            ;; (mode-line-format . (:eval (my/helper-window-mode-line-format)))
                            )))

     ("\\*Completions\\*" (display-buffer-in-side-window)
      (window-height . 0.20)
      (side . bottom)
      (slot . -2)
      ;; (window-parameters . ((no-other-window . t)
      ;;                       ;; (mode-line-format . (:eval (my/helper-window-mode-line-format)))
      ;;                       ))
      )

     ("\\*Apropos\\*" (display-buffer-in-side-window)
      ;; (window-height . 0.40)
      (window-width . 65)
      (side . right)
      (slot . -2)
      (window-parameters . (;; (no-other-window . t)
                            ;; (mode-line-format . (:eval (my/helper-window-mode-line-format)))
                            )))


     ((lambda (buf act) (or (seq-some (lambda (regex) (string-match-p regex buf))
                                      my/repl-names-list)
                            (seq-some (lambda (mode)
                                        (equal
                                         (buffer-mode buf)
                                         mode))
                                      my/repl-modes-list)))
      (display-buffer-reuse-window
       display-buffer-in-direction
       display-buffer-in-side-window)
      (body-function . select-window)
      ;; display-buffer-at-bottom
      (window-height . .35)
      (window-width .  .40)
      ;; (preserve-size . (nil . t))
      (direction . below)
      (side . bottom)
      (slot . 1))

     ((lambda (buf act) (member (buffer-mode buf) my/help-modes-list))
      (display-buffer-reuse-window
       display-buffer-in-direction
       display-buffer-in-side-window)
      (body-function . select-window)
      ;; (direction . bottom)
      ;; (window-height . (lambda (win) (fit-window-to-buffer win 25 14)))
      (window-width . 77 ;; (lambda (win) (fit-window-to-buffer win nil nil 75 65))
                    )
      (direction . below)
      (side . right)
      (slot . 2)
      (window-parameters . ((split-window . #'ignore)
                            ;; (no-other-window . t)
                            ;; (mode-line-format . (:eval (my/helper-window-mode-line-format)))
                            )))

     ("^\\*eldoc.*\\*$"
      (display-buffer-reuse-window
       display-buffer-in-direction
       display-buffer-in-side-window)
      ;; (body-function . select-window)
      ;; (direction . bottom)
      ;; (window-height . (lambda (win) (fit-window-to-buffer win 25 14)))
      (window-width . 82 ;; (lambda (win) (fit-window-to-buffer win nil nil 75 65))
                    )
      (direction . below)
      (side . below)
      (slot . 2)
      (window-parameters . ((split-window . #'ignore)
                            (no-other-window . t)
                            (mode-line-format . none))))

     ((lambda (buf act) (member (buffer-mode buf) '(ibuffer-mode bookmark-bmenu-mode)))
      (;; display-buffer-reuse-window
       ;; display-buffer-in-side-window
       ;;display-buffer-at-bottom
       display-buffer-below-selected)
      (body-function . select-window)
      (direction . below)
      (window-height . (lambda (win) (fit-window-to-buffer win 30 7)))
      ;; (dedicated . t)
      ;; (window-width . (lambda (win) (fit-window-to-buffer win nil nil 85 55)))
      ;; (direction . right)
      (side . bottom)
      (slot . 2))

     ))

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

  ;; Prefer tree-sitter-enabled modes.
  (major-mode-remap-alist
   '((bash-mode . bash-ts-mode)
     (c-mode . c-ts-mode)
     (c++-mode . c++-ts-mode)
     (css-mode . css-ts-mode)
     (js2-mode . js-ts-mode)
     (json-mode . json-ts-mode)
     (typescript-mode . typescript-ts-mode)
     (yaml-mode . yaml-ts-mode)))

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

  (window-divider-default-right-width  1)
  (window-divider-default-bottom-width 0)
  (window-divider-default-places       t)

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

  (defun buffer-mode (&optional buffer-or-name)
    "Returns the major mode associated with a buffer.
If buffer-or-name is nil return current buffer's mode."
    (buffer-local-value 'major-mode
                        (if buffer-or-name
                            (get-buffer buffer-or-name)
                          (current-buffer))))

  (defvar my/occur-grep-modes-list '(occur-mode
                                     grep-mode
                                     xref--xref-buffer-mode
                                     ivy-occur-grep-mode
                                     ivy-occur-mode
                                     locate-mode
                                     flymake-diagnostics-buffer-mode
                                     rg-mode)
    "List of major-modes used in occur-type buffers")

  (defvar my/repl-modes-list '(matlab-shell-mode
                               eshell-mode
                               geiser-repl-mode
                               shell-mode
                               eat-mode
                               ;; vterm-mode
                               inferior-python-mode
                               cider-repl-mode
                               fennel-repl-mode
                               jupyter-repl-mode
                               inferior-ess-julia-mode)
    "List of major-modes used in REPL buffers")

  (defvar my/repl-names-list
    '("^\\*\\(?:.*?-\\)\\{0,1\\}e*shell[^z-a]*\\(?:\\*\\|<[[:digit:]]+>\\)$"
      "\\*.*REPL.*\\*"
      "\\*MATLAB\\*"
      "\\*Python\\*"
      "^\\*jupyter-repl.*?\\(\\*\\|<[[:digit:]]>\\)$"
      "\\*Inferior .*\\*$"
      "^\\*julia.*\\*$"
      "^\\*cider-repl.*\\*$"
      "\\*ielm\\*"
      "\\*edebug\\*")
    "List of buffer names used in REPL buffers")

  (defvar my/help-modes-list '(helpful-mode
                               help-mode
                               pydoc-mode
                               eldoc-mode
                               TeX-special-mode)
    "List of major-modes used in documentation buffers")

  (defvar my/man-modes-list '(Man-mode woman-mode)
    "List of major-modes used in Man-type buffers")

  (defvar my/message-modes-list '(compilation-mode
                                  edebug-eval-mode)
    "List of major-modes used in message buffers")

  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (server-start)
  (repeat-mode 1)
  (column-number-mode 1)
  (delete-selection-mode 1)
  (savehist-mode 1)
  (electric-pair-mode 1)
  (electric-quote-mode 1)
  (fringe-mode 10)
  (window-divider-mode 1)
  (auto-insert-mode t)
  (pixel-scroll-precision-mode 1)
  (find-function-setup-keys)

  (load-theme 'daporter t)

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

(use-package paren
  :custom
  (show-paren-context-when-offscreen 'overlay))

(use-package compile
  :custom
  (compilation-auto-jump-to-first-error 'if-location-known))

(use-package dired
  :custom
  (dired-recursive-copies 'always)
  (dired-dwim-target t))                ; try to guess target directory for copy

(use-package ediff
  :custom
  (ediff-keep-variants         nil)
  (ediff-show-clashes-only     t)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package fontaine
  :ensure t
  :defer t
  :init
  (setq fontaine-latest-state-file
        (locate-user-emacs-file "fontaine-latest-state.eld"))
  (setq fontaine-presets
        '((regular)
          (t
           :default-family "Iosevka Nerd Font"
           :fixed-pitch-family "IBM Plex Mono"
           :variable-pitch-family "XCharter"
           :variable-pitch-height 1.1)))
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)
  (fontaine-set-preset
   (or (fontaine-restore-latest-preset) 'regular)))

(use-package nerd-icons
  :ensure t)

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

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

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
  :defer t)

(use-package consult
  :ensure t
  :bind (("C-x b"             . consult-buffer)
         ("C-x 4 b"           . consult-buffer-other-window)
         ("C-x 5 b"           . consult-buffer-other-frame)
         ("C-x p b"           . consult-project-buffer)
         ("M-y"               . consult-yank-pop)
         ("C-c m"             . consult-man)
         ("C-c i"             . consult-info)
         ([remap Info-search] . consult-info)
         ("M-g o"             . consult-outline)))

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

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h F" . helpful-function)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)
         ("C-h ." . helpful-at-point)))

(use-package avy
  :ensure t
  :bind   (("C-." . avy-goto-char-timer)
           :map isearch-mode-map
           ("C-." . avy-isearch))
  :custom
  (avy-keys '(?d ?a ?n ?e ?s ?r ?u ?c ?g))
  (avy-dispatch-alist '((?m . avy-action-mark)
                        (?i . avy-action-ispell)
                        (?  . avy-action-mark-to-char)
                        (?z . avy-action-zap-to-char)
                        (?, . avy-action-embark)
                        (?h . avy-action-helpful)
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

  (defun avy-action-helpful (pt)
    (save-excursion
      (goto-char pt)
      (helpful-at-point))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

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

(use-package lin
  :ensure t
  :init
  (lin-global-mode 1))

(use-package transpose-frame
  :ensure t
  :bind (("C-c f t"       . transpose-frame)
         ("C-c f <right>" . flop-frame)))

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
  :bind ("C-c w" . whitespace-cleanup)
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

(use-package compile
  :bind ("C-c l" . compile))

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
  (add-hook 'prog-mode-hook #'my/set-fill-column))

(use-package eldoc
  :custom
  (eldoc-echo-area-use-multiline-p nil))

(use-package eglot
  :bind
  (:map eglot-mode-map
        ("C-c l f" . eglot-format)
        ("C-c l h" . eldoc)
        ("C-c l o" . eglot-code-action-organize-imports)
        ("C-c l q" . eglot-code-action-quickfix)
        ("C-c l r" . eglot-rename))
  :config
  (add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode 0))))

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
    (add-hook 'completion-at-point-functions #'cape-file 90 t)
    (add-hook 'completion-at-point-functions #'cape-dabbrev 91 t)
    (add-hook 'completion-at-point-functions #'cape-dict 92 t))
  (add-hook 'text-mode-hook #'my/disable-indent-tabs-mode)
  (add-hook 'text-mode-hook #'my/configure-capfs-text-mode))

(use-package flymake-vale
  :load-path "lisp/flymake-vale"
  :commands flymake-vale-load)

(use-package visual-line-mode
  :hook (text-mode . visual-line-mode))

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
  (add-hook 'emacs-lisp-mode-hook #'my/disable-indent-tabs-mode))

(use-package c-ts-mode
  :after eglot
  :defer t
  :init
  (add-hook 'c-ts-mode-hook #'eglot-ensure)
  :bind
  (:map c-ts-mode-map
        ("C-c o" . ff-find-other-file))
  :custom
  (c-ts-mode-indent-style 'linux)
  (c-ts-mode-indent-offset 8))

(use-package cc-mode
  :init
  (defun my/c-set-style ()
    "Set the C style to \"linux\"."
    (c-set-style "linux"))

  (add-hook 'c-mode-hook #'my/c-set-style))

(use-package markdown-mode
  :ensure t
  :init
  (add-hook 'markdown-mode-hook 'eglot-ensure)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode))
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
  :bind ("C-M-i" . completion-at-point))

(use-package yaml-ts-mode
  :ensure t
  :mode "\\.ya?ml\\'")

(use-package eshell
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

  ;; # After each tagging operation, signal the window manager status bar
  ;; # updater.
  (notmuch-after-tag-hook
   (lambda ()
     (shell-command-to-string "kill -SIGRTMIN+1 $(cat /tmp/mtstatus.pid)")))

  :config
  (add-hook 'notmuch-message-mode-hook #'turn-off-auto-fill)
  (add-hook 'notmuch-mua-send-hook #'notmuch-mua-attachment-check)
  (add-hook 'notmuch-show-mode-hook (lambda () (variable-pitch-mode 1))))

(use-package kbd-mode
  :load-path "lisp"
  :mode "\\.kbd\\'"
  :custom
  (kbd-mode-kill-kmonad "pkill -9 kmonad")
  (kbd-mode-start-kmonad "kmonad ~/.config/kmonad/kd87.kbd"))

(use-package pdf-tools
  :ensure t
  :magic ("%PDF" . pdf-view-mode)
  :init (pdf-tools-install)
  :hook
  ((pdf-view-mode . pdf-history-minor-mode)
   (pdf-view-mode . pdf-view-fit-page-to-window)
   (pdf-view-mode . pdf-view-auto-slice-minor-mode)
   (pdf-view-mode . pdf-view-midnight-minor-mode))
  :custom
  (pdf-view-midnight-colors '("#ffffff" . "#121212")))

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode))

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

(use-package winner
  :config
  (winner-mode 1))

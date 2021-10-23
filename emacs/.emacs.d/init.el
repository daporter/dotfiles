;;; init.el --- Personal configuration file

;;; Commentary:

;;; Code:

;;;; Initial Settings

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(require 'vc)
(setq vc-follow-symlinks t) ; because my dotfiles are managed this way

(add-to-list 'load-path (locate-user-emacs-file "lisp"))
(add-to-list 'load-path (locate-user-emacs-file "prot-lisp"))

;;;;; Set PATH

(unless (package-installed-p 'exec-path-from-shell)
  (package-install 'exec-path-from-shell))
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;;;;; Some Basic Settings

(setq frame-title-format '("%b"))
(setq ring-bell-function 'ignore)

(setq use-short-answers t)

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'overwrite-mode 'disabled t)

(setq initial-buffer-choice t)          ; always start with *scratch*

(define-key global-map (kbd "C-c p") #'package-list-packages)

;;;; Base Settings

(setq custom-file (make-temp-file "emacs-custom-")) ; disable it!

;;;;; Common Custom Functions (prot-simple.el)

(require 'prot-simple)
(setq help-window-select t)
(prot-simple-rename-help-buffers 1)

;;;;;; Highlight Cursor Position

(require 'prot-pulse)
(setq prot-pulse-pulse-command-list
      '(recenter-top-bottom
        move-to-window-line-top-bottom
        reposition-window
        bookmark-jump
        other-window))
(prot-pulse-advice-commands-mode 1)
(define-key global-map (kbd "C-x l") #'prot-pulse-pulse-line)

;;;;; Modus Themes

(setq modus-themes-bold-constructs t
      modus-themes-slanted-constructs t
      modus-themes-completions 'opinionated
      modus-themes-lang-checkers 'subtle-foreground-straight-underline
      modus-themes-region 'bg-only
      modus-themes-diffs 'desaturated
      modus-themes-org-blocks 'grayscale
      modus-themes-headings '((t . section))
      modus-themes-variable-pitch-headings t)

(load-theme 'modus-operandi)

;;;;; Typeface Configurations

;;;;;; Font Configurations

(require 'prot-fonts)
(setq x-underline-at-descent-line t)
(setq-default text-scale-remap-header-line t)

(setq prot-fonts-typeface-sets-alist
      '((thinkpad . ( :fixed-pitch-family "Hack"
                      :fixed-pitch-regular-weight normal
                      :fixed-pitch-heavy-weight bold
                      :fixed-pitch-height 80
                      :fixed-pitch-line-spacing nil
                      :variable-pitch-family "Noto Serif"
                      :variable-pitch-height 1.1
                      :variable-pitch-regular-weight normal))

        (external . ( :fixed-pitch-family "Hack"
                      :fixed-pitch-regular-weight normal
                      :fixed-pitch-heavy-weight bold
                      :fixed-pitch-height 90
                      :fixed-pitch-line-spacing 1
                      :variable-pitch-family "Noto Serif"
                      :variable-pitch-height 1.1
                      :variable-pitch-regular-weight normal))

        (macbook . ( :fixed-pitch-family "Fira Code"
                     :fixed-pitch-regular-weight normal
                     :fixed-pitch-heavy-weight semibold
                     :fixed-pitch-height 120
                     :fixed-pitch-line-spacing 1
                     :variable-pitch-family "Noto Serif"
                     :variable-pitch-height 1.1
                     :variable-pitch-regular-weight normal))))

(setq prot-fonts-laptop-desktop-keys-list
      (prot-fonts-laptop-desktop-keys))

(setq prot-fonts-max-small-resolution-width 1366)

(prot-fonts-fonts-per-monitor)
(add-hook 'modus-themes-after-load-theme-hook #'prot-fonts-restore-last)
(define-key global-map (kbd "C-c f") #'prot-fonts-set-fonts)

;;;;; Repeatable Keychords

(repeat-mode 1)

;;;;; Keychord Hints

(unless (package-installed-p 'which-key)
  (package-install 'which-key))
(require 'which-key)

(setq which-key-idle-delay 0.8)
(setq which-key-idle-secondary-delay 0.05)
(setq which-key-add-column-padding 2)

(which-key-mode 1)

;;;; Selection Candidates and Search Methods

;;;;; Completion Frameword and Extras

;;;;;; Orderless Completion Style

(unless (package-installed-p 'orderless)
  (package-install 'orderless))
(require 'orderless)

(defun dp-orderless-literal-dispatcher (pattern _index _total)
  "Literal style dispatcher using the equals sign as a suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
  (when (string-suffix-p "=" pattern)
    `(orderless-literal . ,(substring pattern 0 -1))))

(defun dp-orderless-initialism-dispatcher (pattern _index _total)
  "Leading initialism  dispatcher using the comma suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
  (when (string-suffix-p "," pattern)
    `(orderless-strict-leading-initialism . ,(substring pattern 0 -1))))

(defun dp-orderless-flex-dispatcher (pattern _index _total)
  "Flex  dispatcher using the tilde suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
  (when (string-suffix-p "~" pattern)
    `(orderless-flex . ,(substring pattern 0 -1))))

(defvar orderless-matching-styles)

(defun dp-orderless-with-styles (cmd &optional styles)
  "Call CMD with optional orderless STYLES.

STYLES is a list of pattern matching methods that is passed to
`orderless-matching-styles'.  Its fallback value is that of
`dp-orderless-alternative-styles'."
  (let ((orderless-matching-styles (or styles dp-orderless-alternative-styles))
        (this-command cmd))
    (call-interactively cmd)))

(setq dp-orderless-default-styles
      '(orderless-prefixes
        orderless-strict-leading-initialism
        orderless-regexp))
(setq dp-orderless-alternative-styles
      '(orderless-literal
        orderless-prefixes
        orderless-strict-leading-initialism
        orderless-regexp))

(setq orderless-component-separator " +")
(setq orderless-matching-styles dp-orderless-default-styles)
(setq orderless-style-dispatchers
      '(dp-orderless-literal-dispatcher
        dp-orderless-initialism-dispatcher
        dp-orderless-flex-dispatcher))
;; SPC should never complete: use it for `orderless' groups.
(let ((map minibuffer-local-completion-map))
  (define-key map (kbd "SPC") nil)
  (define-key map (kbd "?") nil))

;;;;;; Completion Annotations (Marginalia)

(unless (package-installed-p 'marginalia)
  (package-install 'marginalia))
(require 'marginalia)

(marginalia-mode 1)

;;;;;; Minibuffer Configurations

(setq completion-styles
      '(substring initials flex partial-completion orderless))
(setq completion-category-overrides
      '((file (styles . (partial-completion orderless)))))
(setq completion-cycle-threshold 2)
(setq completion-flex-nospace nil)
(setq completion-pcm-complete-word-inserts-delimiters nil)
(setq completion-pcm-word-delimiters "-_./:| ")
(setq completion-ignore-case t)
(setq-default case-fold-search t)   ; For general regexp

;; Grouping of completions for Emacs 28
(setq completions-group t)
(setq completions-group-sort nil)
(setq completions-group-format
      (concat
       (propertize "    " 'face 'completions-group-separator)
       (propertize " %s " 'face 'completions-group-title)
       (propertize " " 'face 'completions-group-separator
                   'display '(space :align-to right))))

(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

(setq enable-recursive-minibuffers t)
(setq read-answer-short t) ; also check `use-short-answers' for Emacs28
(setq resize-mini-windows t)
(setq minibuffer-eldef-shorten-default t)

(setq echo-keystrokes 0.25)           ; from the C source code

(file-name-shadow-mode 1)
(minibuffer-depth-indicate-mode 1)
(minibuffer-electric-default-mode 1)

(let ((map minibuffer-local-must-match-map))
  ;; I use this prefix for other searches
  (define-key map (kbd "M-s") nil))

(require 'mct)
(setq mct-remove-shadowed-file-names t) ; when `file-name-shadow-mode' is enabled
(setq mct-hide-completion-mode-line t)
(setq mct-show-completion-line-numbers nil)
(setq mct-apply-completion-stripes t)
(setq mct-minimum-input 3)
(setq mct-live-update-delay 0.6)
(setq mct-completion-blocklist nil)
(setq mct-completion-passlist '(embark-prefix-help-command
                                Info-goto-node
                                Info-index
                                Info-menu
                                vc-retrieve-tag
                                prot-bookmark-cd-bookmark
                                prot-bongo-playlist-insert-playlist-file))

(mct-mode 1)

(define-key global-map (kbd "C-x :") #'mct-focus-mini-or-completions)

;;;;;; Enhanced Minibuffer Commands (consult.el)

(unless (package-installed-p 'consult)
  (package-install 'consult))
(require 'consult)

(setq completion-in-region-function #'consult-completion-in-region)
(setq consult-narrow-key ">")
(setq consult-imenu-config
      '((emacs-lisp-mode :toplevel "Functions"
                         :types ((?f "Functions" font-lock-function-name-face)
                                 (?m "Macros"    font-lock-keyword-face)
                                 (?p "Packages"  font-lock-constant-face)
                                 (?t "Types"     font-lock-type-face)
                                 (?v "Variables" font-lock-variable-name-face)))))
(setq consult-bookmark-narrow
      `((?d "Docview" ,#'doc-view-bookmark-jump)
        (?e "Eshell" ,#'eshell-bookmark-jump)
        (?f "File" ,#'bookmark-default-handler)
        (?h "Help" ,#'help-bookmark-jump)
        (?i "Info" ,#'Info-bookmark-jump)
        (?m "Man" ,#'Man-bookmark-jump)
        (?p "PDF" ,#'pdf-view-bookmark-jump)
        (?v "VC Dir" ,#'vc-dir-bookmark-jump)
        (?w "EWW" ,#'prot-eww-bookmark-jump)))
(setq register-preview-delay 0.8
      register-preview-function #'consult-register-format)
(setq consult-find-args "find . -not ( -wholename */.* -prune )")
(setq consult-preview-key 'any)

(let ((map global-map))
  (define-key map (kbd "C-x r b") #'consult-bookmark) ; override `bookmark-jump'
  (define-key map (kbd "C-x M-:") #'consult-complex-command)
  (define-key map (kbd "C-x M-m") #'consult-minor-mode-menu)
  (define-key map (kbd "C-x M-k") #'consult-kmacro)
  (define-key map (kbd "M-g M-g") #'consult-goto-line)
  (define-key map (kbd "M-K") #'consult-keep-lines) ; M-S-k is similar to M-S-5 (M-%)
  (define-key map (kbd "M-F") #'consult-focus-lines) ; same principle
  (define-key map (kbd "M-s M-b") #'consult-buffer)
  (define-key map (kbd "M-s M-f") #'consult-find)
  (define-key map (kbd "M-s M-g") #'consult-grep)
  (define-key map (kbd "M-s M-m") #'consult-mark)
  (define-key map (kbd "C-x r r") #'consult-register)) ; Use the register's prefix
(define-key consult-narrow-map (kbd "?") #'consult-narrow-help)

(require 'prot-consult)
(setq consult-project-root-function #'prot-consult-project-root)
(setq prot-consult-command-centre-list
      '(consult-line
        prot-consult-line
        consult-mark))
(setq prot-consult-command-top-list
      '(consult-outline
        consult-imenu
        prot-consult-outline
        prot-consult-imenu))
(prot-consult-set-up-hooks-mode 1)
(let ((map global-map))
  (define-key map (kbd "M-s M-i") #'prot-consult-imenu)
  (define-key map (kbd "M-s M-s") #'prot-consult-outline)
  (define-key map (kbd "M-s M-y") #'prot-consult-yank)
  (define-key map (kbd "M-s M-l") #'prot-consult-line))

;;;;;;; Switch to Directories (consult-dir.el)

(unless (package-installed-p 'consult-dir)
  (package-install 'consult-dir))
(require 'consult-dir)

(setq consult-dir-sources '(consult-dir--source-bookmark
                            consult-dir--source-default
                            consult-dir--source-project
                            consult-dir--source-recentf))

;; Overrides `list-directory' in the `global-map', though I never used
;; that anyway.
(dolist (map (list global-map minibuffer-local-filename-completion-map))
  (define-key map (kbd "C-x C-d") #'consult-dir))

;;;;;; Extended Minibuffer Actions (embark.el)

(unless (package-installed-p 'embark)
  (package-install 'embark))
(require 'embark)

(setq prefix-help-command #'embark-prefix-help-command)
(setq embark-collect-initial-view-alist '((t . list)))
(setq embark-cycle-key (kbd "C-,"))     ; see the `embark-act' key
(setq embark-indicator #'embark-mixed-indicator)
(setq embark-verbose-indicator-excluded-actions
      '("\\`embark-collect-" "\\`customize-" "\\(local\\|global\\)-set-key"
        set-variable embark-cycle embark-export
        embark-keymap-help embark-become embark-isearch))
(setq embark-verbose-indicator-display-action nil)

;; Use alternating backgrounds, if `stripes' is available.
(with-eval-after-load 'stripes
  (let ((hook 'embark-collect-mode-hook))
    (add-hook hook #'stripes-mode)
    (add-hook hook #'hl-line-mode)))

(define-key global-map (kbd "C-,") #'embark-act)
(let ((map minibuffer-local-completion-map))
  (define-key map (kbd "C-,") #'embark-act)
  (define-key map (kbd "C->") #'embark-become)
  (define-key map (kbd "M-q") #'embark-collect-toggle-view))
(let ((map embark-collect-mode-map))
  (define-key map (kbd "C-,") #'embark-act)
  (define-key map (kbd "M-q") #'embark-collect-toggle-view))
(let ((map embark-region-map))
  (define-key map (kbd "a") #'align-regexp)
  (define-key map (kbd "i") #'epa-import-keys-region)
  (define-key map (kbd "r") #'repunctuate-sentences) ; overrides `rot13-region'
  (define-key map (kbd "s") #'sort-lines)
  (define-key map (kbd "u") #'untabify))
(let ((map embark-symbol-map))
  (define-key map (kbd ".") #'embark-find-definition)
  (define-key map (kbd "k") #'describe-keymap))

(unless (package-installed-p 'embark-consult)
  (package-install 'embark-consult))
(require 'embark-consult)

(require 'prot-embark)
(prot-embark-keymaps 1)
(prot-embark-setup-packages 1)

;;;;;; Projects

(require 'project)

(setq project-switch-commands
      '((?f "File" project-find-file)
        (?s "Subdir" prot-project-find-subdir)
        (?g "Grep" project-find-regexp)
        (?d "Dired" project-dired)
        (?b "Buffer" project-switch-to-buffer)
        (?q "Query replace" project-query-replace-regexp)
        (?t "Tag switch" prot-project-retrieve-tag)
        (?m "Magit" prot-project-magit-status)
        (?v "VC dir" project-vc-dir)
        (?l "Log VC" prot-project-commit-log)
        (?e "Eshell" project-eshell)))

(define-key global-map (kbd "C-x p q") #'project-query-replace-regexp)

;;;; Directory, Buffer, Window Management

;;;;; Dired

(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq delete-by-moving-to-trash t)
(setq dired-listing-switches "-AFGhlv")
(setq dired-dwim-target t)
(setq dired-auto-revert-buffer #'dired-directory-changed-p) ; also see `dired-do-revert-buffer'

(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook #'hl-line-mode)

;;;;; Working With Buffers

;;;;;; Keymap for Buffers

(let ((map ctl-x-x-map))
  (define-key map "e" #'eval-buffer)
  (define-key map "f" #'follow-mode)  ; override `font-lock-update'
  (define-key map "r" #'rename-uniquely))

;;;;;; Unique Names for Buffers

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;;;;; Ibuffer and Extras

(require 'ibuffer)
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-formats
      '((mark modified read-only locked " "
              (name 40 40 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " " filename-and-process)
        (mark " "
              (name 16 -1)
              " " filename)))
(add-hook 'ibuffer-mode-hook #'hl-line-mode)
(define-key global-map (kbd "C-x C-b") #'ibuffer)
(let ((map ibuffer-mode-map))
  (define-key map (kbd "* f") #'ibuffer-mark-by-file-name-regexp)
  (define-key map (kbd "* g") #'ibuffer-mark-by-content-regexp) ; "g" is for "grep"
  (define-key map (kbd "* n") #'ibuffer-mark-by-name-regexp)
  (define-key map (kbd "s n") #'ibuffer-do-sort-by-alphabetic)  ; "sort name" mnemonic
  (define-key map (kbd "/ g") #'ibuffer-filter-by-content))

;;;;; Window Configuration

;;;;;; Window Rules and Basic Tweaks

(setq display-buffer-alist
      `(;; no window
        ;;("\\`\\*Async Shell Command\\*\\'"
        ;; (display-buffer-no-window))
        ;; top side window
        ("\\**prot-elfeed-bongo-queue.*"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (window-height . 0.16)
         (side . top)
         (slot . -2))
        ("\\*\\(prot-elfeed-mpv-output\\|world-clock\\).*"
         (display-buffer-in-side-window)
         (window-height . 0.16)
         (side . top)
         (slot . -1))
        ("\\*\\(Flymake diagnostics\\|Package-Lint\\).*"
         (display-buffer-in-side-window)
         (window-height . 0.16)
         (side . top)
         (slot . 0))
        ("\\*Messages.*"
         (display-buffer-in-side-window)
         (window-height . 0.16)
         (side . top)
         (slot . 1))
        ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Flymake log\\)\\*"
         (display-buffer-in-side-window)
         (window-height . 0.16)
         (side . top)
         (slot . 2))
        ;; left side window
        ("\\*Help\\*"             ; See the hooks for `visual-line-mode'
         (display-buffer-in-side-window)
         (window-width . 0.25)
         (side . left)
         (slot . 0))
        ;; right side window
        ("\\*keycast\\*"
         (display-buffer-in-side-window)
         (dedicated . t)
         (window-width . 0.25)
         (side . right)
         (slot . -1)
         (window-parameters . ((no-other-window . t)
                               (mode-line-format . none))))
        ;; bottom side window
        ("\\*Org Select\\*"
         (display-buffer-in-side-window)
         (dedicated . t)
         (side . bottom)
         (slot . 0)
         (window-parameters . ((mode-line-format . none))))
        ;; bottom buffer (NOT side window)
        ("\\*Embark Actions\\*"
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (side . bottom)
         (slot . -1)
         (window-height . fit-window-to-buffer)
         (window-parameters . ((no-other-window . t)
                               (mode-line-format . none))))
        ("\\*\\(Embark\\)?.*Completions.*"
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (side . bottom)
         (slot . 0)
         (window-parameters . ((no-other-window . t))))
        ("\\*\\(Output\\|Register Preview\\).*"
         (display-buffer-reuse-mode-window display-buffer-at-bottom))
        ;; below current window
        ("\\*.*\\(e?shell\\|v?term\\).*"
         (display-buffer-reuse-mode-window display-buffer-below-selected))
        ("\\*\\vc-\\(incoming\\|outgoing\\|git : \\).*"
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         ;; NOTE 2021-10-06: we cannot `fit-window-to-buffer' because
         ;; the size is not known in advance.
         (window-height . 0.2))
        ("\\*\\(Calendar\\|Bookmark Annotation\\).*"
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (window-height . fit-window-to-buffer))))
(setq window-combination-resize t)
(setq even-window-sizes 'height-only)
(setq switch-to-buffer-in-dedicated-window 'pop)

(add-hook 'help-mode-hook #'visual-line-mode)
(add-hook 'custom-mode-hook #'visual-line-mode)

(let ((map global-map))
  (define-key map (kbd "C-x <down>") #'next-buffer)
  (define-key map (kbd "C-x <up>") #'previous-buffer)
  (define-key map (kbd "C-x C-n") #'next-buffer)     ; override `set-goal-column'
  (define-key map (kbd "C-x C-p") #'previous-buffer) ; override `mark-page'
  (define-key map (kbd "C-x !") #'delete-other-windows-vertically)
  (define-key map (kbd "C-x _") #'balance-windows)      ; underscore
  (define-key map (kbd "C-x -") #'fit-window-to-buffer) ; hyphen
  (define-key map (kbd "C-x +") #'balance-windows-area)
  (define-key map (kbd "C-x }") #'enlarge-window)
  (define-key map (kbd "C-x {") #'shrink-window)
  (define-key map (kbd "C-x >") #'enlarge-window-horizontally) ; override `scroll-right'
  (define-key map (kbd "C-x <") #'shrink-window-horizontally)) ; override `scroll-left'
(let ((map resize-window-repeat-map))
  (define-key map ">" #'enlarge-window-horizontally)
  (define-key map "<" #'shrink-window-horizontally))

;;;;;; Window History (winner-mode)

(add-hook 'after-init-hook #'winner-mode)

;;;;;; Quickly Switch Windows (ace-window)

(unless (package-installed-p 'ace-window)
  (package-install 'ace-window))
(require 'ace-window)

(setq aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n))
(setq aw-dispatch-alist
      '((?b aw-switch-buffer-in-window "Select Buffer")
        (?B aw-switch-buffer-other-window "Switch Buffer Other Window")
        (?s aw-swap-window "Swap Windows")
        (?c aw-copy-window "Copy Window")
        (?m aw-move-window "Move Window")
        (?x aw-delete-window "Delete Window")
        (?O delete-other-windows "Delete Other Windows")
        (?f aw-flip-window)
        (?+ aw-split-window-fair "Split Fair Window")
        (?- aw-split-window-vert "Split Vert Window")
        (?| aw-split-window-horz "Split Horz Window")
        (?? aw-show-dispatch-help)))

(define-key global-map (kbd "C-x o") #'ace-window)

(eval-when-compile
  (defmacro my/embark-ace-action (fn)
    `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
       (interactive)
       (with-demoted-errors "%s"
         (require 'ace-window)
         (let ((aw-dispatch-always t))
           (aw-switch-to-window (aw-select nil))
           (call-interactively (symbol-function ',fn)))))))

(define-key embark-file-map (kbd "o")
            (my/embark-ace-action find-file))
(define-key embark-buffer-map (kbd "o")
            (my/embark-ace-action switch-to-buffer))
(define-key embark-bookmark-map (kbd "o")
            (my/embark-ace-action bookmark-jump))


;;;;;; Tabs for Window Layouts

(setq tab-bar-close-button-show nil)
(setq tab-bar-show nil)

(tab-bar-mode -1)                     ; see `prot-tab-status-line'
(tab-bar-history-mode 1)

(require 'prot-tab)

(setq tab-bar-format
      '(prot-tab-format-space-single
        prot-tab-format-mule-info
        prot-tab-format-modified
        tab-bar-format-tabs-groups
        prot-tab-format-space-double
        prot-tab-format-position
        prot-tab-format-space-double
        prot-tab-format-vc
        prot-tab-format-space-double
        prot-tab-format-modes
        tab-bar-format-align-right
        prot-tab-format-misc-info
        prot-tab-format-space-double
        tab-bar-format-global
        prot-tab-format-space-single))

(add-hook 'after-init-hook #'prot-tab-status-line)

(let ((map global-map))
  (define-key map (kbd "C-x <right>") #'prot-tab-winner-redo)
  (define-key map (kbd "C-x <left>") #'prot-tab-winner-undo)
  (define-key map (kbd "C-<f8>") #'prot-tab-status-line) ; unopinionated alternative: `prot-tab-bar-toggle'
  (define-key map (kbd "C-x t t") #'prot-tab-select-tab-dwim))

;;;;;; Transposition and Rotation of Windows

(unless (package-installed-p 'transpose-frame)
  (package-install 'transpose-frame))

(define-key global-map (kbd "C-x M-r") #'rotate-frame-clockwise)

;;;; Applications and Utilities

;;;;; Bookmarking

(setq bookmark-use-annotations nil)
(setq bookmark-automatically-show-annotations t)
(setq bookmark-set-fringe-mark t)

(add-hook 'bookmark-bmenu-mode-hook #'hl-line-mode)

(require 'prot-bookmark)
(prot-bookmark-extra-keywords 1)

;;;;; Focus Mode

(require 'face-remap)

(unless (package-installed-p 'olivetti)
  (package-install 'olivetti))
(require 'olivetti)

(require 'prot-logos)
(require 'prot-cursor)
(setq prot-logos-affect-prot-cursor t)
(define-key global-map (kbd "C-c l") #'prot-logos-focus-mode)

;;;;;; Version Control Tools

;;;;;;; Diff Mode

(setq diff-default-read-only t)
(setq diff-refine nil)         ; I do it on demand
;; The following is further controlled by
;; `prot-diff-modus-themes-diffs'
(setq diff-font-lock-syntax 'hunk-also)

(require 'prot-diff)
(prot-diff-modus-themes-diffs)
(add-hook 'modus-themes-after-load-theme-hook #'prot-diff-modus-themes-diffs)

(prot-diff-extra-keywords 1)

;; `prot-diff-buffer-dwim' replaces the default for `vc-diff' (which I
;; bind to another key---see VC section).
(define-key global-map (kbd "C-x v =") #'prot-diff-buffer-dwim)
(let ((map diff-mode-map))
  (define-key map (kbd "C-c C-b") #'prot-diff-refine-cycle) ; replace `diff-refine-hunk'
  (define-key map (kbd "C-c C-n") #'prot-diff-narrow-dwim))

;;;;;;; Version Control Framework (vc.el and prot-vc.el)

(require 'vc)
;; Those offer various types of functionality, such as blaming,
;; viewing logs, showing a dedicated buffer with changes to affected
;; files.
(require 'vc-annotate)
(require 'vc-dir)
(require 'vc-git)
(require 'add-log)
(require 'log-view)

;; This one is for editing commit messages.
(require 'log-edit)
(setq log-edit-confirm 'changed)
(setq log-edit-keep-buffer nil)
(setq log-edit-require-final-newline t)
(setq log-edit-setup-add-author nil)

;; Note that `prot-vc-git-setup-mode' will run the following when
;; activated:
;;
;;   (remove-hook 'log-edit-hook #'log-edit-show-files)
;;
;; If you need the window to pop back up, do it manually with C-c C-f
;; which calls `log-edit-show-files'.

(setq vc-find-revision-no-save t)
(setq vc-annotate-display-mode 'scale) ; scale to oldest
;; I use a different account for git commits
(setq add-log-mailing-address "info@protesilaos.com")
(setq add-log-keep-changes-together t)
(setq vc-git-diff-switches '("--patch-with-stat" "--histogram"))
(setq vc-git-print-log-follow t)
(setq vc-git-revision-complete-only-branches nil) ; Emacs 28
(setq vc-git-root-log-format
      '("%d %h %ad %an: %s"
        ;; The first shy group matches the characters drawn by --graph.
        ;; We use numbered groups because `log-view-message-re' wants the
        ;; revision number to be group 1.
        "^\\(?:[*/\\|]+\\)\\(?:[*/\\| ]+\\)?\
\\(?2: ([^)]+) \\)?\\(?1:[0-9a-z]+\\) \
\\(?4:[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\) \
\\(?3:.*?\\):"
        ((1 'log-view-message)
         (2 'change-log-list nil lax)
         (3 'change-log-name)
         (4 'change-log-date))))

(add-hook 'log-view-mode-hook #'hl-line-mode)

;; NOTE: I override lots of the defaults
(let ((map global-map))
  (define-key map (kbd "C-x v b") #'vc-retrieve-tag)  ; "branch" switch
  (define-key map (kbd "C-x v t") #'vc-create-tag)
  (define-key map (kbd "C-x v f") #'vc-log-incoming)  ; the actual git fetch
  (define-key map (kbd "C-x v o") #'vc-log-outgoing)
  (define-key map (kbd "C-x v F") #'vc-update)        ; "F" because "P" is push
  (define-key map (kbd "C-x v d") #'vc-diff))
(let ((map vc-dir-mode-map))
  (define-key map (kbd "b") #'vc-retrieve-tag)
  (define-key map (kbd "t") #'vc-create-tag)
  (define-key map (kbd "O") #'vc-log-outgoing)
  (define-key map (kbd "o") #'vc-dir-find-file-other-window)
  (define-key map (kbd "f") #'vc-log-incoming) ; replaces `vc-dir-find-file' (use RET)
  (define-key map (kbd "F") #'vc-update)       ; symmetric with P: `vc-push'
  (define-key map (kbd "d") #'vc-diff)         ; parallel to D: `vc-root-diff'
  (define-key map (kbd "k") #'vc-dir-clean-files)
  (define-key map (kbd "G") #'vc-revert)
  (let ((prot-vc-git-branch-map (make-sparse-keymap)))
    (define-key map "B" prot-vc-git-branch-map)
    (define-key prot-vc-git-branch-map "n" #'vc-create-tag) ; new branch/tag
    (define-key prot-vc-git-branch-map "s" #'vc-retrieve-tag) ; switch branch/tag
    (define-key prot-vc-git-branch-map "c" #'prot-vc-git-checkout-remote) ; "checkout" remote
    (define-key prot-vc-git-branch-map "l" #'vc-print-branch-log))
  (let ((prot-vc-git-stash-map (make-sparse-keymap)))
    (define-key map "S" prot-vc-git-stash-map)
    (define-key prot-vc-git-stash-map "c" 'vc-git-stash) ; "create" named stash
    (define-key prot-vc-git-stash-map "s" 'vc-git-stash-snapshot)))
(let ((map vc-git-stash-shared-map))
  (define-key map "a" 'vc-git-stash-apply-at-point)
  (define-key map "c" 'vc-git-stash) ; "create" named stash
  (define-key map "D" 'vc-git-stash-delete-at-point)
  (define-key map "p" 'vc-git-stash-pop-at-point)
  (define-key map "s" 'vc-git-stash-snapshot))
(let ((map vc-annotate-mode-map))
  (define-key map (kbd "M-q") #'vc-annotate-toggle-annotation-visibility)
  (define-key map (kbd "C-c C-c") #'vc-annotate-goto-line)
  (define-key map (kbd "<return>") #'vc-annotate-find-revision-at-line))
(let ((map log-view-mode-map))
  (define-key map (kbd "<tab>") #'log-view-toggle-entry-display)
  (define-key map (kbd "<return>") #'log-view-find-revision)
  (define-key map (kbd "s") #'vc-log-search)
  (define-key map (kbd "o") #'vc-log-outgoing)
  (define-key map (kbd "f") #'vc-log-incoming)
  (define-key map (kbd "F") #'vc-update)
  (define-key map (kbd "P") #'vc-push))

(require 'prot-vc)
(setq prot-vc-log-limit 100)
(setq prot-vc-log-bulk-action-limit 50)
(setq prot-vc-git-log-edit-show-commits t)
(setq prot-vc-git-log-edit-show-commit-count 10)
(setq prot-vc-shell-output "*prot-vc-output*")
(setq prot-vc-patch-output-dirs (list "~/" "~/Desktop/"))
(add-to-list' log-edit-headers-alist '("Amend"))

;; This refashions log view and log edit buffers
(prot-vc-git-setup-mode 1)

;; NOTE: I override lots of the defaults
(let ((map global-map))
  (define-key map (kbd "C-x v i") #'prot-vc-git-log-insert-commits)
  (define-key map (kbd "C-x v p") #'prot-vc-project-or-dir)
  (define-key map (kbd "C-x v SPC") #'prot-vc-custom-log)
  (define-key map (kbd "C-x v g") #'prot-vc-git-grep)
  (define-key map (kbd "C-x v G") #'prot-vc-git-log-grep)
  (define-key map (kbd "C-x v a") #'prot-vc-git-patch-apply)
  (define-key map (kbd "C-x v c") #'prot-vc-git-patch-create-dwim)
  (define-key map (kbd "C-x v s") #'prot-vc-git-show)
  (define-key map (kbd "C-x v r") #'prot-vc-git-find-revision)
  (define-key map (kbd "C-x v B") #'prot-vc-git-blame-region-or-file)
  (define-key map (kbd "C-x v R") #'prot-vc-git-reset))
(let ((map vc-git-log-edit-mode-map))
  (define-key map (kbd "C-C C-n") #'prot-vc-git-log-edit-extract-file-name)
  (define-key map (kbd "C-C C-i") #'prot-vc-git-log-insert-commits)
  ;; Also done by `prot-vc-git-setup-mode', but I am putting it here
  ;; as well for visibility.
  (define-key map (kbd "C-c C-c") #'prot-vc-git-log-edit-done)
  (define-key map (kbd "C-c C-a") #'prot-vc-git-log-edit-toggle-amend)
  (define-key map (kbd "M-p") #'prot-vc-git-log-edit-previous-comment)
  (define-key map (kbd "M-n") #'prot-vc-git-log-edit-next-comment)
  (define-key map (kbd "M-s") #'prot-vc-git-log-edit-complete-comment)
  (define-key map (kbd "M-r") #'prot-vc-git-log-edit-complete-comment))
(let ((map log-view-mode-map))
  (define-key map (kbd "<C-tab>") #'prot-vc-log-view-toggle-entry-all)
  (define-key map (kbd "a") #'prot-vc-git-patch-apply)
  (define-key map (kbd "c") #'prot-vc-git-patch-create-dwim)
  (define-key map (kbd "R") #'prot-vc-git-log-reset)
  (define-key map (kbd "w") #'prot-vc-log-kill-hash))

;;;;;;; Magit

(unless (package-installed-p 'magit)
  (package-install 'magit))
(require 'magit)

(setq magit-define-global-key-bindings nil)
(define-key global-map (kbd "C-c g") #'magit-status)

(require 'git-commit)
(setq git-commit-summary-max-length 50)
(setq git-commit-known-pseudo-headers
      '("Signed-off-by"
        "Acked-by"
        "Modified-by"
        "Cc"
        "Suggested-by"
        "Reported-by"
        "Tested-by"
        "Reviewed-by"))
(setq git-commit-style-convention-checks
      '(non-empty-second-line
        overlong-summary-line))

(require 'magit-diff)
(setq magit-diff-refine-hunk t)

(require 'magit-repos)
(setq magit-repository-directories
      '(("~/src" . 1)))

;;;;;;; Smerge and Ediff

(require 'smerge-mode)

(require 'ediff)
(setq ediff-keep-variants nil)
(setq ediff-make-buffers-readonly-at-startup nil)
(setq ediff-merge-revisions-with-ancestor t)
(setq ediff-show-clashes-only t)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Tweak those for safer identification and removal
(setq ediff-combination-pattern
      '("<<<<<<< prot-ediff-combine Variant A" A
        ">>>>>>> prot-ediff-combine Variant B" B
        "####### prot-ediff-combine Ancestor" Ancestor
        "======= prot-ediff-combine End"))

;; TODO automate process in a robust way, or at least offer a good key
;; binding.
(defun prot/ediff-flush-combination-pattern ()
  "Remove my custom `ediff-combination-pattern' markers.

This is a quick-and-dirty way to get rid of the markers that are
left behind by `smerge-ediff' when combining the output of two
diffs.  While this could be automated via a hook, I am not yet
sure this is a good approach."
  (interactive)
  (flush-lines ".*prot-ediff.*" (point-min) (point-max) nil))

;;;;; Org Mode

(setq org-directory "~/Dropbox/org")
(setq org-list-allow-alphabetical t)

(setq org-cite-global-bibliography
      '("~/Dropbox/bibliography/bibliography.bib"))

;;;;;; CSL Export Processor

;; `citeproc’ is required by `oc-csl’
(unless (package-installed-p 'citeproc)
  (package-install 'citeproc))
(require 'citeproc)

(require 'oc-csl)
(setq org-cite-csl-styles-dir "~/Dropbox/bibliography")
(setq org-cite-export-processors
      '((t csl "modern-language-association.csl")))

;;;;;; Org-GTD

(unless (package-installed-p 'org-gtd)
  (package-install 'org-gtd))
(require 'org-gtd)

(setq org-gtd-directory "~/Dropbox/gtd/")

(setq org-agenda-property-list '("DELEGATED_TO"))

(setq org-edna-use-inheritance t)
(org-edna-mode 1)

(setq org-agenda-files `(,org-gtd-directory))

;; A useful view to see what can be accomplished today.
(setq org-agenda-custom-commands
      '(("g" "Scheduled today and all NEXT items" ((agenda "" ((org-agenda-span 1)))
                                                   (todo "NEXT")))))

(setq org-default-notes-file "~/Dropbox/inbox/notes.org")

(setq org-capture-templates
      `(("i" "Inbox"
         entry (file ,(org-gtd-inbox-path))
         "* %?\n%U\n\n  %i"
         :kill-buffer t)
        ("l" "Todo with link"
         entry (file ,(org-gtd-inbox-path))
         "* %?\n%U\n\n  %i\n  %a"
         :kill-buffer t)
        ("n" "Note"
         entry (file "")     ; use `org-default-notes-file’
         "* %<%Y-%m-%d %H:%M>\n\n%?\n%i")))

(let ((map global-map))
  (define-key map (kbd "C-c d a") (lambda ()
                                    (interactive)
                                    (org-agenda nil "g")))
  (define-key map (kbd "C-c d c") #'org-gtd-capture)
  (define-key map (kbd "C-c d f") #'org-gtd-clarify-finalize)
  (define-key map (kbd "C-c d n") #'org-gtd-show-all-next)
  (define-key map (kbd "C-c d p") #'org-gtd-process-inbox)
  (define-key map (kbd "C-c d s") #'org-gtd-show-stuck-projects))

;;;;;; Org Agenda

(setq org-agenda-include-diary t)
(setq org-agenda-restore-windows-after-quit t)
(setq org-agenda-sticky t)

;;;;;; Org Journal

(unless (package-installed-p 'org-journal)
  (package-install 'org-journal))
(require 'org-journal)

(setq org-journal-dir "~/Dropbox/journal")
(setq org-journal-file-format "%Y-%m-%d.org")
(setq org-journal-date-prefix "#+title: ")
(setq org-journal-date-format "%A, %d %B %Y")
(define-key global-map (kbd "C-c j") #'org-journal-new-entry)

;;;;;; Deft

(unless (package-installed-p 'deft)
  (package-install 'deft))
(require 'deft)

(setq deft-directory "~/Dropbox/zettelkasten")
(setq deft-default-extension "org")
(setq deft-use-filter-string-for-filename t)

(advice-add 'deft-parse-title :override
            (lambda (file contents)
              (if deft-use-filename-as-title
                  (deft-base-filename file)
                (let* ((case-fold-search 't)
                       (begin (string-match "title: " contents))
                       (end-of-begin (match-end 0))
                       (end (string-match "\n" contents begin)))
                  (if begin
                      (substring contents end-of-begin end)
                    (format "%s" file))))))

(setq deft-strip-summary-regexp
      (concat "\\("
              "[\n\t]"
              "\\|^#\\+[[:alpha:]_]+:.*$" ; org-mode metadata
              "\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n"
              "\\)"))

(define-key global-map (kbd "C-c z d") 'deft)

;;;;;; Org Roam

(setq org-roam-v2-ack t)

(unless (package-installed-p 'org-roam)
  (package-install 'org-roam))
(require 'org-roam)

(setq org-roam-directory "~/Dropbox/zettelkasten")
(org-roam-db-autosync-mode)

(let ((map global-map))
  (define-key map (kbd "C-c z i") #'org-roam-node-insert)
  (define-key map (kbd "C-c z f") #'org-roam-node-find))

;;;;; Anki Card Creation

(unless (package-installed-p 'anki-editor)
  (package-install 'anki-editor))
(require 'anki-editor)

(define-key org-mode-map (kbd "C-c a n") 'anki-editor-insert-note)
(define-key org-mode-map (kbd "C-c a c") 'anki-editor-cloze-region)
(define-key org-mode-map (kbd "C-c a p") 'anki-editor-push-notes)

(define-skeleton anki-vocab-basic
  "Skeleton of an Anki Basic note for vocab."
  nil
  "** Item\n"
  "    :PROPERTIES:\n"
  "    :ANKI_NOTE_TYPE: Basic\n"
  "    :END:\n"
  "*** Front\n"
  > _ "\n"
  "*** Back\n"
  "*** Extra\n"
  "*** Source\n"
  "    /Vocabulaire progressif du français - Débutant, 3^e édition/\n")

(define-skeleton anki-vocab-cloze
  "Skeleton of an Anki Cloze note for vocab."
  nil
  "** Item\n"
  "    :PROPERTIES:\n"
  "    :ANKI_NOTE_TYPE: Cloze\n"
  "    :END:\n"
  "*** Text\n"
  > _ "\n"
  "*** Extra\n"
  "*** Source\n"
  "    /Vocabulaire progressif du français - Débutant, 3^e édition/\n")

(define-skeleton anki-communication-basic
  "Skeleton of an Anki Basic note for communication."
  nil
  "** Item\n"
  "    :PROPERTIES:\n"
  "    :ANKI_NOTE_TYPE: Basic\n"
  "    :END:\n"
  "*** Front\n"
  > _ "\n"
  "*** Back\n"
  "*** Extra\n"
  "*** Source\n"
  "    /Communication progressive du français - Débutant, 2^e édition/\n")

(define-skeleton anki-communication-cloze
  "Skeleton of an Anki Cloze note for communication."
  nil
  "** Item\n"
  "    :PROPERTIES:\n"
  "    :ANKI_NOTE_TYPE: Cloze\n"
  "    :END:\n"
  "*** Text\n"
  > _ "\n"
  "*** Extra\n"
  "*** Source\n"
  "    /Communication progressive du français - Débutant, 2^e édition/\n")

;;;;; Zotero Reference Manager

(defun dp-insert-zotero-reference ()
  "Invoke the Zotero reference chooser and insert the chosen reference.
Note: Zotero must be running and the `Better BibTeX' extension
must be installed."
  (interactive)
  (shell-command
   "curl -s http://127.0.0.1:23119/better-bibtex/cayw?format=formatted-bibliography"
   t))

(defun dp-insert-zotero-citation ()
  "Invoke the Zotero reference chooser and insert the chosen citation.
Note: Zotero must be running and the `Better BibTeX' extension
must be installed."
  (interactive)
  (shell-command
   "curl -s http://127.0.0.1:23119/better-bibtex/cayw?format=formatted-citation"
   t))

(define-key global-map (kbd "C-c z r") #'dp-insert-zotero-reference)
(define-key global-map (kbd "C-c z c") #'dp-insert-zotero-citation)

;;;;; Email Settings

;;;;;; Client-Agnostic Email Settings

(unless (package-installed-p 'auth-source-pass)
  (package-install 'auth-source-pass))
(require 'auth-source-pass)

(auth-source-pass-enable)

(setq user-full-name "David Porter")
(setq user-mail-address "david@daporter.net")

(require 'message)
(setq mail-user-agent 'message-user-agent)
(setq message-mail-user-agent t)      ; use `mail-user-agent'
(setq mail-signature "David Porter\n")
(setq message-signature "David Porter\n")
(setq message-citation-line-format "On %Y-%m-%d, %R %z, %f wrote:\n")
(setq message-citation-line-function
      'message-insert-formatted-citation-line)
(setq message-confirm-send nil)
(setq message-kill-buffer-on-exit t)
(setq message-wide-reply-confirm-recipients t)
(add-to-list 'mm-body-charset-encoding-alist '(utf-8 . base64))

(add-hook 'message-setup-hook #'message-sort-headers)

(require 'gnus-dired)
(add-hook 'dired-mode-hook #'gnus-dired-mode) ; doesn’t require `gnus’

;;;;;; Notmuch

(unless (package-installed-p 'notmuch)
  (package-install 'notmuch))
(require 'notmuch)

;; Account Settings
(setq notmuch-identities '("David Porter <david@daporter.net>"))
(setq notmuch-fcc-dirs "Sent")

;; General UI
(setq notmuch-show-logo nil)
(setq notmuch-hello-thousands-separator "")

;; Search
(setq notmuch-search-result-format
      '(("date" . "%12s  ")
        ("count" . "%-7s  ")
        ("authors" . "%-20s  ")
        ("subject" . "%-70s  ")
        ("tags" . "(%s)")))
(setq notmuch-tree-result-format
      '(("date" . "%12s  ")
        ("authors" . "%-20s  ")
        ((("tree" . "%s") ("subject" . "%s")) . " %-70s  ")
        ("tags" . "(%s)")))
(setq notmuch-show-empty-saved-searches t)
(setq notmuch-saved-searches
      '((:name "unread (inbox)" :query "tag:unread and tag:inbox" :key "u")
        (:name "unread all" :query "tag:unread not tag:archived" :key "U")
        (:name "inbox" :query "tag:inbox" :key "i")
        (:name "flagged" :query "tag:flagged" :key "f")
        (:name "reference" :query "tag:reference not tag:archived" :key "r")
        (:name "todo" :query "tag:todo not tag:archived" :key "t")
        (:name "mailing lists" :query "tag:list" :key "m")
        (:name "emacs-humanities"
               :query "from:emacs-humanities@gnu.org or to:emacs-humanities@gnu.org"
               :key "eh")
        ;; CLI tools
        (:name "notmuch"
               :query "from:notmuch@notmuchmail.org or to:notmuch@notmuchmail.org"
               :key "cn")
        ;; Books
        (:name "great-conversation"
               :query "to:great-conversation@googlegroups.com or to:GreatConversation@yahoogroups.com"
               :key "bg")))

;; Tags
(setq notmuch-archive-tags '("-inbox" "-unread" "+archived"))
(setq notmuch-draft-folder "Drafts")

;; Email Composition
(setq notmuch-mua-cite-function 'message-cite-original-without-signature)
(setq notmuch-mua-user-agent-function #'notmuch-mua-user-agent-full)

;; Reading Messages
(setq notmuch-show-indent-messages-width 0)
(setq notmuch-wash-wrap-lines-length 100)

;; Hooks and Keybindings
(add-hook 'notmuch-mua-send-hook #'notmuch-mua-attachment-check)
(add-hook 'notmuch-show-hook (lambda ()
                               (setq-local header-line-format nil)))

;; Use alternating backgrounds, if `stripes' is available.
(with-eval-after-load 'stripes
  (add-hook 'notmuch-search-hook #'stripes-mode)
  ;; ;; To disable `hl-line-mode':
  ;; (setq notmuch-search-hook nil)
  ;; (add-hook 'notmuch-search-hook #'prot-common-disable-hl-line)
  )

(let ((map global-map))
  (define-key map (kbd "C-c m") #'notmuch)
  (define-key map (kbd "C-x m") #'notmuch-mua-new-mail)) ; override `compose-mail'

;;;;;; Sending Email

(require 'smtpmail)
(setq smtpmail-default-smtp-server "smtp.migadu.com")
(setq smtpmail-smtp-server "smtp.migadu.com")
(setq smtpmail-smtp-user "david@daporter.net")
(setq smtpmail-smtp-service 465)
(setq smtpmail-stream-type 'ssl)
(setq send-mail-function 'smtpmail-send-it)

;;;;;; EBDB

(unless (package-installed-p 'ebdb)
  (package-install 'ebdb))
(require 'ebdb)
(require 'ebdb-message)
(require 'ebdb-notmuch)
(setq ebdb-sources (locate-user-emacs-file "ebdb.gpg"))
(setq ebdb-permanent-ignores-file (locate-user-emacs-file "ebdb-permanent-ignores"))

(setq ebdb-mua-pop-up nil)
(setq ebdb-default-window-size 0.25)
(setq ebdb-mua-default-formatter ebdb-default-multiline-formatter)

(setq ebdb-mua-sender-update-p 'create)
(setq ebdb-message-auto-update-p 'create)

(setq ebdb-message-try-all-headers t)
(setq ebdb-message-headers
      '((sender "From" "Resent-From" "Reply-To" "Sender")
        (recipients "Resent-To" "Resent-Cc" "Resent-CC" "To" "Cc" "CC" "Bcc" "BCC")))
(setq ebdb-message-all-addresses t)

(setq ebdb-complete-mail 'capf)
(setq ebdb-mail-avoid-redundancy t)
(setq ebdb-completion-display-record nil)
(setq ebdb-complete-mail-allow-cycling nil)

(setq ebdb-record-self "2b34a2ee-7521-454a-8191-3b8ef5153dcc")
(setq ebdb-user-name-address-re 'self) ; match the above
(setq ebdb-save-on-exit t)

(setq ebdb-use-diary nil)

(let ((map ebdb-mode-map))
  (define-key map (kbd "D") #'ebdb-delete-field-or-record)
  (define-key map (kbd "M") #'ebdb-mail) ; disables `ebdb-mail-each'
  (define-key map (kbd "m") #'ebdb-toggle-record-mark)
  (define-key map (kbd "t") #'ebdb-toggle-all-record-marks)
  (define-key map (kbd "T") #'ebdb-toggle-records-format) ; disables `ebdb-toggle-all-records-format'
  (define-key map (kbd "U") #'ebdb-unmark-all-records))

;;;;; Elfeed Feed Reader

(unless (package-installed-p 'elfeed)
  (package-install 'elfeed))
(require 'elfeed)

(setq elfeed-db-directory (concat user-emacs-directory "elfeed/"))
(setq elfeed-enclosure-default-dir "~/Downloads/")
(setq elfeed-search-filter "@4-months-ago +unread")
(setq elfeed-sort-order 'ascending)
(setq elfeed-search-title-max-width 80)
(setq elfeed-search-title-min-width 30)
(setq elfeed-search-trailing-width 25)
(setq elfeed-show-truncate-long-urls t)

(add-hook 'elfeed-show-mode-hook
          (lambda ()
            (setq-local shr-width (current-fill-column))))

(define-key global-map (kbd "C-c e") 'elfeed)

(with-eval-after-load 'elfeed
  (require 'prot-elfeed)
  (setq prot-elfeed-feeds-file (concat user-emacs-directory "feeds.el"))
  (setq prot-elfeed-tag-faces t)
  (prot-elfeed-fontify-tags)
  (add-hook 'elfeed-search-mode-hook #'prot-elfeed-load-feeds)

  ;; Use alternating backgrounds, if `stripes' is available.
  (with-eval-after-load 'stripes
    (add-hook 'elfeed-search-mode-hook #'stripes-mode)
    ;; ;; To disable `hl-line-mode':
    ;; (advice-add #'elfeed-search-mode :after #'prot-common-disable-hl-line)
    )

  (let ((map elfeed-search-mode-map))
    (define-key map (kbd "s") #'prot-elfeed-search-tag-filter)
    (define-key map (kbd "o") #'prot-elfeed-search-open-other-window)
    (define-key map (kbd "q") #'prot-elfeed-kill-buffer-close-window-dwim)
    (define-key map (kbd "v") #'prot-elfeed-mpv-dwim)
    (define-key map (kbd "+") #'prot-elfeed-toggle-tag))
  (let ((map elfeed-show-mode-map))
    (define-key map (kbd "a") #'prot-elfeed-show-archive-entry)
    (define-key map (kbd "e") #'prot-elfeed-show-eww)
    (define-key map (kbd "q") #'prot-elfeed-kill-buffer-close-window-dwim)
    (define-key map (kbd "v") #'prot-elfeed-mpv-dwim)
    (define-key map (kbd "+") #'prot-elfeed-toggle-tag)))

;;;;; Proced

(setq proced-auto-update-flag t)
(with-eval-after-load 'stripes
  (add-hook 'proced-mode-hook #'stripes-mode))

(require 'prot-proced)
(prot-proced-extra-keywords 1)

;;;;; Simple HTML Renderer (shr) and EWW

(setq browse-url-browser-function 'eww-browse-url)
(setq browse-url-secondary-browser-function 'browse-url-default-browser)

(setq shr-use-colors nil)             ; t is bad for accessibility
(setq shr-use-fonts nil)              ; t is not for me
(setq shr-max-image-proportion 0.6)
(setq shr-image-animate nil)          ; No GIFs, thank you!
(setq shr-discard-aria-hidden t)
(setq shr-cookie-policy nil)

(setq url-cookie-untrusted-urls '(".*"))

(require 'eww)
(setq eww-restore-desktop t)
(setq eww-header-line-format nil)
(setq eww-search-prefix "https://duckduckgo.com/html/?q=")
(setq eww-download-directory (expand-file-name "~/Downloads"))
(setq eww-suggest-uris '(eww-links-at-point
                         thing-at-point-url-at-point))
(setq eww-bookmarks-directory (locate-user-emacs-file "eww-bookmarks/"))
(setq eww-history-limit 150)
(setq eww-browse-url-new-window-is-tab nil)

(define-key eww-link-keymap (kbd "v") nil) ; stop overriding `eww-view-source'
(define-key eww-mode-map (kbd "L") #'eww-list-bookmarks)
(define-key dired-mode-map (kbd "E") #'eww-open-file) ; to render local HTML files
(define-key eww-buffers-mode-map (kbd "d") #'eww-bookmark-kill)   ; it actually deletes
(define-key eww-bookmark-mode-map (kbd "d") #'eww-bookmark-kill) ; same

(require 'prot-eww)
(setq prot-eww-save-history-file
      (locate-user-emacs-file "prot-eww-visited-history"))
(setq prot-eww-save-visited-history t)
(setq prot-eww-bookmark-link nil)

(add-hook 'prot-eww-history-mode-hook #'hl-line-mode)

(define-prefix-command 'prot-eww-map)
(define-key global-map (kbd "C-c w") 'prot-eww-map)
(let ((map prot-eww-map))
  (define-key map (kbd "b") #'prot-eww-visit-bookmark)
  (define-key map (kbd "e") #'prot-eww-browse-dwim)
  (define-key map (kbd "s") #'prot-eww-search-engine))
(let ((map eww-mode-map))
  (define-key map (kbd "B") #'prot-eww-bookmark-page)
  (define-key map (kbd "D") #'prot-eww-download-html)
  (define-key map (kbd "F") #'prot-eww-find-feed)
  (define-key map (kbd "H") #'prot-eww-list-history)
  (define-key map (kbd "b") #'prot-eww-visit-bookmark)
  (define-key map (kbd "e") #'prot-eww-browse-dwim)
  (define-key map (kbd "o") #'prot-eww-open-in-other-window)
  (define-key map (kbd "E") #'prot-eww-visit-url-on-page)
  (define-key map (kbd "J") #'prot-eww-jump-to-url-on-page)
  (define-key map (kbd "R") #'prot-eww-readable)
  (define-key map (kbd "Q") #'prot-eww-quit))

;;;;; Viewing PDFs

(unless (package-installed-p 'pdf-tools)
  (package-install 'pdf-tools))
(require 'pdf-tools)

(setq pdf-tools-enabled-modes           ; simplified from the defaults
      '(pdf-history-minor-mode
        pdf-isearch-minor-mode
        pdf-links-minor-mode
        pdf-outline-minor-mode
        pdf-misc-size-indication-minor-mode
        pdf-occur-global-minor-mode))
(setq pdf-view-display-size 'fit-height)
(setq pdf-view-max-image-width 1080)
(setq pdf-outline-imenu-use-flat-menus t)

(pdf-loader-install)

;; The following functions and hooks are adapted from the manual modus-themes.

(defun dp-pdf-tools-backdrop ()
  "Set backdrop distinct from the background of the PDF page."
  (face-remap-add-relative
   'default
   `(:background ,(modus-themes-color 'bg-alt))))

(defun dp-pdf-tools-midnight-mode-toggle ()
  "Make pdf-tools adapt to `modus-themes-toggle'."
  (when (derived-mode-p 'pdf-view-mode)
    (if (eq (car custom-enabled-themes) 'modus-vivendi)
        (pdf-view-midnight-minor-mode 1)
      (pdf-view-midnight-minor-mode -1))
    (dp-pdf-tools-backdrop)))

(add-hook 'pdf-tools-enabled-hook #'dp-pdf-tools-midnight-mode-toggle)
(add-hook 'modus-themes-after-load-theme-hook #'dp-pdf-tools-midnight-mode-toggle)

;;;;; Viewing EPUBs

(unless (package-installed-p 'nov)
  (package-install 'nov))
(require 'nov)

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;;;; General Interface and Interactions

;;;;; Mode Line

(setq mode-line-position-column-line-format '(" %l,%c"))
(setq mode-line-defining-kbd-macro
      (propertize " Macro" 'face 'mode-line-emphasis))

(setq-default mode-line-modes
              (seq-filter (lambda (s)
                            (not (and (stringp s)
                                      (string-match-p
                                       "^\\(%\\[\\|%\\]\\)$" s))))
                          mode-line-modes))

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
                mode-line-modes
                "  "
                (vc-mode vc-mode)
                "  "
                mode-line-misc-info
                mode-line-end-spaces))

;;;;;; Moody.el

(unless (package-installed-p 'moody)
  (package-install 'moody))
(require 'moody)
(require 'prot-moody)
(setq prot-moody-font-height-multiplier 1.35)

;; Also check the Modus themes' `modus-themes-mode-line' which can set
;; the styles specifically for Moody.
(prot-moody-set-height -1)

;;;;;; Mode Line Recursion Indicators

(unless (package-installed-p 'recursion-indicator)
  (package-install 'recursion-indicator))
(require 'recursion-indicator)
(recursion-indicator-mode 1)

;;;;;; Battery Status

(setq battery-mode-line-limit 95)
(display-battery-mode 1)

;;;;;; Display Current Time (and world-clock)

(setq display-time-format "%a %e %b, %H:%M")
(setq display-time-default-load-average nil)
(setq display-time-use-mail-icon t)

(setq zoneinfo-style-world-list
      '(("America/Los_Angeles" "Los Angeles")
        ("America/New_York" "New York")
        ("Europe/London" "London")
        ("Europe/Paris" "Paris")
        ("Europe/Amsterdam" "Amsterdam")
        ("Asia/Tokyo" "Tokyo")
        ("Australia/Brisbane" "Brisbane")
        ("Australia/Canberra" "Canberra")))

(setq world-clock-time-format "%R %z  %A %d %B")
(setq world-clock-buffer-name "*world-clock*") ; Placement handled by `display-buffer-alist'

;;;;; Line Numbers and Relevant Indicators

(setq hl-line-sticky-flag nil)

(setq whitespace-style '(face
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
(setq whitespace-line-column nil)       ; if nil, uses fill-column
(setq whitespace-display-mappings
      '((tab-mark 9 [9655 9] [183 9])
        (space-mark 32 [183] [46])
        (newline-mark 10 [8617 10])
        (lines-tail 10 [8617 10])))

;; Long lines are allowed in certain modes.
(dolist (hook '(markdown-mode-hook org-mode-hook))
  (add-hook hook (lambda ()
                   (setq-local whitespace-style
                               (remove 'lines-tail whitespace-style)))))

(define-key global-map (kbd "C-c w") #'whitespace-cleanup)

;;;;; Outline Mode

(require 'outline)
(setq-default outline-minor-mode-highlight 'override)
(setq-default outline-minor-mode-cycle t)
(let ((map outline-minor-mode-map))
  (define-key map (kbd "C-<tab>") #'outline-cycle)
  (define-key map (kbd "<backtab>") #'outline-cycle-buffer) ; S-TAB
  (define-key map (kbd "C-c C-n") #'outline-next-visible-heading)
  (define-key map (kbd "C-c C-p") #'outline-previous-visible-heading)
  (define-key map (kbd "C-c C-f") #'outline-forward-same-level)
  (define-key map (kbd "C-c C-b") #'outline-backward-same-level)
  (define-key map (kbd "C-c C-a") #'outline-show-all)
  (define-key map (kbd "C-c C-o") #'outline-hide-other)
  (define-key map (kbd "C-c C-u") #'outline-up-heading))

;;;;; Cursor and Mouse Settings

;;;;;; Scrolling Behaviour

(setq-default scroll-preserve-screen-position t)
(setq-default scroll-conservatively 1)
(setq-default next-screen-context-lines 0)

;;;;;; Delete Selection

(delete-selection-mode 1)

;;;;; Alternating Background Highlights (stripes.el)

(unless (package-installed-p 'stripes)
  (package-install 'stripes))
(require 'stripes)

(setq stripes-unit 1)

;;;;; Conveniences and Minor Extras
(setq mode-require-final-newline 'visit-save)

;;;;;; Auto-Revert Mode

(global-auto-revert-mode 1)

;;;;;; Preserve Contents of System Clipboard

(setq save-interprogram-paste-before-kill t)

;;;;;; Goggles

(unless (package-installed-p 'goggles)
  (package-install 'goggles))
(require 'goggles)
(setq-default goggles-pulse t)
(dolist (mode '(prog-mode-hook text-mode-hook))
  (add-hook mode #'goggles-mode))

;;;;;; Package Lists

(add-hook 'package-menu-mode-hook #'hl-line-mode)

;;;; Language Settings for Prose and Code

;;;;; Support for Various Major Modes

;;;;;; Plain Text

(require 'titlecase)
(define-key global-map (kbd "C-c t") 'titlecase-dwim)

;;;;;; Markdown

(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode))
(require 'markdown-mode)

;;;;; Paragraphs

(setq-default fill-column 72)
(add-hook 'text-mode-hook #'turn-on-visual-line-mode)

(unless (package-installed-p 'adaptive-wrap)
  (package-install 'adaptive-wrap))
(require 'adaptive-wrap)
(add-hook 'text-mode-hook #'adaptive-wrap-prefix-mode)

(column-number-mode 1)

;;;;; Comments

;;;;; Electric Behaviour

(setq electric-quote-context-sensitive t)
(setq electric-quote-replace-double t)
(electric-quote-mode 1)

(electric-pair-mode 1)

;;;;; Parentheses

(show-paren-mode 1)

;;;;; Tabs, Indentation, and the TAB Key

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq backward-delete-char-untabify-method nil)

;; Use tab characters for indentation in certain modes.
(dolist (hook '(sh-mode-hook
                python-mode-hook
                c-mode-common-hook))
  (add-hook hook (lambda ()
                   (setq indent-tabs-mode t))))

(setq-default tab-always-indent 'complete)
(setq-default tab-first-completion 'word-or-paren-or-punct)

;;;;;; Smart Tabs for Indenting With Tabs and Aligning With Spaces

(unless (package-installed-p 'smart-tabs-mode)
  (package-install 'smart-tabs-mode))
(require 'smart-tabs-mode)

(smart-tabs-add-language-support sh sh-mode-hook
  ((smie-indent-line . sh-basic-offset)))

(smart-tabs-insinuate 'c 'c++ 'java 'javascript 'cperl 'python 'ruby 'sh 'nxml)

;; `whitespace-cleanup’ shouldn’t touch indentation in modes that use
;; Smart Tabs mode.
(dolist (hook '(sh-mode-hook
                python-mode-hook))
  (add-hook hook
            (lambda ()
              (setq-local whitespace-style
                          (remove 'indentation whitespace-style)))))

;;;;; Spell Checking

(require 'flyspell)
(setq flyspell-issue-message-flag nil)
(setq flyspell-issue-welcome-flag nil)
(setq ispell-dictionary "australian-w_accents")

(require 'prot-spell)
(setq prot-spell-dictionaries
      '(("EN English" . "australian-w_accents")
        ("FR Français" . "francais-lrg")
        ("NL Nederlands" . "dutch")
        ("ES Espanõl" . "español")))
(let ((map global-map))
  (define-key map (kbd "M-$") #'prot-spell-spell-dwim)
  (define-key map (kbd "C-M-$") #'prot-spell-change-dictionary)
  (define-key map (kbd "C-M-;") #'flyspell-goto-next-error)
  (define-key map (kbd "C-;") #'flyspell-auto-correct-word))

;;;;;; Code and Text Linters

;;;;;;; Flymake

(require 'flymake)
(setq flymake-suppress-zero-counters t)
(setq flymake-no-changes-timeout nil)
(setq flymake-mode-line-format
      '(""
        flymake-mode-line-exception
        flymake-mode-line-counters))
(setq flymake-mode-line-counter-format
      '(" "
        flymake-mode-line-error-counter
        flymake-mode-line-warning-counter
        flymake-mode-line-note-counter ""))

(let ((map flymake-mode-map))
  (define-key map (kbd "C-c ! s") #'flymake-start)
  (define-key map (kbd "C-c ! d") #'flymake-show-buffer-diagnostics)
  (define-key map (kbd "C-c ! n") #'flymake-goto-next-error)
  (define-key map (kbd "C-c ! p") #'flymake-goto-prev-error))

(unless (package-installed-p 'flymake-diagnostic-at-point)
  (package-install 'flymake-diagnostic-at-point))
(require 'flymake-diagnostic-at-point)
(setq flymake-diagnostic-at-point-display-diagnostic-function
      'flymake-diagnostic-at-point-display-minibuffer)

;;;;;;; Flymake + Shellcheck

(unless (package-installed-p 'flymake-shellcheck)
  (package-install 'flymake-shellcheck))
(require 'flymake-shellcheck)
(add-hook 'sh-mode-hook 'flymake-shellcheck-load)

;;;;;;; Flymake + Proselint

(unless (package-installed-p 'flymake-proselint)
  (package-install 'flymake-proselint))
(require 'flymake-proselint)
(dolist (hook '(markdown-mode-hook
                org-mode-hook
                text-mode-hook))
  (add-hook hook #'flymake-proselint-setup))

;;;;;;; Flymake + Markdown

(unless (package-installed-p 'flymake-quickdef)
  (package-install 'flymake-quickdef))
(require 'flymake-quickdef)

(require 'flymake-markdownlint)
(add-hook 'markdown-mode-hook #'flymake-markdownlint-setup)

;;;; History and State

(server-start)

(require 'recentf)
(setq recentf-max-saved-items 200)
(setq recentf-exclude '(".gz" ".xz" ".zip" "/elpa/" "/ssh:" "/sudo:"))
(recentf-mode 1)

(require 'desktop)
(setq desktop-dirname user-emacs-directory)
(setq desktop-base-file-name "desktop")
(setq desktop-globals-to-clear nil)
(setq desktop-restore-eager 0)
(setq desktop-restore-frames nil)
(dolist (symbol '(kill-ring log-edit-comment-ring))
  (add-to-list 'desktop-globals-to-save symbol))
(desktop-save-mode 1)

(require 'savehist)
(setq savehist-file (locate-user-emacs-file "savehist"))
(setq history-length 10000)
(setq history-delete-duplicates t)
(savehist-mode 1)

(require 'saveplace)
(setq save-place-file (locate-user-emacs-file "saveplace"))
(save-place-mode 1)

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backup/"))))
(setq backup-by-copying t)
(setq version-control t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq create-lockfiles nil)

;;; init.el ends here

;; Local Variables:
;; outline-regexp: ";;;;* [^ \t\n]"
;; eval: (progn (outline-minor-mode 1) (outline-hide-body))
;; End:

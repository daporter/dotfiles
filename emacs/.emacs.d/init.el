;;; init.el --- Personal configuration file

;;; Commentary:

;;; Code:

;;;; Initial Settings

(require 'package)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(setq vc-follow-symlinks t) ; because my dotfiles are managed this way

(add-to-list 'load-path (locate-user-emacs-file "lisp"))
(add-to-list 'load-path (locate-user-emacs-file "prot-lisp"))

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

(defun my/quit-or-restart (&optional restart)
  "Quit Emacs or restart it with RESTART."
  (interactive "P")
  (if restart
      (restart-emacs)
    (save-buffers-kill-terminal t)))

(keymap-set global-map "C-x C-c" #'my/quit-or-restart)
(keymap-set global-map "C-c p" #'package-list-packages)

;;;; Base Settings

;;;;; Highlight Cursor Position (pulsar.el)

(unless (package-installed-p 'pulsar)
  (package-install 'pulsar))

(pulsar-global-mode 1)
(keymap-set global-map "C-x l" #'pulsar-pulse-line) ; override `count-lines-page'

;;;;; Make Custom UI code disposable

(setq custom-file (make-temp-file "emacs-custom-"))

(setq help-window-select t)

;;;;; Propagation of Shell Environment Variables

(unless (package-installed-p 'exec-path-from-shell)
  (package-install 'exec-path-from-shell))

(setq exec-path-from-shell-variables
      '("PATH" "MANPATH" "SSH_AUTH_SOCK"))
(exec-path-from-shell-initialize)

;;;;; Modus Themes

;; Note: All customisation options must be evaluated before loading a
;; theme.
(setq modus-themes-bold-constructs t)
(setq modus-themes-slanted-constructs t)
(setq modus-themes-syntax '(green-strings))
(setq modus-themes-mixed-fonts t)
(setq modus-themes-links '(no-underline faint))
(setq modus-themes-mail-citations 'faint)
(setq modus-themes-lang-checkers '(background))
(setq modus-themes-region '(bg-only))
(setq modus-themes-diffs 'desaturated)
(setq modus-themes-org-blocks 'gray-background)
(setq modus-themes-variable-pitch-headings t)

(setq modus-themes-headings
      '((1 . (overline monochrome 1.25))
        (2 . (monochrome 1.15))
        (t . (monochrome))))

(load-theme 'modus-operandi)

;;;;; Enhancements to hl-line-mode (lin.el)

(unless (package-installed-p 'lin)
  (package-install 'lin))

(lin-global-mode 1)

;;;;; Font Configurations (fontaine.el)

(unless (package-installed-p 'fontaine)
  (package-install 'fontaine))

(setq-default text-scale-remap-header-line t)

(setq fontaine-latest-state-file
      (locate-user-emacs-file "fontaine-latest-state.eld"))

(setq fontaine-presets
      '((regular
         :default-height 100
         :variable-pitch-family "Sans Serif")))

;; Set last preset or fall back to desired style from `fontaine-presets'.
(fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))

;; The other side of `fontaine-restore-latest-preset'.
(add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)

(keymap-set global-map "C-c f" #'fontaine-set-preset)
(keymap-set global-map "C-c F" #'fontaine-set-face-font)

;;;;; Repeatable Keychords

(repeat-mode 1)

;;;;; Handle performance for very long lines (so-long.el)

(global-so-long-mode 1)

;;;;; Keychord Hints

(unless (package-installed-p 'which-key)
  (package-install 'which-key))

(setq which-key-idle-delay 0.8)
(setq which-key-idle-secondary-delay 0.05)
(setq which-key-add-column-padding 2)

(which-key-mode 1)

;;;; Selection Candidates and Search Methods

;;;;; Completion Frameword and Extras

;;;;;; Orderless Completion Style

(unless (package-installed-p 'orderless)
  (package-install 'orderless))

(setq orderless-matching-styles
      '(orderless-prefixes
        orderless-strict-leading-initialism
        orderless-flex
        orderless-regexp))
(setq orderless-style-dispatchers
      '(prot-orderless-literal-dispatcher
        prot-orderless-initialism-dispatcher
        prot-orderless-flex-dispatcher))

;; SPC should never complete: use it for `orderless' groups.
(let ((map minibuffer-local-completion-map))
  (keymap-set map "SPC" nil)
  (keymap-set map "?" nil))

(with-eval-after-load "orderless"
  (require 'prot-orderless))

;;;;;; Completion Annotations (Marginalia)

(unless (package-installed-p 'marginalia)
  (package-install 'marginalia))

(marginalia-mode 1)

;;;;;; Minibuffer and Vertico Configurations

(setq completion-styles '(basic orderless)) ; also see `completion-category-overrides'
(setq completion-category-defaults nil)
(setq completion-category-overrides
      '((file (styles . (basic partial-completion orderless)))
        (project-file (styles . (basic substring partial-completion orderless)))
        (imenu (styles . (basic substring orderless)))
        (kill-ring (styles . (basic substring orderless)))
        (consult-location (styles . (basic substring orderless)))))

(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq-default case-fold-search t)   ; For general regexp

(setq enable-recursive-minibuffers t)
(setq resize-mini-windows t)
(setq minibuffer-eldef-shorten-default t)

(setq read-answer-short t) ; also check `use-short-answers' for Emacs28
(setq echo-keystrokes 0.25)           ; from the C source code

;; Do not allow the cursor to move inside the minibuffer prompt.  I
;; got this from the documentation of Daniel Mendler's Vertico
;; package: <https://github.com/minad/vertico>.
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Also adapted from Vertico.
(defun crm-indicator (args)
  "Add prompt indicator to `completing-read-multiple' filter ARGS."
  ;; The `error' face just makes the text red.
  (cons (concat (propertize "[CRM] " 'face 'error) (car args)) (cdr args)))

(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

(file-name-shadow-mode 1)
(minibuffer-depth-indicate-mode 1)
(minibuffer-electric-default-mode 1)

;;;;;; Minibuffer History (savehist-mode)

(setq savehist-file (locate-user-emacs-file "savehist"))
(setq history-length 10000)
(setq history-delete-duplicates t)
(savehist-mode 1)

;;;;;; Vertico

(unless (package-installed-p 'vertico)
  (package-install 'vertico))

(setq vertico-resize nil)
(setq vertico-cycle t)

(vertico-mode 1)

;; This works with `file-name-shadow-mode'.  When you are in a
;; sub-directory and use, say, `find-file' to go to your home '~/' or
;; root '/' directory, Vertico will clear the old path to keep only
;; your current input.
(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

;;;;;; Enhanced Minibuffer Commands (consult.el)

(unless (package-installed-p 'consult)
  (package-install 'consult))

(setq consult-line-numbers-widen t)
;; (setq completion-in-region-function #'consult-completion-in-region)
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
        ;; (?p "PDF" ,#'pdf-view-bookmark-jump)
        (?v "VC Dir" ,#'vc-dir-bookmark-jump)
        (?w "EWW" ,#'prot-eww-bookmark-jump)))
(setq register-preview-delay 0.8
      register-preview-function #'consult-register-format)
(setq consult-find-args "find . -not ( -wholename */.* -prune )")
(setq consult-preview-key 'any)

(add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)

(with-eval-after-load "consult"
  (require 'consult-imenu))

(let ((map global-map))
  (keymap-set map "<remap> <goto-line>" #'consult-goto-line)
  (keymap-set map "C-x M-:" #'consult-complex-command)
  (keymap-set map "C-x M-k" #'consult-kmacro)
  (keymap-set map "C-x M-m" #'consult-minor-mode-menu)
  (keymap-set map "C-x r b" #'consult-bookmark) ; override `bookmark-jump'
  (keymap-set map "C-x r r" #'consult-register) ; Use the register's prefix
  (keymap-set map "M-F" #'consult-focus-lines) ; same principle
  (keymap-set map "M-K" #'consult-keep-lines) ; M-S-k is similar to M-S-5 (M-%)
  (keymap-set map "M-s M-!" #'consult-flymake)
  (keymap-set map "M-s M-b" #'consult-buffer)
  (keymap-set map "M-s M-c" #'consult-locate)
  (keymap-set map "M-s M-f" #'consult-find)
  (keymap-set map "M-s M-g" #'consult-grep)
  (keymap-set map "M-s M-h" #'consult-history)
  (keymap-set map "M-s M-i" #'consult-imenu)
  (keymap-set map "M-s M-l" #'consult-line)
  (keymap-set map "M-s M-m" #'consult-mark)
  (keymap-set map "M-s M-o" #'consult-outline)
  (keymap-set map "M-s M-y" #'consult-yank-pop))

(with-eval-after-load "consult"
  (keymap-set consult-narrow-map "?" #'consult-narrow-help))

(setq consult-after-jump-hook nil) ; reset it to avoid conflicts with my function
(dolist (fn '(pulsar-recenter-top
              pulsar-reveal-entry))
  (add-hook 'consult-after-jump-hook fn))

;;;;;;; Switch to Directories (consult-dir.el)

(unless (package-installed-p 'consult-dir)
  (package-install 'consult-dir))

(setq consult-dir-sources '(consult-dir--source-bookmark
                            consult-dir--source-default
                            consult-dir--source-project
                            consult-dir--source-recentf))

(keymap-set global-map "C-x C-d" #'consult-dir)
(keymap-set minibuffer-local-filename-completion-map "C-x C-d" #'consult-dir)

;;;;;; Extended Minibuffer Actions (embark.el)

(unless (package-installed-p 'embark)
  (package-install 'embark))

(setq prefix-help-command #'embark-prefix-help-command)
(setq embark-cycle-key (kbd "M-."))     ; see the `embark-act' key
(setq embark-confirm-act-all nil)
(setq embark-indicators '(embark-mixed-indicator
                          embark-highlight-indicator))
(setq embark-verbose-indicator-excluded-actions
      '("\\`customize-" "\\(local\\|global\\)-set-key"
        set-variable embark-cycle embark-keymap-help embark-isearch))
(setq embark-verbose-indicator-display-action nil)

;; The command ‘embark-dwim’ executes the default action at point, while
;; `embark-act’ displays a menu of actions.  With my home-row modifiers,
;; ‘C-.’ can be seen as a left-click at point and and ‘M-.’ as a
;; right-click context menu.  The keybindings are mnemonic, since both
;; act at the point (‘.’).
(keymap-set global-map "C-." #'embark-dwim) ; like left-click
(keymap-set global-map "M-." #'embark-act)  ; like right-click
(keymap-set global-map "C-h B" #'embark-bindings)

(with-eval-after-load "embark"
  (progn
    (keymap-set embark-collect-mode-map "M-." #'embark-act)
    (keymap-set embark-symbol-map "k" #'describe-keymap)))

(unless (package-installed-p 'embark-consult)
  (package-install 'embark-consult))

(with-eval-after-load "embark"
  (progn
    (require 'prot-embark)
    (prot-embark-keymaps 1)
    (prot-embark-setup-packages 1)))

;;;;;; Projects

(setq project-list-file "~/Sync/emacs/projects")
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

(keymap-set global-map "C-x p q" #'project-query-replace-regexp)

;;;;;; Completion for Recent Files and Directories

(setq recentf-max-saved-items 200)
(setq recentf-exclude '(".gz" ".xz" ".zip" "/elpa/" "/ssh:" "/sudo:"))
(recentf-mode 1)

;;;;;; In-Buffer Completions

;;;;;;; Corfu (In-Buffer Completion Popup)

(unless (package-installed-p 'corfu)
  (package-install 'corfu))

(global-corfu-mode 1)

;; Adapted from Corfu's manual.
(defun contrib/corfu-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico is not active.
Useful for prompts such as `eval-expression' and `shell-command'."
  (unless (bound-and-true-p vertico--input)
    (corfu-mode 1)))

(add-hook 'minibuffer-setup-hook #'contrib/corfu-enable-always-in-minibuffer 1)

;;;;;;; CAPE (Extra completion-at-point Backends)

(unless (package-installed-p 'cape)
  (package-install 'cape))

(setq cape-dabbrev-min-length 3)
(dolist (backend '(cape-symbol
                   cape-keyword
                   cape-file
                   cape-dabbrev))
  (add-to-list 'completion-at-point-functions backend))

;;;;;;; Enhance Command-Line Completion (pcmpl-args)

(unless (package-installed-p 'pcmpl-args)
  (package-install 'pcmpl-args))

;;;; Directory, Buffer, Window Management

;;;;; Dired

(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq delete-by-moving-to-trash t)
(setq dired-listing-switches "-AFGhlv")
(setq dired-dwim-target t)
(setq dired-auto-revert-buffer #'dired-directory-changed-p) ; also see `dired-do-revert-buffer'
(setq dired-make-directory-clickable t)

(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook #'hl-line-mode)

;;;;; Working With Buffers

;;;;;; Keymap for Buffers

(let ((map ctl-x-x-map))
  (keymap-set map "e" #'eval-buffer)
  (keymap-set map "f" #'follow-mode)  ; override `font-lock-update'
  (keymap-set map "r" #'rename-uniquely))

;;;;;; Unique Names for Buffers

(setq uniquify-buffer-name-style 'forward)

;;;;;; Ibuffer and Extras

(with-eval-after-load "ibuffer"
  (progn
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
    (let ((map ibuffer-mode-map))
      (keymap-set map "* f" #'ibuffer-mark-by-file-name-regexp)
      (keymap-set map "* g" #'ibuffer-mark-by-content-regexp) ; "g" is for "grep"
      (keymap-set map "* n" #'ibuffer-mark-by-name-regexp)
      (keymap-set map "s n" #'ibuffer-do-sort-by-alphabetic)  ; "sort name" mnemonic
      (keymap-set map "/ g" #'ibuffer-filter-by-content))))

(keymap-set global-map "C-x C-b" #'ibuffer)

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
         (window-height . fit-window-to-buffer)
         (window-parameters . ((no-other-window . t)
                               (mode-line-format . none))))
        ("\\*\\(Embark\\)?.*Completions.*"
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
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
  (keymap-set map "C-x <down>" #'next-buffer)
  (keymap-set map "C-x <up>" #'previous-buffer)
  (keymap-set map "C-x C-n" #'next-buffer)     ; override `set-goal-column'
  (keymap-set map "C-x C-p" #'previous-buffer) ; override `mark-page'
  (keymap-set map "C-x !" #'delete-other-windows-vertically)
  (keymap-set map "C-x _" #'balance-windows)      ; underscore
  (keymap-set map "C-x -" #'fit-window-to-buffer) ; hyphen
  (keymap-set map "C-x +" #'balance-windows-area)
  (keymap-set map "C-x }" #'enlarge-window)
  (keymap-set map "C-x {" #'shrink-window)
  (keymap-set map "C-x >" #'enlarge-window-horizontally) ; override `scroll-right'
  (keymap-set map "C-x <" #'shrink-window-horizontally)) ; override `scroll-left'
(let ((map resize-window-repeat-map))
  (keymap-set map ">" #'enlarge-window-horizontally))

;;;;;; Window History (winner-mode)

(add-hook 'after-init-hook #'winner-mode)

;;;;;; Tabs for Window Layouts

(setq tab-bar-close-button-show nil)
(setq tab-bar-show nil)

(tab-bar-mode -1)                     ; see `prot-tab-status-line'
(tab-bar-history-mode 1)

(with-eval-after-load "tab-bar"
  (require 'prot-tab))

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
  (keymap-set map "C-x <right>" #'prot-tab-winner-redo)
  (keymap-set map "C-x <left>" #'prot-tab-winner-undo)
  (keymap-set map "C-<f8>" #'prot-tab-status-line) ; unopinionated alternative: `prot-tab-bar-toggle'
  (keymap-set map "C-x t t" #'prot-tab-select-tab-dwim))

;;;;;; Transposition and Rotation of Windows

(unless (package-installed-p 'transpose-frame)
  (package-install 'transpose-frame))

(keymap-set global-map "C-x M-r" #'rotate-frame-clockwise)

;;;;;; Quickly Switch Windows (ace-window)

(unless (package-installed-p 'ace-window)
  (package-install 'ace-window))

(setq aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n))
(setq aw-dispatch-alist
      '((?b aw-switch-buffer-in-window
            "Select buffer in window and switch")
        (?B aw-switch-buffer-other-window
            "Select buffer in window without switching")
        (?s aw-swap-window "Swap windows")
        (?c aw-copy-window "Copy window")
        (?m aw-move-window "Move window")
        (?x aw-delete-window "Delete window")
        (?O delete-other-windows "Delete other windows")
        (?+ aw-split-window-fair "Split window fairly")
        (?- aw-split-window-vert "Split window vertically")
        (?| aw-split-window-horz "Split window horizontally")
        (?? aw-show-dispatch-help)))
(setq aw-dispatch-always t)

(keymap-set global-map "M-o" #'ace-window)

(eval-when-compile
  (defmacro my/embark-ace-action (fn)
    `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
       (interactive)
       (with-demoted-errors "%s"
         (require 'ace-window)
         (let ((aw-dispatch-always t))
           (aw-switch-to-window (aw-select nil))
           (call-interactively (symbol-function ',fn)))))))

(with-eval-after-load "embark"
  (progn
    (keymap-set embark-file-map     "o" (my/embark-ace-action find-file))
    (keymap-set embark-buffer-map   "o" (my/embark-ace-action switch-to-buffer))
    (keymap-set embark-bookmark-map "o" (my/embark-ace-action bookmark-jump))))

;;;; Applications and Utilities

;;;;; Bookmarking

(setq bookmark-default-file "~/Sync/emacs/bookmarks")
(setq bookmark-use-annotations nil)
(setq bookmark-automatically-show-annotations t)
(setq bookmark-set-fringe-mark t)

(add-hook 'bookmark-bmenu-mode-hook #'hl-line-mode)

(with-eval-after-load "bookmark"
  (progn
    (require 'prot-bookmark)
    (prot-bookmark-extra-keywords 1)))

;;;;; Focus Mode (logos.el)

(unless (package-installed-p 'olivetti)
  (package-install 'olivetti))

(setq olivetti-body-width 80)

(unless (package-installed-p 'logos)
  (package-install 'logos))

(setq logos-outlines-are-pages t)

;; These apply when `logos-focus-mode' is enabled.  Their value is
;; buffer-local.
(setq-default logos-hide-mode-line t)
(setq-default logos-variable-pitch t)
(setq-default logos-buffer-read-only t)
(setq-default logos-olivetti t)

(let ((map global-map))
  (keymap-set map "<remap> <narrow-to-region>" #'logos-narrow-dwim)
  (keymap-set map "<remap> <forward-page>" #'logos-forward-page-dwim)
  (keymap-set map "<remap> <backward-page>" #'logos-backward-page-dwim)
  ;; I don't think I ever saw a package bind M-] or M-[...
  (keymap-set map "M-]" #'logos-forward-page-dwim)
  (keymap-set map "M-[" #'logos-backward-page-dwim)
  (keymap-set map "<f9>" #'logos-focus-mode))

;;;;;; Extra Tweaks

;; Read the logos manual: <https://protesilaos.com/emacs/logos>.

;; place point at the top when changing pages, but not in `prog-mode'
(defun prot/logos--recenter-top ()
  "Use `recenter' to reposition the view at the top."
  (unless (derived-mode-p 'prog-mode)
    (recenter 1))) ; Use 0 for the absolute top

(add-hook 'logos-page-motion-hook #'prot/logos--recenter-top)

;;;;; Version Control Tools

;;;;;; Diff Mode

(setq diff-default-read-only t)
(setq diff-refine nil)         ; I do it on demand
;; The following is further controlled by
;; `prot-diff-modus-themes-diffs'
(setq diff-font-lock-syntax 'hunk-also)

(with-eval-after-load "diff-mode"
  (progn
    (require 'prot-diff)
    (prot-diff-extra-keywords 1)
    (prot-diff-modus-themes-diffs)
    (add-hook 'modus-themes-after-load-theme-hook #'prot-diff-modus-themes-diffs)))

;; `prot-diff-buffer-dwim' replaces the default for `vc-diff' (which I
;; bind to another key---see VC section).
(keymap-set global-map "C-x v =" #'prot-diff-buffer-dwim)
(with-eval-after-load "diff-mode"
  (let ((map diff-mode-map))
    (keymap-set map "C-c C-b" #'prot-diff-refine-cycle) ; replace `diff-refine-hunk'
    (keymap-set map "C-c C-n" #'prot-diff-narrow-dwim)))

;;;;;; Version Control Framework (vc.el and prot-vc.el)

;; This one is for editing commit messages.
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
  (keymap-set map "C-x v b" #'vc-retrieve-tag)  ; "branch" switch
  (keymap-set map "C-x v t" #'vc-create-tag)
  (keymap-set map "C-x v f" #'vc-log-incoming)  ; the actual git fetch
  (keymap-set map "C-x v o" #'vc-log-outgoing)
  (keymap-set map "C-x v F" #'vc-update)        ; "F" because "P" is push
  (keymap-set map "C-x v d" #'vc-diff))
(with-eval-after-load "vc-dir"
  (let ((map vc-dir-mode-map))
    (keymap-set map "b" #'vc-retrieve-tag)
    (keymap-set map "t" #'vc-create-tag)
    (keymap-set map "O" #'vc-log-outgoing)
    (keymap-set map "o" #'vc-dir-find-file-other-window)
    (keymap-set map "f" #'vc-log-incoming) ; replaces `vc-dir-find-file' (use RET)
    (keymap-set map "F" #'vc-update)       ; symmetric with P: `vc-push'
    (keymap-set map "d" #'vc-diff)         ; parallel to D: `vc-root-diff'
    (keymap-set map "k" #'vc-dir-clean-files)
    (keymap-set map "G" #'vc-revert)
    (let ((prot-vc-git-branch-map (make-sparse-keymap)))
      (keymap-set map "B" prot-vc-git-branch-map)
      (keymap-set prot-vc-git-branch-map "n" #'vc-create-tag) ; new branch/tag
      (keymap-set prot-vc-git-branch-map "s" #'vc-retrieve-tag) ; switch branch/tag
      (keymap-set prot-vc-git-branch-map "c" #'prot-vc-git-checkout-remote) ; "checkout" remote
      (keymap-set prot-vc-git-branch-map "l" #'vc-print-branch-log))
    (let ((prot-vc-git-stash-map (make-sparse-keymap)))
      (keymap-set map "S" prot-vc-git-stash-map)
      (keymap-set prot-vc-git-stash-map "c" #'vc-git-stash) ; "create" named stash
      (keymap-set prot-vc-git-stash-map "s" #'vc-git-stash-snapshot))))
(with-eval-after-load "vc-git"
  (let ((map vc-git-stash-shared-map))
    (keymap-set map "a" #'vc-git-stash-apply-at-point)
    (keymap-set map "c" #'vc-git-stash) ; "create" named stash
    (keymap-set map "D" #'vc-git-stash-delete-at-point)
    (keymap-set map "p" #'vc-git-stash-pop-at-point)
    (keymap-set map "s" #'vc-git-stash-snapshot)))
(with-eval-after-load "vc-annotate"
  (let ((map vc-annotate-mode-map))
    (keymap-set map "M-q" #'vc-annotate-toggle-annotation-visibility)
    (keymap-set map "C-c C-c" #'vc-annotate-goto-line)
    (keymap-set map "<return>" #'vc-annotate-find-revision-at-line)))
(with-eval-after-load "log-view"
  (let ((map log-view-mode-map))
    (keymap-set map "<tab>" #'log-view-toggle-entry-display)
    (keymap-set map "<return>" #'log-view-find-revision)
    (keymap-set map "s" #'vc-log-search)
    (keymap-set map "o" #'vc-log-outgoing)
    (keymap-set map "f" #'vc-log-incoming)
    (keymap-set map "F" #'vc-update)
    (keymap-set map "P" #'vc-push)))

(with-eval-after-load "vc"
  (progn
    (require 'prot-vc)
    ;; This refashions log view and log edit buffers
    (prot-vc-git-setup-mode 1)))

(setq prot-vc-log-limit 100)
(setq prot-vc-log-bulk-action-limit 50)
(setq prot-vc-git-log-edit-show-commits t)
(setq prot-vc-git-log-edit-show-commit-count 10)
(setq prot-vc-shell-output "*prot-vc-output*")
(setq prot-vc-patch-output-dirs (list "~/" "~/Desktop/"))

(with-eval-after-load "log-edit"
  (add-to-list' log-edit-headers-alist '("Amend")))

;; NOTE: I override lots of the defaults
(let ((map global-map))
  (keymap-set map "C-x v i" #'prot-vc-git-log-insert-commits)
  (keymap-set map "C-x v p" #'prot-vc-project-or-dir)
  (keymap-set map "C-x v SPC" #'prot-vc-custom-log)
  (keymap-set map "C-x v g" #'prot-vc-git-grep)
  (keymap-set map "C-x v G" #'prot-vc-git-log-grep)
  (keymap-set map "C-x v a" #'prot-vc-git-patch-apply)
  (keymap-set map "C-x v c" #'prot-vc-git-patch-create-dwim)
  (keymap-set map "C-x v s" #'prot-vc-git-show)
  (keymap-set map "C-x v r" #'prot-vc-git-find-revision)
  (keymap-set map "C-x v B" #'prot-vc-git-blame-region-or-file)
  (keymap-set map "C-x v R" #'prot-vc-git-reset))
(with-eval-after-load "vc-git"
  (let ((map vc-git-log-edit-mode-map))
    (keymap-set map "C-C C-n" #'prot-vc-git-log-edit-extract-file-name)
    (keymap-set map "C-C C-i" #'prot-vc-git-log-insert-commits)
    ;; Also done by `prot-vc-git-setup-mode', but I am putting it here
    ;; as well for visibility.
    (keymap-set map "C-c C-c" #'prot-vc-git-log-edit-done)
    (keymap-set map "C-c C-a" #'prot-vc-git-log-edit-toggle-amend)
    (keymap-set map "M-p" #'prot-vc-git-log-edit-previous-comment)
    (keymap-set map "M-n" #'prot-vc-git-log-edit-next-comment)
    (keymap-set map "M-s" #'prot-vc-git-log-edit-complete-comment)
    (keymap-set map "M-r" #'prot-vc-git-log-edit-complete-comment)))
(with-eval-after-load "log-view"
  (let ((map log-view-mode-map))
    (keymap-set map "C-TAB" #'prot-vc-log-view-toggle-entry-all)
    (keymap-set map "a" #'prot-vc-git-patch-apply)
    (keymap-set map "c" #'prot-vc-git-patch-create-dwim)
    (keymap-set map "R" #'prot-vc-git-log-reset)
    (keymap-set map "w" #'prot-vc-log-kill-hash)))

;;;;;; Magit

(unless (package-installed-p 'magit)
  (package-install 'magit))

(setq magit-define-global-key-bindings nil)
(keymap-set global-map "C-c g" #'magit-status)

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

(setq magit-diff-refine-hunk t)

(setq magit-repository-directories
      '(("~/src" . 1)
        ("~/dotfiles" . 0)))

;;;;;; Smerge and Ediff

(require 'smerge-mode)

(setq ediff-keep-variants nil)
(setq ediff-make-buffers-readonly-at-startup nil)
(setq ediff-merge-revisions-with-ancestor t)
(setq ediff-show-clashes-only t)
(setq ediff-split-window-function #'split-window-horizontally)
(setq ediff-window-setup-function #'ediff-setup-windows-plain)

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

;;;;; Command-Line Shells

;;;;;; Eshell

(setenv "PAGER" "cat") ; solves issues, such as with 'git log' and the default 'less'

(setq password-cache-expiry 600)
(setq eshell-hist-ignoredups t)

(with-eval-after-load "esh-mode"
  (keymap-set eshell-mode-map "C-x DEL" #'eshell-kill-input))

(with-eval-after-load "esh-module"
  (dolist (module '(eshell-tramp
                    eshell-elecslash))
    (add-to-list 'eshell-modules-list module)))

;;;;; Sudo

(unless (package-installed-p 'sudo-edit)
  (package-install 'sudo-edit))

(with-eval-after-load "embark"
  (keymap-set embark-file-map "S" #'sudo-edit-find-file))

;;;;; Org Mode

(setq org-directory "~/Sync/org")

(setq org-M-RET-may-split-line '((default . nil)))
(setq org-list-allow-alphabetical t)
(setq org-catch-invisible-edits 'show)
(setq org-hide-emphasis-markers t)
(setq org-modules '(ol-info ol-eww))
(setq org-structure-template-alist
      '(("s" . "src")
        ("S" . "src sh")
        ("E" . "src emacs-lisp")
        ("e" . "example")
        ("q" . "quote")
        ("v" . "verse")
        ("V" . "verbatim")
        ("c" . "center")
        ("C" . "comment")))

;;;;;; TODO

(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-track-ordered-property-with-tag t)

;;;;;; Capture

(setq org-capture-templates
      `(("n" "Note" entry
         (file "")
         ,(concat "* %U\n"
                  "\n"
                  "%i\n"
                  "%?\n"
                  "%l")
         :empty-lines-after 1)))

;;;;;; Agenda

;;;;;;; Basic Agenda Setup

(setq org-default-notes-file "~/Sync/inbox/notes.org")
(setq org-agenda-window-setup 'current-window)

;;;;;;; Agenda View

(setq org-agenda-sorting-strategy
      '((agenda habit-down time-up deadline-up priority-down)
        (todo priority-down timestamp-up)
        (tags priority-down category-keep)
        (search category-keep)))

(defun my/org-inherited-priority (s)
  "Make subtask S inherit the priority of its parent."
  (cond
   ;; Priority cookie in this heading.
   ((string-match org-priority-regexp s)
    (* 1000 (- org-priority-lowest
               (org-priority-to-value (match-string 2 s)))))
   ;; No priority cookie, but already at highest level.
   ((not (org-up-heading-safe))
    (* 1000 (- org-priority-lowest org-priority-default)))
   ;; Look for the parent's priority.
   (t
    (my/org-inherited-priority (org-get-heading)))))

(setq org-priority-get-priority-function
      #'my/org-inherited-priority)

;;;;;;; Agenda Marks

(setq org-agenda-bulk-mark-char "#")

;;;;;;; Agenda Diary Entries

(setq org-agenda-include-diary t)

;;;;;;; Agenda Items With Scheduled and Deadline Timestamps

(setq org-deadline-warning-days 5)
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
(setq org-agenda-skip-timestamp-if-deadline-is-shown t)
(setq org-agenda-skip-deadline-prewarning-if-scheduled 1)
(setq org-agenda-search-headline-for-time nil)
(setq org-scheduled-past-days 365)
(setq org-deadline-past-days 365)
(setq org-agenda-time-leading-zero t)
(setq org-agenda-current-time-string
      (concat "Now " (make-string 70 ?-)))
(setq org-agenda-time-grid
      '((daily today require-timed)
        (0600 0700 0800 0900 1000 1100
              1200 1300 1400 1500 1600
              1700 1800 1900 2000 2100)
        " ....." "-----------------"))

;;;;;;; Agenda Global To-Do List

(setq org-agenda-todo-ignore-scheduled t)

;;;;;; Hooks and Key Bindings

(dolist (hook '(org-agenda-after-show-hook
                org-follow-link-hook))
  (add-hook hook #'pulsar-recenter-top)
  (add-hook hook #'pulsar-reveal-entry))

(defun my/org-enable-avy-binding ()
  "Remove local binding \"C-,\" from Org Mode.
\"C-,\" is my binding for activating Avy."
  (keymap-unset org-mode-map "C-,"))

(add-hook 'org-mode-hook #'my/org-enable-avy-binding)

(keymap-set global-map "C-c l" #'org-store-link)

(with-eval-after-load "org"
  (keymap-set org-mode-map "C-c L" #'org-toggle-link-display))

;;;;;; Prettier Org Constructs (org-modern.el)

(unless (package-installed-p 'org-modern)
  (package-install 'org-modern))

(setq org-modern-hide-stars t)

(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

;;;;;; Org-GTD

(unless (package-installed-p 'org-gtd)
  (package-install 'org-gtd))

(setq org-gtd-directory "~/Sync/gtd/")

(setq org-edna-use-inheritance t)
(org-edna-mode 1)

(defun my/goto-first-agenda-item ()
  "Move point to the beginning of the first agenda item."
  (interactive)
  (goto-char (point-min))
  (org-agenda-next-item 1))

;; Go to first agenda item after `org-gtd-engage’ is run.
(advice-add 'org-gtd-engage :after #'my/goto-first-agenda-item)

(let ((map global-map))
  (keymap-set map "C-c c" #'org-gtd-capture)
  (keymap-set map "C-c d e" #'org-gtd-engage)
  (keymap-set map "C-c d n" #'org-gtd-show-all-next)
  (keymap-set map "C-c d p" #'org-gtd-process-inbox)
  (keymap-set map "C-c d s" #'org-gtd-show-stuck-projects))

(with-eval-after-load "org-agenda"
  (keymap-set org-agenda-mode-map "g" #'org-gtd-engage))

(keymap-set org-gtd-process-map "C-c c" #'org-gtd-choose)

;;;;;; Org Journal

(unless (package-installed-p 'org-journal)
  (package-install 'org-journal))

(setq org-journal-dir "~/Sync/journal")
(setq org-journal-file-format "%Y-%m-%d.org")
(setq org-journal-date-format "%A, %d %B %Y")

(keymap-set global-map "C-c j" #'org-journal-new-entry)

;;;;;; Org Roam

(setq org-roam-v2-ack t)

(unless (package-installed-p 'org-roam)
  (package-install 'org-roam))

(setq org-roam-directory "~/Sync/zettelkasten")

(org-roam-db-autosync-mode)

(let ((map global-map))
  (keymap-set map "C-c z f" #'org-roam-node-find)
  (keymap-set map "C-c z i" #'org-roam-node-insert))
(keymap-set org-mode-map "C-c z t" #'org-roam-buffer-toggle)

(setq org-roam-capture-templates
      '(("d" "default" plain
         (file "~/Sync/org/zettel_template.org")
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n")
         :empty-lines-before 1
         :unnarrowed t)))

;;;;; Calendar and Diary

(setq calendar-mark-diary-entries-flag t)
(setq calendar-mark-holidays-flag t)
(setq calendar-time-display-form
      '(24-hours ":" minutes
                 (when time-zone
                   (format "(%s)" time-zone))))
(setq calendar-week-start-day 1)      ; Monday
(setq calendar-date-style 'iso)
(setq calendar-date-display-form calendar-iso-date-display-form)
(setq calendar-time-zone-style 'numeric)

(setq calendar-latitude 48.86
      calendar-longitude 2.35)

(setq calendar-standard-time-zone-name "+0100")
(setq calendar-daylight-time-zone-name "+0200")

(setq diary-file "~/Sync/emacs/diary")
(setq diary-date-forms diary-iso-date-forms)
(setq diary-comment-start ";;")
(setq diary-nonmarking-symbol "!")
(setq diary-number-of-entries 2)
(setq diary-mail-days 2)
(setq diary-abbreviated-year-flag nil)

(add-hook 'calendar-today-visible-hook #'calendar-mark-today)
(add-hook 'diary-list-entries-hook #'diary-sort-entries t)
(add-hook 'diary-mode-hook #'goto-address-mode) ; buttonise plain text links

;; These presuppose (setq diary-display-function #'diary-fancy-display)
(add-hook 'diary-list-entries-hook #'diary-include-other-diary-files)
(add-hook 'diary-mark-entries-hook #'diary-mark-included-diary-files)

(setq appt-warning-time-regexp "appt \\([0-9]+\\)")
(setq appt-message-warning-time 15)

(run-at-time 10 nil #'appt-activate 1)

(setq calendar-christian-all-holidays-flag nil)

(setq holiday-other-holidays
      '((holiday-fixed 01 26 "Australia Day holiday")
        (holiday-fixed 03 14 "Canberra Day holiday")
        (holiday-fixed 04 15 "Good Friday holiday")
        (holiday-fixed 05 26 "Ascension Day holiday")
        (holiday-fixed 06 06 "White Monday/Pentecôte holiday")
        (holiday-fixed 06 13 "Queen's Birthday holiday")
        (holiday-fixed 07 14 "Bastille Day/Fête Nationale holiday")
        (holiday-fixed 08 15 "Assumption of Mary/Assomption holiday")
        (holiday-fixed 10 03 "Labour Day (Aus) holiday")
        (holiday-fixed 11 01 "All saints/Toussaint holiday")
        (holiday-fixed 11 11 "Armistice Day 1918 holiday")
        (holiday-fixed 12 26 "Boxing Day holiday")
        (holiday-fixed 12 27 "Christmas in lieu holiday")))

(setq calendar-holidays (append holiday-christian-holidays
                                holiday-solar-holidays
                                holiday-local-holidays
                                holiday-other-holidays))

;;;;; Email Settings

;;;;;; Client-Agnostic Email Settings

(setq auth-source-pass-filename "~/Sync/password-store")
(auth-source-pass-enable)

(setq user-full-name "David Porter")
(setq user-mail-address "david@daporter.net")

(setq mail-user-agent 'message-user-agent)
(setq message-mail-user-agent t)      ; use `mail-user-agent'
(setq mail-signature "David Porter\n")
(setq message-signature "David Porter\n")
(setq message-citation-line-format "On %Y-%m-%d, %R %z, %f wrote:\n")
(setq message-citation-line-function nil)
(setq message-ignored-cited-headers nil) ; default is "." for all headers
(setq message-confirm-send nil)
(setq message-kill-buffer-on-exit t)
(setq message-wide-reply-confirm-recipients t)
(with-eval-after-load "mm-bodies"
  (add-to-list 'mm-body-charset-encoding-alist '(utf-8 . base64)))

(add-hook 'message-setup-hook #'message-sort-headers)

(with-eval-after-load "dired"
  (require 'gnus-dired))
(add-hook 'dired-mode-hook #'gnus-dired-mode) ; doesn’t require `gnus’

;;;;;; Notmuch

(add-to-list 'load-path "/usr/share/emacs/site-lisp/notmuch")
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
        (:name "emacs-humanities" :query "tag:list/emacs-humanities" :key "e")
        (:name "emacs-paris" :query "tag:list/emacs-paris" :key "p")
        (:name "notmuch" :query "tag:list/notmuch" :key "n")
        (:name "great-conversation" :query "tag:list/great-conversation" :key "g")
        (:name "mailing lists" :query "tag:lists" :key "m")))

(setq notmuch-tagging-keys '(("r" notmuch-show-mark-read-tags "Mark read")
                             ("a" notmuch-archive-tags "Archive")
                             ("f" ("+flagged") "Flag")
                             ("s" ("+spam" "-inbox") "Mark as spam")
                             ("d" ("+deleted" "-inbox") "Delete")))

;; Tags
(setq notmuch-archive-tags '("-inbox" "-unread" "+archived"))
(setq notmuch-draft-folder "Drafts")

;; Email Composition
(setq notmuch-mua-cite-function #'message-cite-original-without-signature)
(setq notmuch-mua-user-agent-function #'notmuch-mua-user-agent-full)

;; Reading Messages
(setq notmuch-show-indent-messages-width 0)
(setq notmuch-wash-wrap-lines-length 100)

;; Hooks and Keybindings
(add-hook 'notmuch-mua-send-hook #'notmuch-mua-attachment-check)
(add-hook 'notmuch-show-hook (lambda ()
                               (setq-local header-line-format nil)))

(let ((map global-map))
  (keymap-set map "C-c m" #'notmuch)
  (keymap-set map "C-x m" #'notmuch-mua-new-mail)) ; override `compose-mail'

;;;;;; Sending Email

(setq send-mail-function #'sendmail-send-it)

;;;;;; EBDB

(unless (package-installed-p 'ebdb)
  (package-install 'ebdb))

(require 'ebdb-message)
(require 'ebdb-notmuch)

(setq ebdb-mua-default-formatter #'ebdb-default-multiline-formatter)

(setq ebdb-sources "~/Sync/emacs/ebdb.gpg")
(setq ebdb-permanent-ignores-file "~/Sync/emacs/ebdb-permanent-ignores")

(setq ebdb-mua-pop-up nil)
(setq ebdb-default-window-size 0.25)

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

(with-eval-after-load "ebdb-com"
  (let ((map ebdb-mode-map))
    (keymap-set map "D" #'ebdb-delete-field-or-record)
    (keymap-set map "M" #'ebdb-mail) ; disables `ebdb-mail-each'
    (keymap-set map "m" #'ebdb-toggle-record-mark)
    (keymap-set map "t" #'ebdb-toggle-all-record-marks)
    (keymap-set map "T" #'ebdb-toggle-records-format) ; disables `ebdb-toggle-all-records-format'
    (keymap-set map "U" #'ebdb-unmark-all-records)))

;;;;; Bongo Music Manager

(unless (package-installed-p 'bongo)
  (package-install 'bongo))

(setq bongo-enabled-backends '(mpv vlc))

;;;;; Elfeed Feed Reader

(unless (package-installed-p 'elfeed)
  (package-install 'elfeed))

(setq elfeed-db-directory "~/Sync/emacs/elfeed/")
(setq elfeed-enclosure-default-dir "~/Downloads/")
(setq elfeed-search-filter "@4-months-ago +unread")
(setq elfeed-sort-order 'ascending)
(setq elfeed-search-title-max-width 100)
(setq elfeed-search-title-min-width 30)
(setq elfeed-search-trailing-width 25)
(setq elfeed-show-truncate-long-urls t)

(add-hook 'elfeed-show-mode-hook
          (lambda ()
            (setq-local shr-width (current-fill-column))))

(autoload #'elfeed-db-load "elfeed")

(defun my/elfeed-load-db-and-start ()
  "Load the Elfeed db from disk and start Elfeed."
  (interactive)
  (elfeed-db-load)
  (elfeed))

(defun my/elfeed-save-db-and-bury ()
  "Save the Elfeed db to disk and bury the Elfeed buffer."
  (interactive)
  (elfeed-db-unload)
  (quit-window))

(keymap-set global-map "C-c e" #'my/elfeed-load-db-and-start)

(with-eval-after-load 'elfeed
  (progn
    (require 'prot-elfeed)
    (setq prot-elfeed-feeds-file "~/Sync/emacs/feeds.el")
    (setq prot-elfeed-tag-faces t)
    (prot-elfeed-fontify-tags)
    (add-hook 'elfeed-search-mode-hook #'prot-elfeed-load-feeds))

  ;; Use alternating backgrounds, if `stripes' is available.
  (with-eval-after-load 'stripes
    (add-hook 'elfeed-search-mode-hook #'stripes-mode)
    ;; ;; To disable `hl-line-mode':
    ;; (advice-add #'elfeed-search-mode :after #'prot-common-disable-hl-line)
    )

  (let ((map elfeed-search-mode-map))
    (keymap-set map "s" #'prot-elfeed-search-tag-filter)
    (keymap-set map "o" #'prot-elfeed-search-open-other-window)
    (keymap-set map "q" #'my/elfeed-save-db-and-bury)
    (keymap-set map "v" #'prot-elfeed-mpv-dwim)
    (keymap-set map "+" #'prot-elfeed-toggle-tag))
  (let ((map elfeed-show-mode-map))
    (keymap-set map "a" #'prot-elfeed-show-archive-entry)
    (keymap-set map "e" #'prot-elfeed-show-eww)
    (keymap-set map "v" #'prot-elfeed-mpv-dwim)
    (keymap-set map "+" #'prot-elfeed-toggle-tag)))

;;;;; Proced

(setq proced-auto-update-flag t)
(with-eval-after-load 'stripes
  (add-hook 'proced-mode-hook #'stripes-mode))

(with-eval-after-load "proced"
  (progn
    (require 'prot-proced)
    (prot-proced-extra-keywords 1)))

;;;;; Simple HTML Renderer (shr) and EWW

(setq browse-url-browser-function #'eww-browse-url)
(setq browse-url-secondary-browser-function #'browse-url-default-browser)

(setq shr-use-colors nil)             ; t is bad for accessibility
(setq shr-use-fonts nil)              ; t is not for me
(setq shr-max-image-proportion 0.6)
(setq shr-image-animate nil)          ; No GIFs, thank you!
(setq shr-discard-aria-hidden t)
(setq shr-cookie-policy nil)

(setq url-cookie-untrusted-urls '(".*"))

(setq eww-restore-desktop t)
(setq eww-header-line-format nil)
(setq eww-search-prefix "https://duckduckgo.com/html/?q=")
(setq eww-download-directory (expand-file-name "~/Downloads"))
(setq eww-suggest-uris '(eww-links-at-point
                         thing-at-point-url-at-point))
(setq eww-bookmarks-directory "~/Sync/emacs/eww-bookmarks/")
(setq eww-history-limit 150)
(setq eww-browse-url-new-window-is-tab nil)

(with-eval-after-load "eww"
  (progn
    (keymap-set eww-link-keymap "v" nil) ; stop overriding `eww-view-source'
    (keymap-set eww-mode-map "L" #'eww-list-bookmarks)
    (keymap-set eww-buffers-mode-map "d" #'eww-bookmark-kill)   ; it actually deletes
    (keymap-set eww-bookmark-mode-map "d" #'eww-bookmark-kill))) ; same

(with-eval-after-load "dired"
  (keymap-set dired-mode-map "E" #'eww-open-file)) ; to render local HTML files

(with-eval-after-load "eww"
  (progn
    (require 'prot-eww)
    (define-prefix-command 'prot-eww-map)
    (keymap-set global-map "C-c w" 'prot-eww-map)
    (let ((map prot-eww-map))
      (keymap-set map "b" #'prot-eww-visit-bookmark)
      (keymap-set map "e" #'prot-eww-browse-dwim)
      (keymap-set map "s" #'prot-eww-search-engine))
    (let ((map eww-mode-map))
      (keymap-set map "B" #'prot-eww-bookmark-page)
      (keymap-set map "D" #'prot-eww-download-html)
      (keymap-set map "F" #'prot-eww-find-feed)
      (keymap-set map "H" #'prot-eww-list-history)
      (keymap-set map "b" #'prot-eww-visit-bookmark)
      (keymap-set map "e" #'prot-eww-browse-dwim)
      (keymap-set map "o" #'prot-eww-open-in-other-window)
      (keymap-set map "E" #'prot-eww-visit-url-on-page)
      (keymap-set map "J" #'prot-eww-jump-to-url-on-page)
      (keymap-set map "R" #'prot-eww-readable)
      (keymap-set map "Q" #'prot-eww-quit))
    (add-hook 'prot-eww-history-mode-hook #'hl-line-mode)))

(setq prot-eww-save-history-file
      (locate-user-emacs-file "prot-eww-visited-history"))
(setq prot-eww-save-visited-history t)
(setq prot-eww-bookmark-link nil)

;;;;; Deft

(unless (package-installed-p 'deft)
  (package-install 'deft))

(setq deft-directory "~/Sync/zettelkasten")
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

(keymap-set global-map "C-c z d" #'deft)

;;;;; Zotero Reference Manager

(defun my/insert-zotero-reference ()
  "Invoke the Zotero reference chooser and insert the chosen reference.
Note: Zotero must be running and the `Better BibTeX' extension
must be installed."
  (interactive)
  (shell-command
   "curl -s http://127.0.0.1:23119/better-bibtex/cayw?format=formatted-bibliography"
   t))

(defun my/insert-zotero-citation ()
  "Invoke the Zotero reference chooser and insert the chosen citation.
Note: Zotero must be running and the `Better BibTeX' extension
must be installed."
  (interactive)
  (shell-command
   "curl -s http://127.0.0.1:23119/better-bibtex/cayw?format=formatted-citation"
   t))

(let ((map global-map))
  (keymap-set map "C-c z c" #'my/insert-zotero-citation)
  (keymap-set map "C-c z r" #'my/insert-zotero-reference))

;;;;; Anki Card Creation

(unless (package-installed-p 'anki-editor)
  (package-install 'anki-editor))

(keymap-set org-mode-map "C-c a n" #'anki-editor-insert-note)
(keymap-set org-mode-map "C-c a c" #'anki-editor-cloze-region)
(keymap-set org-mode-map "C-c a p" #'anki-editor-push-notes)

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

;;;;; Viewing PDFs

(unless (package-installed-p 'pdf-tools)
  (package-install 'pdf-tools))

(setq pdf-tools-enabled-modes           ; simplified from the defaults
      '(pdf-history-minor-mode
        pdf-isearch-minor-mode
        pdf-links-minor-mode
        pdf-outline-minor-mode
        pdf-misc-size-indication-minor-mode
        pdf-occur-global-minor-mode))
(setq-default pdf-view-display-size 'fit-page)
(setq pdf-view-max-image-width 1080)
(setq pdf-outline-imenu-use-flat-menus t)

(pdf-loader-install)

;; The following functions and hooks are adapted from the manual modus-themes.

(defun my/pdf-tools-backdrop ()
  "Set backdrop distinct from the background of the PDF page."
  (face-remap-add-relative
   'default
   `(:background ,(modus-themes-color 'bg-alt))))

(defun my/pdf-tools-midnight-mode-toggle ()
  "Make pdf-tools adapt to `modus-themes-toggle'."
  (when (derived-mode-p 'pdf-view-mode)
    (if (eq (car custom-enabled-themes) 'modus-vivendi)
        (pdf-view-midnight-minor-mode 1)
      (pdf-view-midnight-minor-mode -1))
    (my/pdf-tools-backdrop)))

(add-hook 'pdf-tools-enabled-hook #'my/pdf-tools-midnight-mode-toggle)
(add-hook 'modus-themes-after-load-theme-hook #'my/pdf-tools-midnight-mode-toggle)

;;;;;; Open PDFs From Org Mode

(unless (package-installed-p 'org-pdftools)
  (package-install 'org-pdftools))

(add-hook 'org-mode-hook #'org-pdftools-setup-link)

;;;;; Viewing EPUBs

(unless (package-installed-p 'nov)
  (package-install 'nov))

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;;;;; Kbd-Mode for KMonad

(add-to-list 'load-path (locate-user-emacs-file "lisp/kbd-mode"))
(require 'kbd-mode)

;;;; General Interface and Interactions

;;;;; Jump to Visible Position (avy)

(unless (package-installed-p 'avy)
  (package-install 'avy))

(setq avy-all-windows nil)              ; only the current window
(setq avy-all-windows-alt t)            ;  all windows with C-u
(setq avy-keys '(?u ?h ?e ?t ?a ?s ?o ?n))

(keymap-set global-map "C-," #'avy-goto-char-timer)

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

(with-eval-after-load "moody"
  (progn
    (require 'prot-moody)
    (setq prot-moody-font-height-multiplier 1.35)
    ;; Also check the Modus themes' `modus-themes-mode-line' which can set
    ;; the styles specifically for Moody.
    (prot-moody-set-height -1)))

;;;;;; Mode Line Recursion Indicators

(unless (package-installed-p 'recursion-indicator)
  (package-install 'recursion-indicator))

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

;;;;; Window Divider Mode

(setq window-divider-default-right-width 1)
(setq window-divider-default-bottom-width 1)
(setq window-divider-default-places 'right-only)

;;;;; Line Numbers and Relevant Indicators

(require 'prot-sideline)
;; Set absolute line numbers.  A value of "relative" is also useful.
(setq display-line-numbers-type t)
;; Those two variables were introduced in Emacs 27.1
(setq display-line-numbers-major-tick 0)
(setq display-line-numbers-minor-tick 0)
;; Use absolute numbers in narrowed buffers
(setq-default display-line-numbers-widen t)

(unless (package-installed-p 'diff-hl)
  (package-install 'diff-hl))

(setq diff-hl-draw-borders nil)
(setq diff-hl-side 'left)

(setq hl-line-sticky-flag nil)
(setq hl-line-overlay-priority -50) ; emacs28

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

(defun my/whitespace-style-allow-long-lines ()
  "Set `whitespace-style’ to allow long lines."
  (setq-local whitespace-style
              (remove 'lines-tail whitespace-style)))

;; Long lines are allowed in certain modes.
(dolist (hook '(markdown-mode-hook
                org-mode-hook))
  (add-hook hook #'my/whitespace-style-allow-long-lines))

(let ((map global-map))
  (keymap-set map "<f6>" #'prot-sideline-negative-space-toggle)
  (keymap-set map "<f7>" #'prot-sideline-mode))

;;;;; Outline Mode

(setq-default outline-minor-mode-highlight 'override)

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

(setq stripes-unit 1)

;;;;; Conveniences and Minor Extras

;;;;;; Automatic Time Stamps for Files

(add-hook 'before-save-hook #'time-stamp)

;;;;;; Auto-Revert Mode

(global-auto-revert-mode 1)

;;;;;; Preserve Contents of System Clipboard

(setq save-interprogram-paste-before-kill t)

;;;;;; Newline Characters for File Ending

(setq mode-require-final-newline 'visit-save)

;;;;;; Package Lists

(add-hook 'package-menu-mode-hook #'hl-line-mode)

;;;; Language Settings for Prose and Code

;;;;; Support for Various Major Modes

;;;;;; Emacs Lisp

(defun my/setup-emacs-lisp-mode ()
  "Set up Emacs Lisp mode according to my preferences."
  (setq outline-regexp ";;;+ [^ ]")
  (outline-minor-mode 1))

(add-hook 'emacs-lisp-mode-hook #'my/setup-emacs-lisp-mode)

;;;;;; Markdown

(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode))

;;;;;; YAML

(unless (package-installed-p 'yaml-mode)
  (package-install 'yaml-mode))

(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

;;;;; Paragraphs

(setq-default fill-column 72)
(add-hook 'text-mode-hook #'turn-on-visual-line-mode)

(unless (package-installed-p 'adaptive-wrap)
  (package-install 'adaptive-wrap))

(add-hook 'text-mode-hook #'adaptive-wrap-prefix-mode)

(column-number-mode 1)

;;;;;; Titlecasing

(unless (package-installed-p 'titlecase)
  (package-install 'titlecase))

(setq titlecase-style 'mla)

(with-eval-after-load "embark"
  (progn
    (keymap-set embark-heading-map "t" #'titlecase-line)
    (keymap-set embark-region-map "t" #'titlecase-region)))

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

(defun my/turn-on-indent-tabs-mode ()
  "Enable `indent-tabs-mode’."
  (setq indent-tabs-mode t))

;; Use tab characters for indentation in certain modes.
(dolist (hook '(sh-mode-hook
                python-mode-hook
                c-mode-common-hook
                nxml-mode-hook))
  (add-hook hook #'my/turn-on-indent-tabs-mode))

(defun my/set-tab-width-nxml-mode ()
  "Set my preferred ‘tab-width’ for `nxml-mode’."
  (setq tab-width 2))

(add-hook 'nxml-mode-hook #'my/set-tab-width-nxml-mode)

(setq-default tab-always-indent 'complete)
(setq-default tab-first-completion 'word-or-paren-or-punct)

;;;;;; Smart Tabs for Indenting With Tabs and Aligning With Spaces

(unless (package-installed-p 'smart-tabs-mode)
  (package-install 'smart-tabs-mode))

(smart-tabs-add-language-support sh sh-mode-hook
  ((smie-indent-line . sh-basic-offset)))

(smart-tabs-insinuate 'c 'c++ 'java 'javascript 'cperl 'python 'ruby 'sh 'nxml)

(defun my/whitespace-style-ignore-indentation ()
  "Set `whitespace-style’ to ignore ignore indentation."
  (setq-local whitespace-style
              (remove 'indentation whitespace-style)))

;; `whitespace-cleanup’ shouldn’t touch indentation in modes that use
;; Smart Tabs mode.
(dolist (hook '(sh-mode-hook
                python-mode-hook
                nxml-mode-hook))
  (add-hook hook #'my/whitespace-style-ignore-indentation))

;;;;; Spell Checking

(setq flyspell-issue-message-flag nil)
(setq flyspell-issue-welcome-flag nil)
(setq ispell-dictionary "australian-w_accents")

(let ((map global-map))
  (keymap-set map "C-M-;" #'flyspell-goto-next-error)
  (keymap-set map "C-;" #'flyspell-auto-correct-word))

(with-eval-after-load "flyspell"
  (progn
    (require 'prot-spell)
    (setq prot-spell-dictionaries
          '(("EN English" . "australian-w_accents")
            ("FR Français" . "francais-lrg")
            ("NL Nederlands" . "dutch")
            ("ES Espanõl" . "español")))
    (let ((map global-map))
      (keymap-set map "M-$" #'prot-spell-spell-dwim)
      (keymap-set map "C-M-$" #'prot-spell-change-dictionary))))

;;;;; Code and Text Linters

;;;;;; Flymake

(setq flymake-fringe-indicator-position 'left-fringe)
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

(with-eval-after-load "flymake"
  (let ((map flymake-mode-map))
    (keymap-set map "C-c ! s" #'flymake-start)
    (keymap-set map "C-c ! d" #'flymake-show-buffer-diagnostics)
    (keymap-set map "C-c ! n" #'flymake-goto-next-error)
    (keymap-set map "C-c ! p" #'flymake-goto-prev-error)))

(unless (package-installed-p 'flymake-diagnostic-at-point)
  (package-install 'flymake-diagnostic-at-point))

(setq flymake-diagnostic-at-point-display-diagnostic-function
      #'flymake-diagnostic-at-point-display-minibuffer)

(add-hook 'prog-mode-hook #'flymake-mode)

;;;;;; Flymake + Shellcheck

(unless (package-installed-p 'flymake-shellcheck)
  (package-install 'flymake-shellcheck))

(add-hook 'sh-mode-hook #'flymake-shellcheck-load)

;;;;;; Flymake + Proselint

(unless (package-installed-p 'flymake-proselint)
  (package-install 'flymake-proselint))

(dolist (hook '(markdown-mode-hook
                org-mode-hook
                text-mode-hook))
  (add-hook hook #'flymake-proselint-setup)
  (add-hook hook #'flymake-mode))

;;;;;; Flymake + Markdown

(unless (package-installed-p 'flymake-markdownlint)
  (package-install 'flymake-markdownlint))

(add-hook 'markdown-mode-hook #'flymake-markdownlint-setup)

;;;;;; Flymake + CSS

(unless (package-installed-p 'flymake-css)
  (package-install 'flymake-css))

(add-hook 'css-mode-hook 'flymake-css-load)

;;;;; Eldoc

(global-eldoc-mode 1)

;;;; History and State

;;;;; Emacs Server and Desktop

(server-start)

(setq desktop-dirname user-emacs-directory)
(setq desktop-base-file-name "desktop")
(setq desktop-globals-to-clear nil)
(setq desktop-restore-eager 0)
(setq desktop-restore-frames nil)
(with-eval-after-load "desktop"
  (dolist (symbol '(kill-ring
                    log-edit-comment-ring))
    (add-to-list 'desktop-globals-to-save symbol)))
(desktop-save-mode 1)

;;;;; Record Various Types of History

(setq save-place-file (locate-user-emacs-file "saveplace"))
(save-place-mode 1)

;;;;; Backups

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backup/"))))
(setq backup-by-copying t)
(setq version-control t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq create-lockfiles nil)

;;; init.el ends here

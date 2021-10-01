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

(global-set-key (kbd "C-c p") #'package-list-packages)

;;;; Base Settings

(setq custom-file (make-temp-file "emacs-custom-")) ; disable it!

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

(internal-set-lisp-face-attribute 'default :family "Iosevka Comfy" 0)
(internal-set-lisp-face-attribute 'default :weight 'book 0)
(internal-set-lisp-face-attribute 'default :height 90 0)
(internal-set-lisp-face-attribute 'fixed-pitch :family "Iosevka Comfy" 0)
(internal-set-lisp-face-attribute 'fixed-pitch :weight 'book 0)
(internal-set-lisp-face-attribute 'variable-pitch :family "Noto Serif" 0)
(internal-set-lisp-face-attribute 'variable-pitch :weight 'normal 0)
(internal-set-lisp-face-attribute 'variable-pitch :height 1.0 0)
(set-face-attribute 'bold nil :weight 'extrabold)
(setq-default line-spacing 2)
(setq-default text-scale-remap-header-line t)
(setq x-underline-at-descent-line t)

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
(setq completion-show-help nil)
(setq completion-auto-help t)
(setq completion-ignore-case t)
(setq-default case-fold-search t)   ; For general regexp

;; The following two are updated in Emacs 28.  They concern the
;; *Completions* buffer.
(setq completions-format 'one-column)
(setq completions-detailed t)

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

(add-hook 'completion-list-mode-hook #'prot-common-truncate-lines-silently) ; from `prot-common.el'

(let ((map completion-list-mode-map))
  (define-key map (kbd "<tab>") #'choose-completion)
  (define-key map (kbd "M-v") #'scroll-down-command))
(let ((map minibuffer-local-completion-map))
  (define-key map (kbd "C-j") #'exit-minibuffer)
  (define-key map (kbd "<tab>") #'minibuffer-force-complete))
(let ((map minibuffer-local-must-match-map))
  ;; I use this prefix for other searches
  (define-key map (kbd "M-s") nil))

(require 'prot-minibuffer)
(setq-default prot-minibuffer-mini-cursors t) ; also check `prot-cursor.el'
(setq prot-minibuffer-remove-shadowed-file-names t)
(setq prot-minibuffer-minimum-input 3)

;; ;; NOTE: `prot-minibuffer-completion-blocklist' can be used for
;; ;; commands with lots of candidates, depending also on how low
;; ;; `prot-minibuffer-minimum-input' is.  With my current settings,
;; ;; this is not required, otherwise I would use this list:
;;
;; '( describe-symbol describe-function
;;    describe-variable execute-extended-command
;;    insert-char)
(setq prot-minibuffer-completion-blocklist nil)

;; This is for commands that should always pop up the completions'
;; buffer.  It circumvents the default method of waiting for some user
;; input (see `prot-minibuffer-minimum-input') before displaying and
;; updating the completions' buffer.
(setq prot-minibuffer-completion-passlist
      '( vc-retrieve-tag embark-prefix-help-command org-capture
         prot-bongo-playlist-insert-playlist-file
         prot-bookmark-cd-bookmark))

(define-key global-map (kbd "C-x :") #'prot-minibuffer-focus-mini-or-completions)
(let ((map completion-list-mode-map))
  (define-key map (kbd "h") #'prot-simple-describe-symbol) ; from `prot-simple.el'
  (define-key map (kbd "M-g") #'prot-minibuffer-choose-completion-number)
  (define-key map (kbd "M-e") #'prot-minibuffer-edit-completion)
  (define-key map (kbd "C-g") #'prot-minibuffer-keyboard-quit-dwim)
  (define-key map (kbd "C-n") #'prot-minibuffer-next-completion-or-mini)
  (define-key map (kbd "<down>") #'prot-minibuffer-next-completion-or-mini)
  (define-key map (kbd "C-p") #'prot-minibuffer-previous-completion-or-mini)
  (define-key map (kbd "<up>") #'prot-minibuffer-previous-completion-or-mini)
  (define-key map (kbd "<right>") #'prot-minibuffer-completion-next-group)
  (define-key map (kbd "<left>") #'prot-minibuffer-completion-previous-group)
  (define-key map (kbd "<return>") #'prot-minibuffer-choose-completion-exit)
  (define-key map (kbd "<M-return>") #'prot-minibuffer-choose-completion-dwim)
  (define-key map (kbd "M-<") #'prot-minibuffer-beginning-of-buffer)
  ;; Those are generic actions for the "*Completions*" buffer, though
  ;; I normally use `embark'.
  (define-key map (kbd "w") #'prot-minibuffer-completions-kill-symbol-at-point)
  (define-key map (kbd "i") #'prot-minibuffer-completions-insert-symbol-at-point)
  (define-key map (kbd "j") #'prot-minibuffer-completions-insert-symbol-at-point-exit))
(let ((map minibuffer-local-completion-map))
  (define-key map (kbd "M-g") #'prot-minibuffer-choose-completion-number)
  (define-key map (kbd "M-e") #'prot-minibuffer-edit-completion)
  (define-key map (kbd "C-n") #'prot-minibuffer-switch-to-completions-top)
  (define-key map (kbd "<down>") #'prot-minibuffer-switch-to-completions-top)
  (define-key map (kbd "C-p") #'prot-minibuffer-switch-to-completions-bottom)
  (define-key map (kbd "<up>") #'prot-minibuffer-switch-to-completions-bottom)
  (define-key map (kbd "C-l") #'prot-minibuffer-toggle-completions)) ; "list" mnemonic
(let ((map minibuffer-local-filename-completion-map))
  (define-key map (kbd "<backspace>") #'prot-minibuffer-backward-updir))

(add-hook 'minibuffer-setup-hook #'prot-minibuffer-mini-cursor)
(let ((hook 'completion-list-mode-hook))
  (add-hook hook #'prot-minibuffer-completions-cursor)
  (add-hook hook #'prot-minibuffer-hl-line)
  (add-hook hook #'prot-minibuffer-completions-stripes)
  (add-hook hook #'prot-minibuffer-display-line-numbers))

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

(setq embark-collect-initial-view-alist '((t . list)))
(setq embark-cycle-key (kbd "C-."))   ; see the `embark-act' key
(setq embark-collect-live-update-delay 0.5)
(setq embark-collect-live-initial-delay 0.8)
(setq embark-indicator #'embark-mixed-indicator)
(setq embark-verbose-indicator-excluded-actions
      '("\\`embark-collect-" "\\`customize-" "\\(local\\|global\\)-set-key"
        set-variable embark-cycle embark-export
        embark-keymap-help embark-become embark-isearch))
(setq embark-mixed-indicator-delay 1.2)
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
        ("\\`\\*Async Shell Command\\*\\'"
         (display-buffer-no-window))
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
        ("\\*\\(Flymake\\|Package-Lint\\).*"
         (display-buffer-in-side-window)
         (window-height . 0.16)
         (side . top)
         (slot . 0))
        ("\\*Messages.*"
         (display-buffer-in-side-window)
         (window-height . 0.16)
         (side . top)
         (slot . 1))
        ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\)\\*"
         (display-buffer-in-side-window)
         (window-height . 0.16)
         (side . top)
         (slot . 2))
        ;; bottom side window
        ("\\*Embark Actions.*"
         (display-buffer-in-side-window)
         (side . bottom)
         (slot . -1)
         (window-height . fit-window-to-buffer)
         (window-parameters . ((no-other-window . t)
                               (mode-line-format . none))))
        ("\\*\\(Embark\\)?.*Completions.*"
         (display-buffer-in-side-window)
         (side . bottom)
         (slot . 0)
         (window-parameters . ((no-other-window . t)
                               (mode-line-format . none))))
        ;; left side window
        ("\\*Help.*"            ; See the hooks for `visual-line-mode'
         (display-buffer-in-side-window)
         (window-width . 0.25)
         (side . left)
         (slot . -1))
        ("\\*Faces\\*"
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
        ;; bottom buffer (NOT side window)
        ("\\*\\(Output\\|Register Preview\\).*"
         (display-buffer-at-bottom))
        ("\\*\\vc-\\(incoming\\|outgoing\\|git : \\).*"
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (window-height . 0.2))
        ;; ("\\*.*\\(e?shell\\|v?term\\).*"
        ;;  (display-buffer-reuse-mode-window display-buffer-at-bottom)
        ;;  (window-height . 0.2))
        ;; below current window
        ("\\*\\(Calendar\\|Org Select\\|Bookmark Annotation\\).*"
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

;;;;;; Directional Window Motions (windmove)

(setq windmove-create-window nil)     ; Emacs 27.1
(let ((map global-map))
  ;; Those override some commands that are already available with
  ;; C-M-u, C-M-f, C-M-b.
  (define-key map (kbd "C-M-<up>") #'windmove-up)
  (define-key map (kbd "C-M-<right>") #'windmove-right)
  (define-key map (kbd "C-M-<down>") #'windmove-down)
  (define-key map (kbd "C-M-<left>") #'windmove-left)
  (define-key map (kbd "C-M-S-<up>") #'windmove-swap-states-up)
  (define-key map (kbd "C-M-S-<right>") #'windmove-swap-states-right) ; conflicts with `org-increase-number-at-point'
  (define-key map (kbd "C-M-S-<down>") #'windmove-swap-states-down)
  (define-key map (kbd "C-M-S-<left>") #'windmove-swap-states-left))

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
  (define-key map (kbd "C-<f8>") #'prot-tab-status-line) ; unopinionated alternative: `prot-tab-tab-bar-toggle'
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
(setq prot-logos-variable-pitch t)
(setq prot-logos-affect-prot-cursor t)
(define-key global-map (kbd "C-c f") #'prot-logos-focus-mode)

;;;;; Org Mode

;;;;;; Org Capture

(setq org-default-notes-file "~/inbox/notes.org")
(setq org-capture-templates
      '(("n" "Note" entry (file "")     ; use `org-default-notes-file’
         "* %<%Y-%m-%d %H:%M>\n  %?\n  %i")))

(defun dp-org-capture-note ()
  "Run `org-capture’ with the Note template."
  (interactive)
  (org-capture nil "n"))

(define-key global-map (kbd "C-c n") #'dp-org-capture-note)

;;;;;; Org Journal

(unless (package-installed-p 'org-journal)
  (package-install 'org-journal))
(require 'org-journal)
(setq org-journal-dir "~/journal")
(setq org-journal-file-format "%Y-%m-%d")
(setq org-journal-date-format "%A, %d %B %Y")

;;;;; Deft and Zetteldeft

(unless (package-installed-p 'deft)
  (package-install 'deft))
(require 'deft)
(setq deft-extensions '("md" "txt" "org"))
(setq deft-default-extension "md")
(setq deft-directory "~/zettelkasten")
(setq deft-use-filename-as-title t)

(unless (package-installed-p 'zetteldeft)
  (package-install 'zetteldeft))
(require 'zetteldeft)
(zetteldeft-set-classic-keybindings)
(setq zetteldeft-home-id "202101292226") ; the "index" note
(setq zetteldeft-link-indicator "[[")
(setq zetteldeft-link-suffix "]]")
(setq zetteldeft-title-prefix "# ")
(setq zetteldeft-list-prefix "* ")
(setq zetteldeft-id-format "%Y%m%d%H%M")
(setq zetteldeft-id-regex "[0-9]\\{12\\}")
(font-lock-add-keywords 'markdown-mode
                        `((,zetteldeft-id-regex
                           . font-lock-constant-face)))

;; Zetteldeft doesn't include the ID in the titles it generates. This function
;; fixes that.
(defun dp-zetteldeft-insert-title ()
  "Insert a title based on the file name."
  (interactive)
  (let ((title (file-name-base (buffer-file-name))))
    (save-excursion
      (goto-char (point-min))
      (zetteldeft--insert-title title)
      (newline))))

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

(defun dp-insert-zotero-bibliography ()
  "Invoke the Zotero reference chooser and insert the bibliography chosen.
Note: Zotero must be running and the `Better BibTeX' extension
must be installed."
  (interactive)
  (shell-command
   "curl -s http://127.0.0.1:23119/better-bibtex/cayw?format=formatted-bibliography"
   t))

(defun dp-insert-zotero-citation ()
  "Invoke the Zotero reference chooser and insert the bibliography chosen.
Note: Zotero must be running and the `Better BibTeX' extension
must be installed."
  (interactive)
  (shell-command
   "curl -s http://127.0.0.1:23119/better-bibtex/cayw?format=formatted-citation"
   t))

(define-key global-map (kbd "C-c z b") #'dp-insert-zotero-bibliography)
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
(setq notmuch-search-result-format '(("date" . "%12s  ")
                                     ("count" . "%-7s  ")
                                     ("authors" . "%-20s  ")
                                     ("subject" . "%-70s  ")
                                     ("tags" . "(%s)")))
(setq notmuch-tree-result-format '(("date" . "%12s  ")
                                   ("authors" . "%-20s  ")
                                   ((("tree" . "%s")
                                     ("subject" . "%s"))
                                    . " %-70s  ")
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

(setq mode-line-compact 'long)
(setq mode-line-position-column-line-format '(" %l,%c"))
(setq mode-line-defining-kbd-macro
      (propertize " Macro" 'face 'mode-line-emphasis))

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

;;;;;; Battery Status

(setq battery-mode-line-limit 95)
(display-battery-mode 1)

;;;;;; Display Current Time

(setq display-time-format "%Y-%m-%d %H:%M")
(setq display-time-default-load-average nil)
(setq display-time-use-mail-icon t)
(display-time-mode 1)

;;;;; Line Numbers and Relevant Indicators

(setq hl-line-sticky-flag nil)

(setq whitespace-line-column nil)       ; if nil, uses fill-column
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

;; Long lines are allowed in certain modes.
(dolist (hook '(markdown-mode-hook org-mode-hook))
  (add-hook hook (lambda ()
                   (setq-local whitespace-style
                               (remove 'lines-tail whitespace-style)))))

(define-key global-map (kbd "C-c w") #'delete-trailing-whitespace)

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

;;;;; Spell Checking

(require 'flyspell)
(setq flyspell-issue-message-flag nil)
(setq flyspell-issue-welcome-flag nil)
(setq ispell-dictionary "australian-w_accents")
(define-key flyspell-mode-map (kbd "C-;") nil)

(require 'prot-spell)
(setq prot-spell-dictionaries
      '(("EN English" . "australian-w_accents")
        ("FR Français" . "francais-lrg")
        ("NL Nederlands" . "dutch")
        ("ES Espanõl" . "español")))
(define-key global-map (kbd "C-M-$") #'prot-spell-change-dictionary)

;;;;;; Aode and Text Linters

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

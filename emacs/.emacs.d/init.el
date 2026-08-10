;; -*- lexical-binding: t; -*-

;;; Code:

;;;; Bootstrap

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

(use-package use-package
  :custom
  (use-package-verbose t)
  (use-package-minimum-reported-time 0)
  (use-package-enable-imenu-support t))

(use-package package-vc
  :custom
  (package-vc-allow-build-commands t))

(use-package emacs
  :preface
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
  (cursor-type 'box)
  (initial-buffer-choice t)             ; always start with *scratch*
  (frame-title-format '("%b"))
  (use-short-answers t)

  ;; Improve the spacing of underlines.
  (x-use-underline-position-properties nil)

  ;; Right-align mode-line content to the fringe, not the window edge,
  ;; so it isn't cut off by margin-setting modes like `olivetti-mode'.
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

  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)

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

  (advice-add #'completing-read-multiple
              :filter-args #'crm-indicator)

  :config
  (setq custom-file (concat user-emacs-directory "emacs-custom.el"))
  (load custom-file)

  ;; Installing anything via `:ensure' before this point would save
  ;; `package-selected-packages' -- and hence overwrite emacs-custom.el
  ;; on disk -- before that variable has been loaded from it above,
  ;; clobbering the existing list. `auto-compile' (which early-init.el
  ;; enables; see the `require' there) must therefore be installed no
  ;; earlier than here.

  (dolist (cmd '(upcase-region
                 downcase-region
                 narrow-to-region
                 set-goal-column
                 scroll-left))
    (put cmd 'disabled nil))

  ;; Specify the fonts to use for displaying emoji.
  (set-fontset-font t 'emoji
                    (cond
                     ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
                     ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
                     ((member "Symbola" (font-family-list)) "Symbola")))

  (defun my/insert-date-time (prefix)
    "Insert the current date and time.
  With PREFIX, use `ID' format, e.g. 20230323113431."
    (interactive "P")
    (let ((format (if (equal prefix '(4))
                      "%Y%m%d%H%M%S"
                    "%Y-%m-%d %H:%M:%S")))
      (insert (format-time-string format)))))

;; Installs the package early-init.el enables (see the `require' there);
;; keeps it, and its `packed' dependency, tracked in package-selected-packages
;; like any other package.
(use-package auto-compile
  :ensure t
  :config
  (auto-compile-on-load-mode 1)
  (auto-compile-on-save-mode 1))

(use-package custom
  :custom
  (custom-safe-themes t))

;;;; Core editing primitives

;; Indentation policy: tabs for indentation, spaces for alignment, via
;; `indent-tabs-mode''s factory default (t) -- `indent-to' already emits
;; tabs then spaces when it's non-nil, so no restating is needed.  Modes
;; with a hard requirement for spaces override it locally (`elisp-mode',
;; `python-mode', `yaml-ts-mode'); modes that require tabs (`make-mode')
;; already match the default.

(use-package simple
  :preface
  ;; https://karthinks.com/software/emacs-window-management-almanac/#org-target--pop-global-mark-advice
  ;;
  ;; By default, pop-global-mark always switches buffers (if required) in the
  ;; current window.  We’d like it to double as a window-switcher, which
  ;; requires a little advice:
  (define-advice pop-global-mark (:around (pgm) use-display-buffer)
    "Make `pop-to-buffer' jump buffers via `display-buffer'."
    (cl-letf (((symbol-function 'switch-to-buffer)
               #'pop-to-buffer))
      (funcall pgm)))
  :hook ((after-init . column-number-mode))
  :bind (("C-*" . undo-redo)             ; i.e., C-S-/ since undo is C-/
         ("C-z" . undo)                  ; was suspend-frame, never used
         ("C-S-z" . undo-redo)))         ; matches redo in non-Emacs apps

(use-package minibuffer
  :custom
  (completion-cycle-threshold 3))       ; TAB cycle if only few candidates

(use-package keymap
  :preface
  (defun my/goto-emacs-dir ()
    "Open Emacs directory"
    (interactive)
    (find-file user-emacs-directory))

  ;; Enable standard Linux unicode input.
  ;; https://emacs.stackexchange.com/questions/55994/unicode-input-from-keyboard-qmk-to-emacs
  (defun my/read-unicode-char (c1 c2 c3 c4 _trailing_space_ignored)
    "Convert unicode input C1 C2 C3 C4 to the corresponding insert char call."
    (interactive "c\nc\nc\nc\nc")
    (insert-char (string-to-number (format "%c%c%c%c" c1 c2 c3 c4) 16)))

  (defun my/load-config ()
    "Load config"
    (interactive)
    (load-file user-init-file)))

(use-package window
  :custom
  ;; By default, interactively switched buffers are exempt from the rules set in
  ;; ‘display-buffer-alist’ and friends. The following makes such buffers obey
  ;; the buffer display rules, making for a consistent buffer-switching
  ;; experience.
  (switch-to-buffer-obey-display-actions t)
  (fit-window-to-buffer-horizontally t)

  :config
  (add-to-list 'display-buffer-alist
               `(,(rx "*Warnings*")
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . (lambda (w) (fit-window-to-buffer w 10 5))))))

(use-package frame
  :custom
  (window-divider-default-right-width 1)
  :config
  (window-divider-mode 1))

(use-package repeat
  :init (repeat-mode 1)
  :hook (after-init)
  :bind ("C-_" . repeat))               ; reuse one of the ‘undo’ bindings

(use-package delsel
  :config
  (delete-selection-mode 1))

(use-package elec-pair
  :config
  (electric-pair-mode 1))

(use-package electric
  :config
  (electric-quote-mode 1))

(use-package paren
  :custom
  (show-paren-context-when-offscreen 'overlay))

;;;; Minibuffer completion framework

(use-package vertico
  :ensure t
  :hook (after-init)
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :custom
  (vertico-cycle t)
  :config
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

(use-package vertico-repeat
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind (("M-R" . vertico-repeat)
         :map vertico-map
         ("M-P" . vertico-repeat-previous)
         ("M-N" . vertico-repeat-next)))

(use-package prescient
  :ensure t
  :config
  (prescient-persist-mode 1))

(use-package vertico-prescient
  :ensure t
  :after (prescient vertico)
  ;; Keep orderless doing the matching; only take prescient's sorting.
  :custom
  (vertico-prescient-enable-filtering nil)
  :config
  (vertico-prescient-mode 1))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles partial-completion))
     ;; Let M-x, describe-function, describe-variable, etc. match
     ;; initialisms, e.g. "gtb" for "git-todos-browse".
     (command  (styles orderless+initialism))
     (symbol   (styles orderless+initialism))
     (variable (styles orderless+initialism))))
  (completion-category-defaults nil)
  ;; partial-completion behaves like substring:
  (completion-pcm-leading-wildcard t)
  :config
  (orderless-define-completion-style orderless+initialism
    (orderless-matching-styles '(orderless-initialism
                                 orderless-literal
                                 orderless-regexp))))

(use-package marginalia
  :ensure t
  :hook (after-init)
  :bind (:map minibuffer-local-map ("M-m" . marginalia-cycle)))

(use-package embark
  :ensure t
  ;; C-./M-. mirror right-click/left-click context menus at point (mnemonic:
  ;; both act at point).  Overwriting the default M-. (xref-find-definitions)
  ;; is fine since embark-dwim does the same thing on the symbol at point.
  :bind (("C-."   . embark-act)
         ("M-."   . embark-dwim)
         ("C-h B" . embark-bindings))
  :custom
  ;; Prompt for the action via completing-read (narrow by typing the
  ;; command name) instead of embark-keymap-prompter's which-key-style
  ;; grid of keys, which gets hard to scan once a target has many bound
  ;; actions.  Press `@' at the prompt to fall back to reading a single
  ;; key via embark-keymap-prompter for that invocation.
  (embark-prompter #'embark-completing-read-prompter)
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line in Embark Collect buffers (embark-export,
  ;; embark-collect) for a cleaner look.
  (add-to-list 'display-buffer-alist
               `(,(rx "*Embark Collect " (or "Live" "Completions") "*")
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package consult
  :ensure t
  :bind (([remap switch-to-buffer]              . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
         ([remap project-switch-to-buffer]      . consult-project-buffer)
         ([remap project-find-regexp]           . consult-ripgrep)
         ([remap yank-pop]                      . consult-yank-pop)
         ([remap imenu]                         . consult-imenu)
         ([remap Info-search]                   . consult-info)
         ([remap goto-line]                     . consult-goto-line)
         :map isearch-mode-map
         ("M-e"   . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         :map minibuffer-local-map
         ("M-r"   . consult-history))
  :custom
  (consult-narrow-key "<")
  (register-preview-delay 0.5)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :init
  (advice-add #'register-preview :override #'consult-register-window))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;;; In-buffer completion

(use-package corfu
  :ensure t
  :preface
  (defun my/corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer, unless Vertico is driving it."
    (unless (or (bound-and-true-p vertico--input)
                (eq (current-local-map) read-passwd-map))
      (setq-local corfu-auto nil)
      (corfu-mode 1)))
  :custom
  (corfu-cycle t)
  :custom-face
  ;; `corfu-default' has no explicit `:family', so it inherits whatever
  ;; `default' resolves to -- and `variable-pitch-mode' (markdown-mode/
  ;; org-mode) remaps `default' to a proportional face, breaking the
  ;; popup's character-count-based column padding.  Force a fixed-pitch
  ;; family directly here so it's immune to that remap.
  (corfu-default ((t (:inherit fixed-pitch))))
  ;; `corfu-annotations' inherits `completions-annotations', which
  ;; modus-themes recolors to a saturated blue nearly as prominent as the
  ;; candidate text; send it to `vertical-border' instead so the source
  ;; annotation reads as secondary (see the `nerd-icons-corfu-mapping'
  ;; comment below for why not plain `shadow').  Scoped to this face
  ;; rather than `completions-annotations' itself, which is also used
  ;; outside Corfu (e.g. the `*Completions*' buffer).
  (corfu-annotations ((t (:inherit (vertical-border italic)))))
  (corfu-popupinfo ((t (:inherit corfu-default :height 0.9))))
  :hook
  ((window-setup . global-corfu-mode)
   (minibuffer-setup . my/corfu-enable-in-minibuffer))
  :config
  (corfu-popupinfo-mode 1)
  ;; The annotation column (source metadata like "Dabbrev"/"Dict") is
  ;; right-aligned against the widest candidate in view, so whichever row
  ;; has that candidate gets zero gap before its annotation -- Corfu has
  ;; no customization variable for this padding.  `corfu--affixate' is a
  ;; `cl-defgeneric', Corfu's own extension point for this sort of thing
  ;; (it's how `corfu-terminal' and similar packages hook in), so extend
  ;; it to prepend one column of breathing room to every annotation.
  (require 'cl-lib)
  (cl-defmethod corfu--affixate :around (cands)
    (pcase-let ((`(,mf . ,acands) (cl-call-next-method)))
      (dolist (x acands)
        (unless (string-empty-p (caddr x))
          (setf (caddr x) (concat " " (caddr x)))))
      (cons mf acands))))

(use-package corfu-prescient
  :ensure t
  :after (prescient corfu)
  ;; Keep orderless doing the matching; only take prescient's sorting.
  :custom
  (corfu-prescient-enable-filtering nil)
  :config
  (corfu-prescient-mode 1))

(use-package completion-preview
  :hook (window-setup . global-completion-preview-mode)
  :custom
  ;; Debounce so rapid typing doesn't re-run every CAPF (including the
  ;; uncapped `cape-dict' grep in `my/text-mode-capf', see the
  ;; `cape-dict-limit' comment below) on each keystroke.
  (completion-preview-idle-delay 0.5)
  :custom-face
  ;; Drop the default underline and inherit `default' (undimmed) instead,
  ;; so the confirmed common prefix reads via color/brightness alone
  ;; against the dimmer `completion-preview' shadow text -- same
  ;; single-channel treatment `modus-themes-completions' uses for Corfu.
  (completion-preview-common ((t (:underline nil :inherit default))))
  ;; Default hardcodes a non-theme-aware green underline; use the
  ;; theme's own `success' face instead so it adapts with the theme.
  (completion-preview-exact ((t (:underline nil :inherit success)))))

;; Note on CAPFs: completion is two-step -- each entry on
;; `completion-at-point-functions' is first asked only whether it *can*
;; complete here, and the first to say yes "wins" before being asked for
;; actual completions; if that CAPF then returns none, completion just
;; ends rather than falling back to the next one.  `:exclusive 'no' exists
;; to opt into that fallback, but nothing sets it in practice, so list
;; CAPFs in an order where they don't compete for the same context.
;; (https://www.reddit.com/r/emacs/comments/td0nth/comment/i0i8hi7/?context=3&share_id=CfzOVcILIBvpQKfRmTanK)
(use-package cape
  :ensure t
  :preface
  (defun my/eshell-capf ()
    (cape-capf-super #'pcomplete-completions-at-point
                     #'comint-completion-at-point
                     #'cape-history))
  (defun my/eshell-add-completions ()
    (add-hook 'completion-at-point-functions 'my/eshell-capf nil t))
  :bind ("C-c p" . cape-prefix-map)
  :custom
  ;; `cape-dict' greps `cape-dict-file' for the input as a substring (not
  ;; anchored to word-start), then caps hits at this limit before Emacs's
  ;; prefix-filtering runs -- since the dictionary is alphabetical, not
  ;; frequency-sorted, the default 100 is routinely exhausted by mid-word
  ;; matches before real prefix matches are reached.  Uncapped is still
  ;; <10ms here since completion is manually triggered (`corfu-auto' is
  ;; off), so there's no per-keystroke cost.
  (cape-dict-limit nil)
  :init
  ;; Add to the global default of `completion-at-point-functions' (used by
  ;; `completion-at-point'); order matters since the first function to
  ;; return a result wins.  Buffer-local completion functions take
  ;; precedence over this global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  :hook
  ((eshell-mode . my/eshell-add-completions)))

(use-package hippie-exp
  :bind
  ([remap dabbrev-expand] . hippie-expand)
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

(use-package dabbrev
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'reader-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(use-package which-key
  :hook
  (after-init))

;;;; Icons

(use-package nerd-icons
  :ensure t
  :defer t)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode 1)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :init
  ;; The bundled mapping colors the `text' kind (used by `cape-dabbrev'/
  ;; `cape-dict', so most prose completions) with `font-lock-doc-face', as
  ;; prominent as the candidate text -- dim it to match the
  ;; `corfu-annotations' treatment above.  Uses `vertical-border' rather
  ;; than `shadow', which in this theme isn't actually very dim;
  ;; `vertical-border' sits on modus-themes' quietest gray (`border').
  (setq nerd-icons-corfu-mapping
        (let ((mapping (copy-tree nerd-icons-corfu-mapping)))
          (plist-put (alist-get 'text mapping) :face 'vertical-border)
          mapping))
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :hook dired-mode)

(use-package nerd-icons-grep
  :ensure t
  :init
  (nerd-icons-grep-mode)
  :custom
  ;; Pre-requirement so an icon can be displayed next to each heading.
  (grep-use-headings t))

(use-package nerd-icons-ibuffer
  :ensure t
  :hook ibuffer-mode)

(use-package nerd-icons-xref
  :ensure t
  :init
  (nerd-icons-xref-mode))

;;;; Fonts, themes, visual UI

(use-package fontset
  ;; Set this to nil to set symbols entirely separately
  ;; Need it set to `t` in order to display org-modern-indent faces properly
  :custom
  (use-default-font-for-symbols t)
  :config
  ;; Use symbola for proper symbol glyphs, but have some fallbacks
  (set-fontset-font t 'symbol "Symbola" nil))

(use-package fontaine
  :ensure t
  :hook (after-init . fontaine-mode)
  :init
  (setq fontaine-presets
        '((regular)
          (large
           :default-height 110)
          (t
           :default-family "JetBrainsMono Nerd Font Mono"
           :default-height 100

           ;; Without this, `fixed-pitch' inherits whatever `default'
           ;; resolves to, and `variable-pitch-mode' (markdown-mode/org-mode)
           ;; remaps `default' to a scaled-up face -- fixed-pitch text
           ;; silently inflated along with it (measured: 15px instead of
           ;; the frame's true 13px).  An absolute height ignores that
           ;; remap, anchoring fixed-pitch to the frame default everywhere;
           ;; same bug class as the `corfu-default' `:family' fix above,
           ;; just for `:height'.
           :fixed-pitch-height 100

           ;; Same gap as `fixed-pitch' above -- unused today (nothing in
           ;; this config references `fixed-pitch-serif'), but pin it too
           ;; so it doesn't silently inherit a remapped `default' the day
           ;; something starts using it.
           :fixed-pitch-serif-height 100

           ;; 1.01, not the eyeballed-looking 1.05 this replaced: x-height is
           ;; what a reader perceives as "the size of the text" (not point
           ;; size, and not the ascent+descent `font-info' reports, which
           ;; mostly reflects each font's chosen line-spacing rather than
           ;; glyph size).  Per each font's `OS/2.sxHeight' normalized by
           ;; `head.unitsPerEm' -- JetBrainsMono 550/1000 = 0.5500, Inter
           ;; 1118/2048 = 0.5459 -- the two are already within ~0.75% of each
           ;; other at the same nominal point size, so only a small
           ;; correction is needed to match, not a 5% one.
           :variable-pitch-family "Inter"
           :variable-pitch-height 1.01

           :mode-line-active-family "Inter"
           :mode-line-active-height 1.01

           :mode-line-inactive-family "Inter"
           :mode-line-inactive-height 1.01)))
  :config
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular)))

(use-package modus-themes
  ;; The themes ship in `etc/themes' (on `custom-theme-load-path', not
  ;; `load-path'), so load with `require-theme' instead of use-package's
  ;; implicit require.  Must happen in `:preface' (wrapped in
  ;; `eval-and-compile', runs before `:init'/`:config') so the library is
  ;; loaded before `:config' sets the options below, including when
  ;; evaluating this form in isolation (e.g. `C-M-x').
  :preface
  (require-theme 'modus-themes)
  :no-require t
  ;; Use plain `setq' here, not use-package's `:custom': Custom replays a
  ;; variable's raw, unevaluated form via `eval' whenever it reprocesses
  ;; it (e.g. on a later `enable-theme'), and for
  ;; `modus-themes-common-palette-overrides' below that can run before its
  ;; preset variable is (re)defined, raising a void-variable error.
  ;; Options must be set before the theme loads; prefer
  ;; `modus-themes-load-theme' over `load-theme' since it also disables
  ;; conflicting themes and runs hooks.
  :config
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-variable-pitch-ui t)
  (setq modus-themes-headings
        '((1 . (variable-pitch 1.3))
          (2 . (variable-pitch 1.15))
          (3 . (1.05))
          (agenda-date . (1.2))
          (agenda-structure . (variable-pitch light 1.3))
          (t . (1.0))))
  ;; `regular' (not the default extrabold+underline) keeps completion
  ;; matches at normal weight with no underline, relying on their
  ;; existing colored foreground (blue/magenta-warmer/cyan/red, per
  ;; match index) as the single channel that marks a match -- stacking
  ;; weight+underline+color+background all at once (the default, and a
  ;; background-based variant tried after it) reads as an alert rather
  ;; than as informative.  A background fill in particular competes with
  ;; `corfu-current''s own background, which already uses that channel
  ;; to mean "selected row" -- one visual channel, one meaning.
  (setq modus-themes-completions
        '((matches . (regular))
          (selection . (semibold))))
  (setq modus-themes-common-palette-overrides modus-themes-preset-overrides-faint)
  (modus-themes-load-theme 'modus-operandi-tinted))

(use-package doric-themes
  :ensure t
  :demand t
  :config
  (setq doric-themes-to-rotate '(doric-light doric-marble doric-earth)))

(use-package ligature
  :ensure t
  :config
  ;; Enable all ligatures in programming modes.
  (ligature-set-ligatures
   '(prog-mode text-mode)
   '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->"
     "<---->" "<!--" "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>"
     "<==>" "<===>" "<====>" "<!---" "<~~" "<~" "~>" "~~>" "::" ":::" "=="
     "!=" "===" "!==" ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:"
     "-:" "=:" "<******>" "++" "+++"))
  (global-ligature-mode t))

(use-package page-break-lines
  :ensure t
  :hook (after-init . global-page-break-lines-mode))

(use-package pixel-scroll
  :hook (after-init . pixel-scroll-precision-mode))

(use-package face-remap
  :preface
  (defvar-local my/variable-pitch-serif-cookie nil
    "Face-remap cookie for this buffer's serif override of `variable-pitch',
or nil when showing the default (sans-serif) family.")

  (defun my/variable-pitch-line-spacing ()
    "Set this buffer's `line-spacing' to match its variable-pitch state.
XCharter (serif) is a book/text serif designed for generous leading and
has a smaller x-height than Inter, so it needs more extra leading; the
frame default (when not showing variable-pitch text) is tuned against
JetBrainsMono and applies to neither.

`variable-pitch-mode' is a thin wrapper around `buffer-face-mode' with
`buffer-face-mode-face' fixed at `variable-pitch' -- there's no
`variable-pitch-mode' variable or hook to key off of, so check the
underlying mode and face instead."
    (if (and buffer-face-mode (eq buffer-face-mode-face 'variable-pitch))
        (setq-local line-spacing (if my/variable-pitch-serif-cookie 0.3 0.2))
      (kill-local-variable 'line-spacing)))

  (defun my/variable-pitch-serif ()
    "Show this buffer's variable-pitch text in XCharter (serif) rather
than the default Inter (sans-serif)."
    (interactive)
    (unless my/variable-pitch-serif-cookie
      (setq my/variable-pitch-serif-cookie
            ;; XCharter's x-height is ~12% smaller than Inter's --
            ;; `OS/2.sxHeight' normalized by `head.unitsPerEm': XCharter
            ;; 481/1000 = 48.10%, Inter 1118/2048 = 54.59%.  Scale up to
            ;; match Inter's perceived size: 54.59/48.10 = 1.135.
            (face-remap-add-relative 'variable-pitch :family "XCharter" :height 1.135)))
    (my/variable-pitch-line-spacing))

  (defun my/variable-pitch-sans ()
    "Show this buffer's variable-pitch text in the default Inter (sans-serif)."
    (interactive)
    (when my/variable-pitch-serif-cookie
      (face-remap-remove-relative my/variable-pitch-serif-cookie)
      (setq my/variable-pitch-serif-cookie nil))
    (my/variable-pitch-line-spacing))

  (defun my/variable-pitch-toggle-serif ()
    "Toggle this buffer's variable-pitch face between serif and sans-serif."
    (interactive)
    (if my/variable-pitch-serif-cookie
        (my/variable-pitch-sans)
      (my/variable-pitch-serif)))
  :hook (buffer-face-mode . my/variable-pitch-line-spacing))

(use-package winner
  :custom
  (winner-dont-bind-my-keys t)   ; free C-c <left>/<right> for major modes (markdown-mode)
  :bind (("C--" . winner-undo)   ; mirrors C-/ -> undo
         ("C-+" . winner-redo))  ; mirrors C-* -> undo-redo (C-S--)
  :hook (after-init))

(use-package popper
  :ensure t
  :bind (("C-'"   . popper-toggle)
         ("M-'"   . popper-cycle)
         ("C-M-'" . popper-toggle-type))

  :custom
  (popper-reference-buffers '("\\*Messages\\*$"
                              "\\*Warnings\\*$"
                              help-mode
                              "\\*eldoc.*\\*$"
                              "\\*Occur\\*"
                              "\\*Shell Command Output\\*$"
                              "\\*Async Shell Command\\*$"
                              flymake-diagnostics-buffer-mode
                              compilation-mode))
  (popper-display-control nil)
  :config
  (popper-mode 1)
  (popper-echo-mode 1))

;;;; Programming support

(use-package prog-mode
  :preface
  (defun my/set-fill-column ()
    (setq-local fill-column 80))

  :hook
  (prog-mode . my/set-fill-column)
  (prog-mode . turn-on-auto-fill))

(use-package eldoc
  :custom
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (eldoc-echo-area-use-multiline-p nil)

  :config
  (add-to-list 'display-buffer-alist
               `(,(rx "*eldoc*")
                 (display-buffer-below-selected)
                 (dedicated . t)
                 (window-height . (lambda (w) (fit-window-to-buffer w 20 10))))))

(use-package treesit
  :preface
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
  :preface
  (defun my/eglot-setup-eldoc ()
    (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly))
  (defun my/eglot-disable-hints ()
    (eglot-inlay-hints-mode 0))

  ;; Some servers (e.g. vscode-json-language-server's "Sort JSON object
  ;; keys") advertise code actions backed by commands they never
  ;; implement server-side, so `workspace/executeCommand' comes back as
  ;; method-not-found instead of running the action.  Report that as a
  ;; message instead of a raw jsonrpc-error backtrace.
  (defun my/eglot-execute-ignore-unhandled-method (orig-fun server action)
    (condition-case err
        (funcall orig-fun server action)
      (jsonrpc-error
       (if (eql (alist-get 'jsonrpc-error-code (cdr err)) -32601)
           (eglot--message "Server does not support this code action")
         (signal (car err) (cdr err))))))

  :hook
  (((c-ts-mode css-ts-mode json-ts-mode) . eglot-ensure)
   (eglot-managed-mode      . my/eglot-setup-eldoc)
   (eglot-managed-mode      . my/eglot-disable-hints))

  :config
  (advice-add 'eglot-execute :around #'my/eglot-execute-ignore-unhandled-method))

(use-package dape
  :ensure t
  :commands dape
  :custom
  (dape-buffer-window-arrangement 'right)
  (dape-inlay-hints t)
  :config
  ;; Kill compile buffer on build success:
  (add-hook 'dape-compile-hook 'kill-buffer))

(use-package flymake
  :hook (prog-mode text-mode)
  :custom (flymake-fringe-indicator-position nil)
  :config
  (add-to-list 'display-buffer-alist
               '((major-mode . flymake-diagnostics-buffer-mode)
                 (display-buffer-reuse-window
                  display-buffer-in-direction)
                 (direction . above)
                 (window-height . (lambda (w) (fit-window-to-buffer w 20 10)))
                 (dedicated . t)
                 (body-function . select-window))))

(use-package compile
  :hook
  (compilation-filter . ansi-color-compilation-filter)
  :custom
  (compilation-auto-jump-to-first-error 'if-location-known))

(use-package gdb-mi
  :commands (gdb)
  :custom
  (gdb-many-windows t)
  (gdb-default-window-configuration-file "gdb-config")
  (gdb-restore-window-configuration-after-quit t))

(use-package hideshow
  ;; https://github.com/karthink/.emacs.d/blob/master/lisp/setup-folds.el
  :commands (hs-cycle
             hs-global-cycle)
  :bind
  (:map prog-mode-map ("C-S-<iso-lefttab>" . hs-global-cycle))
  :config
  (setq hs-hide-comments-when-hiding-all nil
        ;; Nicer code-folding overlays (with fringe indicators)
        hs-set-up-overlay #'hideshow-set-up-overlay-fn)

  (defface hideshow-folded-face
    `((t (:inherit font-lock-comment-face :weight light)))
    "Face to hightlight `hideshow' overlays."
    :group 'hideshow)

  (defun hideshow-set-up-overlay-fn (ov)
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put
       ov 'display (propertize "  [...]  " 'face 'hideshow-folded-face))))

  (dolist (hs-command (list #'hs-cycle
                            #'hs-global-cycle))
    (advice-add hs-command :before
                (lambda (&optional end) "Advice to ensure `hs-minor-mode' is enabled"
                  (unless (bound-and-true-p hs-minor-mode)
                    (hs-minor-mode +1)))))

  (defun hs-cycle (&optional level)
    (interactive "p")
    (save-excursion
      (if (= level 1)
          (pcase last-command
            ('hs-cycle
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))
            ('hs-cycle-children
             ;;TODO: Fix this case. `hs-show-block' needs to be called twice to
             ;;open all folds of the parent block.
             (hs-show-block)
             (hs-show-block)
             (setq this-command 'hs-cycle-subtree))
            ('hs-cycle-subtree
             (hs-hide-block))
            (_
             (if (not (hs-already-hidden-p))
                 (hs-hide-block)
               (hs-hide-level 1)
               (setq this-command 'hs-cycle-children))))
        (hs-hide-level level)
        (setq this-command 'hs-hide-level))))

  (defun hs-global-cycle ()
    (interactive)
    (pcase last-command
      ('hs-global-cycle
       (save-excursion (hs-show-all))
       (setq this-command 'hs-global-show))
      (_ (hs-hide-all))))

  ;; extra folding support for more languages
  (unless (assq 't hs-special-modes-alist)
    (setq hs-special-modes-alist
          (append
           '((nxml-mode "<!--\\|<[^/>]*[^/]>"
                        "-->\\|</[^/>]*[^/]>"
                        "<!--" sgml-skip-tag-forward nil))
           hs-special-modes-alist
           '((t))))))

;;;; Editing utilities

(use-package newcomment
  :custom
  (comment-style 'extra-line))

(use-package isearch
  :preface
  ;; https://karthinks.com/software/emacs-window-management-almanac/#with-other-window-an-elisp-helper
  (defmacro with-other-window (&rest body)
    "Execute forms in BODY in the other-window."
    `(unless (one-window-p)
       (with-selected-window (other-window-for-scrolling)
         ,@body)))

  (defun isearch-other-window (regexp-p)
    (interactive "P")
    (with-other-window (isearch-forward regexp-p)))

  (defun isearch-other-window-backwards (regexp-p)
    (interactive "P")
    (with-other-window (isearch-backward regexp-p)))

  :bind (("C-M-s" . isearch-other-window)
         :map isearch-mode-map
         ;; Allow M-c to capitalise word and exit the search.  The original
         ;; command is still available via M-s c.
         ("M-c" . nil)))

(use-package replace
  :config
  (add-to-list 'display-buffer-alist
               `(,(rx "*Occur*")
                 (display-buffer-reuse-window
                  display-buffer-in-direction)
                 (direction . above)
                 (window-height . (lambda (w) (fit-window-to-buffer w 20 10)))
                 (dedicated . t)
                 (body-function . select-window))))

(use-package whitespace
  :preface
  (defun my/whitespace-toggle-chars ()
    "Toggle indicators for every tab and space character.
By default `whitespace-mode' only highlights whitespace that's
wrong (bad indentation, trailing, ...); this toggles `tabs',
`tab-mark', `spaces', and `space-mark' as a group on top of
that."
    (interactive)
    (whitespace-toggle-options '(tabs tab-mark spaces space-mark)))
  :custom
  (whitespace-line-column nil)
  ;; `indentation' flags indentation using the wrong character for the
  ;; buffer's own `indent-tabs-mode' -- e.g. spaces-only leading whitespace
  ;; once tabs are the default -- so only genuinely wrong whitespace is
  ;; highlighted by default.
  (whitespace-style '(face indentation trailing empty missing-newline-at-eof))
  ;; `my/whitespace-toggle-chars' (bound in `my/dispatch-menu') layers
  ;; `tabs tab-mark spaces space-mark' on top of this to mark every
  ;; tab/space character, not just wrong ones.  `tab-mark' has no face of
  ;; its own, so without `tabs' supplying `whitespace-tab' it'd fall back
  ;; to the alarming `escape-glyph' face; pairing them routes it through
  ;; `whitespace-tab' (set to `shadow' below) instead, same for spaces.
  :custom-face
  (whitespace-tab ((t (:inherit shadow))))
  (whitespace-space ((t (:inherit shadow))))
  (whitespace-hspace ((t (:inherit shadow))))
  :hook
  (((text-mode prog-mode conf-mode) . whitespace-mode)
   (before-save . delete-trailing-whitespace)))

(use-package apheleia
  :ensure t
  :hook ((prog-mode text-mode conf-mode) . apheleia-mode)
  :config
  ;; stylua only searches its own working directory by default; without
  ;; `--search-parent-directories' it never finds `~/.stylua.toml' (added
  ;; to pin lua-mode's indent policy -- see lua/.stylua.toml) for files
  ;; that live elsewhere under $HOME.
  (setf (alist-get 'stylua apheleia-formatters)
        '("stylua" "--search-parent-directories" "-"))
  ;; Upstream apheleia omits markdown from `apheleia-mode-alist' by design
  ;; ("so many people format markdown in so many different ways"), but
  ;; this repo's own markdown policy (spaces, tab-width 4; see
  ;; `my/markdown-set-indent-tabs-mode') is deliberate enough to enforce.
  ;; The stock `prettier-markdown' formatter needs no changes here -- its
  ;; --tab-width comes from the *.md override in ~/.prettierrc.json5
  ;; instead of from apheleia deriving it from an Emacs variable, since
  ;; apheleia has no indent-var case for markdown-mode/gfm-mode at all.
  (add-to-list 'apheleia-mode-alist '(markdown-mode . prettier-markdown))
  (add-to-list 'apheleia-mode-alist '(gfm-mode . prettier-markdown)))

(use-package editorconfig
  :hook
  (after-init))

(use-package titlecase
  :ensure t
  :after embark
  :commands (titlecase-region
             titlecase-line
             titlecase-sentence
             titlecase-dwim)
  :bind (:map embark-region-map
              ("T" . titlecase-region)
              :map embark-heading-map
              ("T" . titlecase-region))
  :custom (titlecase-style 'chicago))

(use-package string-inflection
  :ensure t
  :after embark
  :bind (:map embark-identifier-map
              ("k" . string-inflection-kebab-case)
              :map embark-region-map
              ("k" . string-inflection-kebab-case))
  ;; Default region behavior just replaces spaces with underscores instead
  ;; of running the inflection function -- useless for "kebab-case this
  ;; selection", so switch to applying it symbol-by-symbol.
  :custom (string-inflection-region-selection-behavior 'apply-to-each-symbols))

(use-package unfill
  :ensure t
  :commands (unfill-region unfill-paragraph))

(use-package lorem-ipsum
  :ensure t
  :commands (lorem-ipsum-insert-list
             lorem-ipsum-insert-paragraphs
             lorem-ipsum-insert-sentences))

(use-package abbrev
  :custom
  (abbrev-suggest t))

(use-package captain
  :ensure t
  :preface
  (defun my/captain-text-mode-setup ()
    ;; In text-mode, work all the time.
    (setq captain-predicate (lambda () t))
    (captain-mode 1))
  (defun my/captain-prog-mode-setup ()
    ;; In prog-mode, work only in comments.
    (setq captain-predicate (lambda ()
                              (nth 8 (syntax-ppss (point)))))
    (captain-mode 1))
  :hook
  (text-mode . my/captain-text-mode-setup)
  (prog-mode . my/captain-prog-mode-setup))

(use-package find-func
  :config
  (find-function-setup-keys))

(use-package autoinsert
  :config
  (auto-insert-mode t))

;;;; Files, projects, VC

;; Bare chord: `save-buffer' is the single most-used command in my logged
;; keybinding history, and `C-x C-s' puts repeated load on the awkward `x'
;; reach every time.
(keymap-global-set "C-#" #'save-buffer)

(use-package dired
  :defer t
  :custom
  (dired-recursive-copies   'always)
  (dired-dwim-target        t)          ; try to guess target directory for copy
  (dired-auto-revert-buffer t)
  (dired-vc-rename-file     t)

  :config
  (add-hook 'dired-mode-hook 'hl-line-mode)
  (add-hook 'dired-mode-hook 'dired-async-mode)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

(use-package project
  :commands (project-find-file
             project-switch-to-buffer
             project-switch-project
             project-switch-project-open-file))

(use-package beframe
  :ensure t
  :after project
  :custom
  (beframe-global-buffers '("^\\*scratch\\*$" "^\\*Messages\\*$"
                            "^\\*Backtrace\\*$" "^\\*Warnings\\*$"))
  ;; Run `project-prompt-project-dir' (the directory prompt that
  ;; `project-switch-project' reads from) in a new frame, so every
  ;; project switch lands in its own frame. See beframe's "Features of
  ;; beframe-mode" manual node for this as the recommended hook point.
  (beframe-functions-in-frames '(project-prompt-project-dir))
  :bind
  (:map global-map
        ("C-c b" . beframe-prefix-map))
  :config
  (beframe-mode 1)
  (defvar consult-buffer-sources)
  (declare-function consult--buffer-state "consult")
  (with-eval-after-load 'consult
    (defface beframe-buffer '((t :inherit font-lock-string-face))
      "Face for `consult' framed buffers."
      :group 'beframe)
    (defun my/beframe-buffer-names-sorted (&optional frame)
      "Return the list of buffers from `beframe-buffer-names' sorted by visibility.
With optional argument FRAME, return the list of buffers of FRAME."
      (beframe-buffer-names frame :sort #'beframe-buffer-sort-visibility))
    (defvar beframe-consult-source
      `( :name     "Frame-specific buffers (current frame)"
         :narrow   ?F
         :category buffer
         :face     beframe-buffer
         :history  beframe-history
         :items    ,#'my/beframe-buffer-names-sorted
         :action   ,#'switch-to-buffer
         :state    ,#'consult--buffer-state))
    (add-to-list 'consult-buffer-sources 'beframe-consult-source)))

(use-package autorevert
  :custom
  ;; Whenever the disk state changes, Emacs should update as well.
  (global-auto-revert-mode             t)
  (global-auto-revert-non-file-buffers t))

(use-package recentf
  :hook (after-init))

(use-package tramp
  :defer t
  :custom
  (tramp-default-method "sudo")
  (tramp-verbose 1)
  (tramp-connection-timeout 10)
  (tramp-chunksize 2000)
  (tramp-completion-reread-directory-timeout nil)
  (auto-save-default nil)
  (make-backup-files nil)
  (backup-directory-alist
   (append backup-directory-alist
           '(("/sudo::" . (concat user-emacs-directory "tramp-backups")))))
  (tramp-backup-directory-alist backup-directory-alist))

(use-package ediff
  :commands (ediff-buffers
             ediff-current-file
             ediff-files
             ediff-regions-linewise
             ediff-regions-wordwise)
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-keep-variants         nil)
  (ediff-show-clashes-only     t))

(use-package magit
  :ensure t
  :after project
  :commands magit-status
  :bind (:map project-prefix-map
              ("m" . magit-project-status))
  :custom
  (magit-display-buffer-function
   'magit-display-buffer-same-window-except-diff-v1)
  (git-commit-summary-max-length 50)
  (magit-diff-refine-hunk t)
  (git-commit-style-convention-checks '(non-empty-second-line
                                        overlong-summary-line))
  :config
  (add-to-list 'project-switch-commands
               '(magit-project-status "Magit" "m")))

(use-package hl-todo
  :ensure t
  :hook prog-mode)

;;;; Shells and processes

(use-package eshell
  :commands (eshell)
  :config
  (with-eval-after-load 'esh-mode
    (define-key eshell-mode-map [remap display-local-help] #'man))
  (dolist (module '(eshell-smart eshell-tramp))
    (add-to-list 'eshell-modules-list module)))

(use-package em-hist
  :preface
  (defun my/remove-eshell-hist-binding ()
    "Remove binding C-<up>, since it’s used by windmove."
    (keymap-unset eshell-hist-mode-map "C-<up>" :remove))
  :hook (eshell-hist-load . my/remove-eshell-hist-binding))

(use-package shell
  :commands shell
  :bind (:map shell-mode-map
              ([remap display-local-help] . man)))

(use-package vterm
  :ensure t
  :commands vterm)

(use-package pcmpl-args
  :ensure t
  :after pcomplete)

(use-package proced
  :commands (proced)
  :custom
  (proced-auto-update-flag t)
  (proced-enable-color-flag t))

(use-package server
  :config
  (server-start))

(use-package ghostel
  :ensure t
  :commands (ghostel)
  :bind (:map project-prefix-map
              ("t" . ghostel-project)
              ("T" . ghostel-project-list-buffers))
  :config
  (add-to-list 'project-switch-commands
               '(ghostel-project "Ghostel") t)
  (add-to-list 'project-switch-commands
               '(ghostel-project-list-buffers "Ghostel buffers") t)
  (add-to-list 'ghostel-eval-cmds
               '("magit-status-setup-buffer" magit-status-setup-buffer))
  (add-to-list 'display-buffer-alist
               '((major-mode . ghostel-mode)
                 (display-buffer-reuse-window
                  display-buffer-in-direction)
                 (direction . right)
                 (window-width . 100))))

;;;; Language modes

(use-package c-ts-mode
  :bind (:map c-ts-mode-map
              ("C-c o" . ff-find-other-file))
  :custom
  (c-ts-mode-indent-style 'linux)
  (c-ts-mode-indent-offset 8)
  :init
  ;; Only this binding actually needs `embark' loaded; gating the whole
  ;; package config behind `:after (eglot embark)' meant indent-style and
  ;; indent-offset above wouldn't apply until both happened to load, so a
  ;; .c file opened early in a session could silently get c-ts-mode's
  ;; factory GNU/2 instead of these.
  (with-eval-after-load 'embark
    (define-key embark-identifier-map "m" #'man)))

(use-package sh-script
  :preface
  ;; Match `sh-basic-offset' so each indent level is exactly one tab
  ;; character, rather than `indent-to' splitting some levels into a
  ;; tab-plus-spaces mix whenever the offset doesn't land on a tab stop.
  (defun my/sh-set-tab-width ()
    (setq tab-width 4))
  :init
  ;; Is there a more idiomatic way to do this?:
  (with-eval-after-load 'sh-script
    (define-key sh-mode-map [remap display-local-help] #'man))
  :mode
  (("\\.sh\\'" . bash-ts-mode)
   ("\\.bash\\'" . bash-ts-mode))
  :hook (bash-ts-mode . my/sh-set-tab-width)
  :config
  (add-to-list 'interpreter-mode-alist '("bash" . bash-ts-mode)))

(use-package python-mode
  :ensure t)

(use-package lua-mode
  :ensure t
  ;; No .stylua.toml exists, so stylua's own defaults (indent_type = Tabs,
  ;; indent_width = 4) are the convention actually in effect via apheleia.
  ;; Match them here so Emacs's own indentation agrees even if apheleia
  ;; isn't running.
  :preface
  (defun my/lua-set-tab-width ()
    (setq tab-width 4))
  :custom
  (lua-indent-level 4)
  :hook (lua-mode . my/lua-set-tab-width))

(use-package flymake-lua
  :ensure t)

(use-package mhtml-mode
  :preface
  (defun my/mhtml-set-indent-tabs-mode ()
    (setq indent-tabs-mode nil))
  :mode "\\.html\\'"
  :hook (mhtml-mode . my/mhtml-set-indent-tabs-mode))

(use-package css-mode
  :preface
  (defun my/css-set-indent-tabs-mode ()
    (setq indent-tabs-mode nil))
  :custom
  (css-indent-offset 2)
  :hook
  ((css-mode css-ts-mode) . my/css-set-indent-tabs-mode))

(use-package json-ts-mode
  :preface
  (defun my/json-set-indent-tabs-mode ()
    (setq indent-tabs-mode nil))
  :mode "\\.jsonc?\\'"
  :hook (json-ts-mode . my/json-set-indent-tabs-mode))

(use-package js
  :preface
  (defun my/js-set-indent-tabs-mode ()
    (setq indent-tabs-mode nil))
  :custom
  (js-indent-level 2)
  :hook (js-ts-mode . my/js-set-indent-tabs-mode))

(use-package typescript-ts-mode
  :preface
  (defun my/typescript-set-indent-tabs-mode ()
    (setq indent-tabs-mode nil))
  :hook (typescript-ts-mode . my/typescript-set-indent-tabs-mode))

(use-package conf-mode
  :mode "\\.service\\'")

(use-package yaml-ts-mode
  :preface
  (defun my/yaml-set-tab-width ()
    (setq tab-width 2))
  :mode "\\.ya?ml\\'"
  :hook (yaml-ts-mode . my/yaml-set-tab-width))

(use-package flymake-yamllint
  :ensure t
  :after yaml-ts-mode
  :hook
  (yaml-ts-mode . flymake-yamllint-setup))

(use-package hyprlang-ts-mode
  :ensure t
  ;; No community convention pulls this toward spaces (it's Hyprland's own
  ;; config format), so keep the tabs default and match tab-width to the
  ;; offset instead, same fix as sh-basic-offset/tab-width in `sh-script'.
  :preface
  (defun my/hyprlang-set-tab-width ()
    (setq tab-width 4))
  :custom
  (hyprlang-ts-mode-indent-offset 4)
  :hook (hyprlang-ts-mode . my/hyprlang-set-tab-width))

(use-package make-mode
  :preface
  ;; Builds on the global `whitespace-style' (recipe lines require a
  ;; literal tab, and `make-mode' itself sets `indent-tabs-mode' t, so the
  ;; adaptive `indentation' element already flags space-indented recipes
  ;; correctly) rather than duplicating it, adding only the two checks
  ;; specific to Makefiles' tab/space pitfalls: a tab followed by a run of
  ;; alignment spaces, and the classic space-then-tab mistake.
  (defun my/makefile-whitespace-setup ()
    (setq-local whitespace-style
                (append '(space-after-tab::tab space-before-tab::space)
                        whitespace-style)))
  :hook
  (makefile-mode . my/makefile-whitespace-setup))

(use-package csv-mode
  :ensure t
  :preface
  (defun my/csv-mode-setup ()
    (progn
      (visual-line-mode -1)
      (toggle-truncate-lines 1)))
  :mode "\\.csv\\'"
  :hook
  ((csv-mode . my/csv-mode-setup)
   (csv-mode . csv-guess-set-separator)
   (csv-mode . csv-align-mode)))

(use-package elisp-mode
  :preface
  ;; Elisp indentation depth is frequently not a multiple of `tab-width' --
  ;; e.g. a wrapped argument list lines up under the first argument, not
  ;; under the next tab stop -- so tabs-for-depth doesn't apply cleanly, and
  ;; the community (including Emacs's own source tree) indents with spaces.
  (defun my/elisp-set-indent-tabs-mode ()
    (setq indent-tabs-mode nil))
  :hook (emacs-lisp-mode . my/elisp-set-indent-tabs-mode))

;;;; Text and writing modes

(use-package text-mode
  :preface
  (defun my/text-mode-capf ()
    ;; `cape-wrap-super' (not the curried `cape-capf-super') since this
    ;; function is itself used directly as a `completion-at-point-functions'
    ;; entry, so it must return completion data now rather than a capf for
    ;; something else to call later.  A bare function here structurally
    ;; matches Corfu's pcase pattern but fails its `integer-or-marker-p'
    ;; check on BEG, so Corfu silently falls through to the plain
    ;; `cape-dabbrev' entry below -- which is why only dabbrev candidates
    ;; used to appear.
    (cape-wrap-super #'cape-dabbrev #'cape-dict))
  (defun my/text-mode-add-completions ()
    (add-hook 'completion-at-point-functions #'my/text-mode-capf nil t))
  :hook ((text-mode . abbrev-mode)
         (text-mode . my/text-mode-add-completions))
  :config
  ;; For some reason the following doesn't work with :bind
  (define-key text-mode-map (kbd "C-M-i") #'completion-at-point))

(use-package visual-wrap-prefix-mode
  :preface
  (defun my/toggle-visual-wrap-prefix-mode ()
    "Enable/disable `visual-wrap-prefix-mode' based on `visual-line-mode' status.
Skipped in agent-shell viewport buffers: their indentation is a
`line-prefix'/`wrap-prefix' text property agent-shell sets itself, and
this mode's own `fill-context-prefix' computation (based on literal
leading whitespace) overwrites it with an empty prefix — and disabling
the mode later would wipe every `wrap-prefix' in the buffer outright."
    (unless (derived-mode-p 'agent-shell-viewport-edit-mode
                            'agent-shell-viewport-view-mode)
      (visual-wrap-prefix-mode (if visual-line-mode 1 -1))))
  :hook (visual-line-mode . my/toggle-visual-wrap-prefix-mode))

(use-package olivetti
  :ensure t
  :hook (text-mode . olivetti-mode))

(use-package visual-fill-column
  :ensure t
  :commands visual-fill-column-mode)

(use-package markdown-mode
  :ensure t
  :preface
  ;; Tab characters are risky in markdown source: some renderers treat a
  ;; leading tab in a list continuation inconsistently, unlike a fixed
  ;; number of spaces. Force spaces the same way as the other markup/data
  ;; formats (css-mode, json-ts-mode, mhtml-mode).
  ;;
  ;; No tab-width override needed here: markdown-mode's own
  ;; define-derived-mode body already does (setq tab-width 4) ("Natural
  ;; Markdown tab width"), and its indent-cycling logic still consults
  ;; tab-width even with indent-tabs-mode nil, same as yaml-ts-mode's
  ;; indent-relative fallback.
  (defun my/markdown-set-indent-tabs-mode ()
    (setq indent-tabs-mode nil))
  (defun my/markdown-time-stamp-updated-field ()
    (setq-local time-stamp-pattern "20/^updated: %Y-%m-%d %H:%M$")
    (add-hook 'before-save-hook #'time-stamp nil t))
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode))
  :hook
  ((markdown-mode . my/markdown-set-indent-tabs-mode)
   (markdown-mode . my/markdown-time-stamp-updated-field)
   (markdown-mode . variable-pitch-mode)
   (markdown-mode . my/variable-pitch-serif))
  :custom
  (markdown-command "pandoc")
  (markdown-asymmetric-header t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-italic-underscore t)
  :config
  (define-auto-insert
    "/.*/Dropbox/reference/.*\\.md\\'"
    '(nil "---" n
          "title: " (capitalize (file-name-base buffer-file-name)) n
          "id: " (format "\"%s\"" (format-time-string "%Y%m%d%H%M")) n
          "created: " (format-time-string "%Y-%m-%d %H:%M") n
          "---\n\n")))

(use-package flymake-markdownlint
  :ensure t
  :hook
  (markdown-mode . flymake-markdownlint-setup))

(use-package ispell
  :custom
  (ispell-dictionary "australian-w_accents"))

(use-package flymake-vale
  :vc (:url "https://github.com/tpeacock19/flymake-vale" :rev :newest)
  :hook (text-mode . flymake-vale-load))

;;;; Org

(use-package org
  :hook (org-mode . variable-pitch-mode)
  :custom
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis "…")
  :config
  ;; When following links, use find-file.  To follow a link in another window,
  ;; use C-x 4 C-o.
  (setf (alist-get 'file org-link-frame-setup)
        'find-file)

  ;; Don’t steal my keybinding!  `org-keys' binds this to
  ;; `org-cycle-agenda-files' unconditionally, whether or not org-agenda is
  ;; ever used.
  (keymap-unset org-mode-map "C-," t))

(use-package org-table
  :defer t
  :config
  (keymap-unset org-mode-map "C-#" t))  ; Don’t take my Embark binding

(use-package org-indent
  :hook (org-mode))

(use-package org-modern
  :ensure t
  :after org
  :config (global-org-modern-mode 1))

(use-package org-anki
  :ensure t
  :commands (org-anki-sync-entry)
  :custom
  (org-anki-model-fields '(("Basic" "Front" "Back" "Extra" "Source")
                           ("Cloze" "Text" "Extra" "Source"))))

;;;; Mail and news

(use-package sendmail
  :commands sendmail-send-it
  :custom
  (send-mail-function 'sendmail-send-it)
  (sendmail-program "msmtp")
  (mail-specify-envelope-from t)
  (message-sendmail-envelope-from 'header)
  (mail-envelope-from 'header))

;; The Notmuch Emacs interface must match the installed `notmuch' CLI
;; version, so load it from the distro package's site-lisp rather than a
;; possibly out-of-sync ELPA/MELPA build.
(use-package notmuch
  :load-path "/usr/share/emacs/site-lisp"
  :commands notmuch
  :custom
  (notmuch-show-logo nil)
  (mail-user-agent #'notmuch-mua-new-mail)
  (notmuch-identities '("David Porter <david@daporter.net>"))
  (notmuch-wash-wrap-lines-length 80)
  (notmuch-saved-searches
   '((:name "Unread inbox"       :query "tag:unread and tag:inbox"       :key "u")
     (:name "Unread all"         :query "tag:unread"                     :key "U")
     (:name "Inbox"              :query "tag:inbox"                      :key "i")
     (:name "Drafts"             :query "tag:draft"                      :key "d")
     (:name "Sent"               :query "tag:sent"                       :key "s"   :sort-order newest-first)
     (:name "Flagged"            :query "tag:flagged"                    :key "f")
     (:name "All mail"           :query "*"                              :key "a"   :sort-order newest-first)
     (:name "Deleted"            :query "tag:deleted"                    :key "D")
     (:name "emacs-humanities"   :query "tag:list/emacs-humanities"      :key "leh" :count-query "tag:list/emacs-humanities and tag:unread")
     (:name "emacs-paris"        :query "tag:list/emacs-paris"           :key "lep" :count-query "tag:list/emacs-paris and tag:unread")
     (:name "great-conversation" :query "tag:list/great-conversation"    :key "lg"  :count-query "tag:list/great-conversation and tag:unread")))
  (notmuch-archive-tags '("-inbox"))
  (notmuch-draft-folder "Drafts")
  (notmuch-tagging-keys
   '(("r" notmuch-show-mark-read-tags "Mark read")
     ("f" ("+flagged") "Flag")
     ("a" notmuch-archive-tags "Archive")
     ("d" ("+deleted" "-inbox" "-unread") "Delete")))
  (notmuch-tag-formats '(("starred" tag))) ; Show the tag name, not an icon
  (notmuch-mua-user-agent-function #'notmuch-mua-user-agent-full)

  (notmuch-fcc-dirs
   '(("david@daporter.net" . "gmail/\"[Gmail]/Sent Mail\"")))

  :config
  (add-hook 'notmuch-message-mode-hook #'turn-off-auto-fill)
  (add-hook 'notmuch-mua-send-hook #'notmuch-mua-attachment-check)
  (add-hook 'notmuch-show-mode-hook
            (lambda () (variable-pitch-mode 1))))

(use-package consult-notmuch
  :ensure t
  :after (consult notmuch)
  :bind (("C-c m" . consult-notmuch)
         ("C-c M" . consult-notmuch-tree))
  :config
  ;; Narrow `consult-buffer' to already-open notmuch buffers with "< n".
  (add-to-list 'consult-buffer-sources 'consult-notmuch-buffer-source))

;;;; Documents and notes

(use-package reader
  :vc (:url "https://codeberg.org/MonadicSheep/emacs-reader"
            :make "clean all"))

;; Plain markdown notes (no denote/org-roam): find/search them recursively
;; with consult's own consult-fd/consult-ripgrep, rather than consult-notes'
;; file-dir-sources, which only lists one directory level (no recursion).
(defvar my/note-dirs
  (list (expand-file-name "~/Dropbox/reference")
        (expand-file-name "~/Dropbox/Apps/remotely-save/Zettelkasten"))
  "Directories to search when finding personal notes.")

(defun my/find-note ()
  "Find a note by filename, recursively, across `my/note-dirs'."
  (interactive)
  (consult-fd my/note-dirs))

(defun my/search-notes ()
  "Search note contents, recursively, across `my/note-dirs'."
  (interactive)
  (consult-ripgrep my/note-dirs))

(keymap-global-set "C-c n" #'my/find-note)
(keymap-global-set "C-c N" #'my/search-notes)

;;;; Finance

(use-package ledger-mode
  :ensure t
  :mode "\\.journal\\'"
  :custom
  (ledger-default-date-format ledger-iso-date-format))

(use-package hledger-mode
  :ensure t
  :mode "\\.journal\\'"
  :custom
  (hledger-jfile (getenv "LEDGER_FILE"))
  (hledger-currency-string "$")
  (hledger-comments-column comment-column)
  (hledger-life-expectancy 90)
  (hledger-ratios-income-accounts hledger-top-income-account)
  (hledger-ratios-liquid-asset-accounts "assets:cash")
  (hledger-top-income-account "revenues")
  (hledger-year-of-birth 1971))

(use-package flymake-hledger
  :ensure t
  :after (ledger-mode flymake)
  :hook (ledger-mode . flymake-hledger-enable))

(defvar my/finances-dir (expand-file-name "~/Documents/finances/")
  "Root of the finances project, the only project hledger commands apply to.")

(defun my/hledger-import ()
  "Run \"hledger-flow import\" asynchronously in `my/finances-dir'."
  (interactive)
  (let ((default-directory my/finances-dir))
    (async-shell-command "hledger-flow import")))

(defun my/hledger-report ()
  "Run \"hledger-flow report\" asynchronously in `my/finances-dir'."
  (interactive)
  (let ((default-directory my/finances-dir))
    (async-shell-command "hledger-flow report")))

;;;; AI assistants

(use-package agent-shell
  :ensure t
  :preface
  (defun my/agent-shell-setup ()
    (olivetti-mode -1)
    (whitespace-mode -1)
    (apheleia-mode -1)
    (flymake-mode -1)
    ;; Must come after `olivetti-mode' is disabled above: exiting olivetti
    ;; restores whatever `visual-line-mode' state it recorded on entry,
    ;; which would otherwise clobber the wrapping enabled here.
    (setq-local fill-column 100)
    (visual-line-mode 1)
    (visual-fill-column-mode 1))
  :custom
  (agent-shell-anthropic-default-model-id "Default")
  (agent-shell-preferred-agent-config 'claude-code)
  (agent-shell-show-usage-at-turn-end t)
  :commands (agent-shell agent-shell-anthropic-start-claude-code)
  :bind (:map agent-shell-mode-map
              ("C-o" . agent-shell-help-menu))
  ;; The viewport buffers (`agent-shell-prefer-viewport-interaction') derive
  ;; from `text-mode', so they'd otherwise pick up all of `text-mode's
  ;; hooks: `olivetti-mode', `whitespace-mode', `apheleia-mode', `flymake'.
  ;; `agent-shell-mode' itself doesn't derive from `text-mode' (so those
  ;; disables are no-ops there), but gets the same hook for consistent
  ;; wrapping in the main chat buffer.
  :hook ((agent-shell-viewport-edit-mode . my/agent-shell-setup)
         (agent-shell-viewport-view-mode . my/agent-shell-setup)
         (agent-shell-mode . my/agent-shell-setup)))

;;;; Dispatch and menu systems

(use-package transient
  :preface
  (defun my/eglot-active-p ()
    (and (boundp 'eglot--managed-mode)
         eglot--managed-mode))
  (defun my/in-finances-project-p ()
    (file-in-directory-p default-directory my/finances-dir))
  :config
  (transient-define-prefix my/dispatch-menu ()
    "My dispatch menu of context-sensitive editing commands."
    [["Search"
      ("s l" "Consult line"          consult-line)
      ("s i" "Consult imenu"         consult-imenu)
      ("s o" "Consult outline"       consult-outline)]
     ["Whitespace"
      ("w"   "Cleanup"               whitespace-cleanup)]
     ["Insert"
      ("u"   "Unicode char"          my/read-unicode-char)]
     ["Toggle"
      ("t h" "Highlight line"        hl-line-mode)
      ("t n" "Line numbers"          display-line-numbers-mode)
      ("t o" "Olivetti"              olivetti-mode)
      ("t f" "Flymake"               flymake-mode)
      ("t r" "Serif variable-pitch"  my/variable-pitch-toggle-serif)
      ("t w" "Whitespace chars"      my/whitespace-toggle-chars)]]
    [["Flymake" :if-non-nil flymake-mode
      ("f f" "Consult Flymake"       consult-flymake)
      ("f h" "Next error"            flymake-goto-next-error)
      ("f a" "Previous error"        flymake-goto-prev-error)
      ("f d" "Diagnostics"           flymake-show-buffer-diagnostics)
      ("f p" "Diagnostics (project)" flymake-show-project-diagnostics)]
     ["Comment" :if-non-nil comment-start
      ("c c" "Comment DWIM"          comment-dwim)
      ("c l" "Comment line"          comment-line)]
     ["Spellcheck" :if-derived (text-mode prog-mode conf-mode)
      ("p p" "Region"                ispell-region)
      ("p s" "Word"                  ispell-word)
      ("p b" "Buffer"                ispell-buffer               :if-derived text-mode)
      ("p c" "Comments & strings"    ispell-comments-and-strings :if-derived (prog-mode conf-mode))
      ("p m" "Message"               ispell-message              :if-derived message-mode)]
     ["Eglot" :if my/eglot-active-p
      ("e e" "Format region"         eglot-format)
      ("e o" "Organise imports"      eglot-code-action-organize-imports)
      ("e r" "Rename symbol"         eglot-rename)
      ("e q" "Quickfix actions"      eglot-code-action-quickfix)
      ("e a" "Code actionsⁿ"         eglot-code-actions)
      ("e d" "Symbol documentation"  eldoc)
      ("e s" "Restart server"        eglot-reconnect)]
     ["Hledger" :if my/in-finances-project-p
      ("h i" "Import"                my/hledger-import)
      ("h r" "Report"                my/hledger-report)]])

  ;; `org' unsets `C-,' in `org-mode-map' to keep this chord clear.
  (keymap-set (current-global-map) "C-," #'my/dispatch-menu))

(use-package casual-suite
  :ensure t
  :custom
  (casual-lib-use-unicode t)
  :bind
  (:map bookmark-bmenu-mode-map ("C-o" . casual-bookmarks-tmenu)
        :map calc-mode-map ("C-o" . casual-calc-tmenu)
        :map calc-alg-map ("C-o" . casual-calc-tmenu)
        :map compilation-mode-map ("C-o" . casual-compile-tmenu)
        :map dired-mode-map ("C-o" . casual-dired-tmenu)
        :map eshell-mode-map ("C-o" . casual-eshell-tmenu)
        :map grep-mode-map ("C-o" . casual-compile-tmenu)
        :map help-mode-map ("C-o" . casual-help-tmenu)
        :map ibuffer-mode-map ("C-o" . casual-ibuffer-tmenu)
        :map Info-mode-map ("C-o" . casual-info-tmenu)
        :map makefile-mode-map ("C-o" . casual-make-tmenu)
        :map Man-mode-map ("C-o" . casual-man-tmenu)
        :map isearch-mode-map ("C-o" . casual-isearch-tmenu)
        :map reb-lisp-mode-map ("C-o" . casual-re-builder-tmenu)
        :map reb-mode-map ("C-o" . casual-re-builder-tmenu)
        :map csv-mode-map ("C-o" . casual-csv-tmenu)))

;;;; Misc utilities

(use-package apropos
  :custom
  (apropos-sort-by-scores t))

(use-package bookmark
  :custom
  (bookmark-save-flag 1)) ;; Save bookmarks on each modification

(use-package man
  :custom
  (Man-notify-method 'aggressive))

(use-package saveplace
  :init (save-place-mode 1)
  :config
  ;; Always land on the first file entry in Dired instead of restoring
  ;; wherever point was last left in that directory.
  (remove-hook 'dired-initial-position-hook #'save-place-dired-hook))

(use-package savehist
  :hook (after-init))

(use-package shannon-max
  :vc (:url "https://github.com/sstraust/shannonmax" :rev :newest)
  :ensure t
  :commands shannon-max-start-logger
  :init
  (setq shannon-max-keylog-file-name
        (expand-file-name
         (concat user-emacs-directory "emacs-logged-keys")))
  :hook
  (after-init . shannon-max-start-logger))

;; `package-autoremove' deletes every installed package neither named here
;; nor a dependency of one that is; since `use-package' :ensure only
;; installs when a package is missing, this list drifts from the file over
;; time and autoremove starts offering to delete things still in use.
;; Declare it by hand instead -- via Custom, not `setq' (which
;; `enable-theme' would silently overwrite on every theme load below) --
;; and after `custom-file' is loaded, which also sets this variable.
;;
;; eglot, flymake, project and tramp are ELPA upgrades of built-ins (see
;; `package-install-upgrade-built-in' above) that autoremove would
;; otherwise delete; auto-compile is `require'd directly by early-init.el
;; rather than via `use-package', so it needs the same manual entry to
;; avoid looking unused.
(custom-set-variables
 '(package-selected-packages
   '(agent-shell apheleia auto-compile beframe cape captain
                 casual-suite consult consult-notmuch corfu
                 corfu-prescient csv-mode
                 dape doric-themes eglot embark
                 embark-consult flymake flymake-hledger
                 flymake-lua flymake-markdownlint flymake-vale
                 flymake-yamllint fontaine
                 ghostel hl-todo hledger-mode hyprlang-ts-mode ledger-mode
                 ligature lorem-ipsum lua-mode magit marginalia
                 markdown-mode nerd-icons
                 nerd-icons-completion nerd-icons-corfu nerd-icons-dired
                 nerd-icons-grep nerd-icons-ibuffer nerd-icons-xref
                 olivetti orderless org-anki org-modern
                 page-break-lines pcmpl-args popper prescient
                 project python-mode reader shannon-max string-inflection
                 titlecase tramp unfill vertico vertico-prescient
                 visual-fill-column vterm)))

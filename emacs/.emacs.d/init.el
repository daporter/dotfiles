;;; init.el --- Personal configuration file

;;; Commentary:

;;; Code:

;;;; Basic Settings

(setq frame-title-format '("%b"))
(setq ring-bell-function 'ignore)
(setq use-short-answers t)

(dolist (cmd '(narrow-to-region
               narrow-to-page
               upcase-region
               downcase-region
               overwrite-mode))
  (put cmd 'disabled nil))

(setq initial-buffer-choice t)          ; always start with *scratch*

;;;; Packages

(dolist (path '("prot-lisp" "prot-emacs-modules"))
  (add-to-list 'load-path (locate-user-emacs-file path)))

(require 'package)

(add-hook 'package-menu-mode-hook #'hl-line-mode)

(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
      '(("elpa" . 2)
        ("nongnu" . 1)))

(defmacro prot-emacs-builtin-package (package &rest body)
  "Set up builtin PACKAGE with rest BODY.
PACKAGE is a quoted symbol, while BODY consists of balanced
expressions."
  (declare (indent 1))
  `(progn
     (unless (require ,package nil 'noerror)
       (display-warning 'prot-emacs
                        (format "Loading `%s' failed" ,package)
                        :warning))
     ,@body))

(defmacro prot-emacs-elpa-package (package &rest body)
  "Set up PACKAGE from an Elisp archive with rest BODY.
PACKAGE is a quoted symbol, while BODY consists of balanced
expressions.

Try to install the package if it is missing."
  (declare (indent 1))
  `(progn
     (when (not (package-installed-p ,package))
       (unless package-archive-contents
         (package-refresh-contents))
       (package-install ,package))
     (if (require ,package nil 'noerror)
         (progn ,@body)
       (display-warning 'prot-emacs
                        (format "Loading `%s' failed" ,package)
                        :warning))))

(defvar prot-emacs-package-form-regexp
  "^(\\(prot-emacs-.*-package\\|require\\) +'\\([0-9a-zA-Z-]+\\)"
  "Regexp to add packages to `lisp-imenu-generic-expression'.")

(eval-after-load 'lisp-mode
  `(add-to-list 'lisp-imenu-generic-expression
                (list "Packages" ,prot-emacs-package-form-regexp 2)))

(define-key global-map (kbd "C-c P") #'package-list-packages)

;;;; Essentials

;;;;; Read Environment Variables

(prot-emacs-elpa-package 'exec-path-from-shell
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "SSH_AUTH_SOCK"))
  (exec-path-from-shell-initialize))

;;;;; Common Auxiliary Functions (prot-common.el)

(prot-emacs-builtin-package 'prot-common)

;;;;; Common Custom Functions (prot-simple.el)

(prot-emacs-builtin-package 'prot-simple
  (setq prot-simple-insert-pair-alist
        '(("' Single quote"        . (39 39))     ; ' '
          ("\" Double quotes"      . (34 34))     ; " "
          ("` Elisp quote"         . (96 39))     ; ` '
          ("‘ Single apostrophe"   . (8216 8217)) ; ‘ ’
          ("“ Double apostrophes"  . (8220 8221)) ; “ ”
          ("( Parentheses"         . (40 41))     ; ( )
          ("{ Curly brackets"      . (123 125))   ; { }
          ("[ Square brackets"     . (91 93))     ; [ ]
          ("< Angled brackets"     . (60 62))     ; < >
          ("« Εισαγωγικά Gr quote" . (171 187))   ; « »
          ("= Equals signs"        . (61 61))     ; = =
          ("~ Tilde"               . (126 126))   ; ~ ~
          ("* Asterisks"           . (42 42))     ; * *
          ("/ Forward Slash"       . (47 47))     ; / /
          ("_ underscores"         . (95 95))))   ; _ _
  (setq prot-simple-date-specifier "%F")
  (setq prot-simple-time-specifier "%R %z")
  (setq delete-pair-blink-delay 0.15) ; Emacs28 -- see `prot-simple-delete-pair-dwim'
  (setq prot-simple-scratch-buffer-default-mode 'markdown-mode)
  (setq help-window-select t)

  ;; ;; DEPRECATED 2021-10-15: set `help-window-select' to non-nil.
  ;; (setq prot-simple-focusable-help-commands
  ;;       '( describe-symbol
  ;;          describe-function
  ;;          describe-mode
  ;;          describe-variable
  ;;          describe-key
  ;;          describe-char
  ;;          what-cursor-position
  ;;          describe-package
  ;;          view-lossage))
  ;; (prot-simple-focus-help-buffers 1)

  ;; ;; NOTE 2022-01-20: The idea is good, but the implementation needs
  ;; ;; to be refined.
  ;; (prot-simple-rename-help-buffers 1)

  ;; General commands
  (let ((map global-map))
    (define-key map (kbd "<insert>") nil)
    (define-key map (kbd "C-z") nil)
    (define-key map (kbd "C-x C-z") nil)
    (define-key map (kbd "C-h h") nil)
    (define-key map (kbd "M-`") nil)
    (define-key map (kbd "C-h .") #'prot-simple-describe-symbol) ; overrides `display-local-help'
    (define-key map (kbd "C-h K") #'describe-keymap) ; overrides `Info-goto-emacs-key-command-node'
    (define-key map (kbd "C-h c") #'describe-char) ; overrides `describe-key-briefly'
    (define-key map (kbd "C-c s") #'prot-simple-scratch-buffer)
    ;; Commands for lines
    (define-key map (kbd "C-S-w") #'prot-simple-copy-line-or-region)
    (define-key map (kbd "C-S-y") #'prot-simple-yank-replace-line-or-region)
    (define-key map (kbd "M-SPC") #'cycle-spacing)
    (define-key map (kbd "M-o") #'delete-blank-lines)   ; alias for C-x C-o
    (define-key map (kbd "M-k") #'prot-simple-kill-line-backward)
    (define-key map (kbd "C-S-n") #'prot-simple-multi-line-next)
    (define-key map (kbd "C-S-p") #'prot-simple-multi-line-prev)
    (define-key map (kbd "<C-return>") #'prot-simple-new-line-below)
    (define-key map (kbd "<C-S-return>") #'prot-simple-new-line-above)
    ;; Commands for text insertion or manipulation
    (define-key map (kbd "C-=") #'prot-simple-insert-date)
    (define-key map (kbd "C-<") #'prot-simple-escape-url)
    (define-key map (kbd "C-'") #'prot-simple-insert-pair)
    (define-key map (kbd "M-'") #'prot-simple-insert-pair)
    (define-key map (kbd "M-\\") #'prot-simple-delete-pair-dwim)
    (define-key map (kbd "M-z") #'zap-up-to-char) ; NOT `zap-to-char'
    (define-key map (kbd "M-Z") #'prot-simple-zap-to-char-backward)
    ;; NOTE 2022-05-01: I deprecated those commands.  I don't use them
    ;; and they need to be reworked.
    ;;
    ;; (define-key map (kbd "C-M-;") #'prot-simple-cite-region)
    ;; (define-key map (kbd "C-M-^") #'prot-simple-insert-undercaret)
    (define-key map (kbd "<C-M-backspace>") #'backward-kill-sexp)
    (define-key map (kbd "M-c") #'capitalize-dwim)
    (define-key map (kbd "M-l") #'downcase-dwim)        ; "lower" case
    (define-key map (kbd "M-u") #'upcase-dwim)
    ;; Commands for object transposition
    (define-key map (kbd "C-t") #'prot-simple-transpose-chars)
    (define-key map (kbd "C-x C-t") #'prot-simple-transpose-lines)
    (define-key map (kbd "C-S-t") #'prot-simple-transpose-paragraphs)
    (define-key map (kbd "C-x M-t") #'prot-simple-transpose-sentences)
    (define-key map (kbd "C-M-t") #'prot-simple-transpose-sexps)
    (define-key map (kbd "M-t") #'prot-simple-transpose-words)
    ;; Commands for marking objects
    (define-key map (kbd "M-@") #'prot-simple-mark-word)       ; replaces `mark-word'
    (define-key map (kbd "C-M-SPC") #'prot-simple-mark-construct-dwim)
    (define-key map (kbd "C-M-d") #'prot-simple-downward-list)
    ;; Commands for paragraphs
    (define-key map (kbd "M-Q") #'prot-simple-unfill-region-or-paragraph)
    ;; Commands for windows and pages
    (define-key map (kbd "C-x n k") #'prot-simple-delete-page-delimiters)
    (define-key map (kbd "C-x M") #'prot-simple-monocle)
    ;; NOTE 2022-03-02: Elsewhere I provide my `logos.el' package which
    ;; has the functionality of these three commands.
    ;;
    ;; (define-key map [remap narrow-to-region] #'prot-simple-narrow-dwim)
    ;; (define-key map [remap forward-page] #'prot-simple-forward-page-dwim)
    ;; (define-key map [remap backward-page] #'prot-simple-backward-page-dwim)
    ;;
    ;; Commands for buffers
    (define-key map (kbd "M-=") #'count-words)
    (define-key map (kbd "<C-f2>") #'prot-simple-rename-file-and-buffer)
    (define-key map (kbd "C-x K") #'prot-simple-kill-buffer-current)
    (define-key map (kbd "M-s b") #'prot-simple-buffers-major-mode)
    (define-key map (kbd "M-s v") #'prot-simple-buffers-vc-root)))

;;;; Theme

;;;;; Modus Themes

(prot-emacs-elpa-package 'modus-themes

  ;; Note: All customisation options must be evaluated before loading a
  ;; theme.
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-slanted-constructs t)
  (setq modus-themes-syntax '(green-strings))
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-links '(neutral-underline no-color))
  (setq modus-themes-mail-citations 'faint)
  (setq modus-themes-lang-checkers '(background))
  (setq modus-themes-hl-line '(accented))
  (setq modus-themes-region '(bg-only))
  (setq modus-themes-diffs 'desaturated)
  (setq modus-themes-org-blocks 'gray-background)
  (setq modus-themes-variable-pitch-headings t)

  ;; The following values come from the Typographic Scale Calculator,
  ;; <https://www.layoutgridcalculator.com/typographic-scale/>. It uses
  ;; the golden ratio and pentatonic scale.
  (setq modus-themes-headings
        '((1 . (monochrome 1.2917))
          (2 . (monochrome 1.2083))
          (3 . (monochrome 1.1458))
          (4 . (monochrome 1.0625))
          (t . (monochrome))))

  (load-theme 'modus-operandi))

;;;;; Pulsar

(prot-emacs-elpa-package 'pulsar
  (dolist (cmd '(narrow-to-page
                 narrow-to-defun
                 narrow-to-region
                 widen
                 logos-forward-page-dwim
                 logos-backward-page-dwim))
    (add-to-list 'pulsar-pulse-functions cmd))

  (setq pulsar-pulse t)
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-red)
  (setq pulsar-highlight-face 'pulsar-cyan)

  (pulsar-global-mode 1)

  ;; There are convenience functions/commands which pulse the line using
  ;; a specific colour: `pulsar-pulse-line-red' is one of them.
  (add-hook 'next-error-hook #'pulsar-pulse-line-red)

  ;; pulsar does not define any key bindings.  This is just my personal
  ;; preference.  Remember to read the manual on the matter.  Evaluate:
  ;;
  ;; (info "(elisp) Key Binding Conventions")
  (let ((map global-map))
    (define-key map (kbd "C-x l") #'pulsar-pulse-line) ; override `count-lines-page'
    (define-key map (kbd "C-x L") #'pulsar-highlight-dwim)))

;;;;; Lin

(prot-emacs-elpa-package 'lin
  ;; You can use this to live update the face:
  ;;
  ;; (customize-set-variable 'lin-face 'lin-green)
  (setq lin-face 'hl-line)
  (setq lin-mode-hooks
        '(bongo-mode-hook
          dired-mode-hook
          elfeed-search-mode-hook
          git-rebase-mode-hook
          ibuffer-mode-hook
          ilist-mode-hook
          ledger-report-mode-hook
          log-view-mode-hook
          magit-log-mode-hook
          mu4e-headers-mode
          notmuch-search-mode-hook
          notmuch-tree-mode-hook
          occur-mode-hook
          org-agenda-mode-hook
          tabulated-list-mode-hook))
  (lin-global-mode 1))

;;;;; Line Numbers and Relevant Indicators

(prot-emacs-builtin-package 'prot-sideline
  (require 'display-line-numbers)
  ;; Set absolute line numbers.  A value of "relative" is also useful.
  (setq display-line-numbers-type t)
  ;; Those two variables were introduced in Emacs 27.1
  (setq display-line-numbers-major-tick 0)
  (setq display-line-numbers-minor-tick 0)
  ;; Use absolute numbers in narrowed buffers
  (setq-default display-line-numbers-widen t)

  (prot-emacs-elpa-package 'diff-hl
    (setq diff-hl-draw-borders nil)
    (setq diff-hl-side 'left))

  (require 'hl-line)
  (setq hl-line-sticky-flag nil)
  (setq hl-line-overlay-priority -50) ; emacs28

  (require 'whitespace)

  (let ((map global-map))
    (define-key map (kbd "<f6>") #'prot-sideline-negative-space-toggle)
    (define-key map (kbd "<f7>") #'prot-sideline-mode)))

;;;;; Fringe Mode

(prot-emacs-builtin-package 'fringe
  (fringe-mode nil)
  (setq-default fringes-outside-margins nil)
  (setq-default indicate-buffer-boundaries nil)
  (setq-default indicate-empty-lines nil)
  (setq-default overflow-newline-into-fringe t))

;;;;; Cursor Appearance

(prot-emacs-elpa-package 'cursory
  (setq cursory-presets
        '((bar
           :cursor-type (bar . 2)
           :cursor-in-non-selected-windows hollow
           :blink-cursor-blinks 10
           :blink-cursor-interval 0.5
           :blink-cursor-delay 0.2)
          (box
           :cursor-type box
           :cursor-in-non-selected-windows hollow
           :blink-cursor-blinks 10
           :blink-cursor-interval 0.5
           :blink-cursor-delay 0.2)
          (underscore
           :cursor-type (hbar . 3)
           :cursor-in-non-selected-windows hollow
           :blink-cursor-blinks 50
           :blink-cursor-interval 0.2
           :blink-cursor-delay 0.2)))

  (setq cursory-latest-state-file (locate-user-emacs-file "cursory-latest-state.eld"))

  ;; Set last preset or fall back to desired style from `cursory-presets'.
  (cursory-set-preset (or (cursory-restore-latest-preset) 'bar))

  ;; The other side of `cursory-restore-latest-preset'.
  (add-hook 'kill-emacs-hook #'cursory-store-latest-preset)

  ;; We have to use the "point" mnemonic, because C-c c is often the
  ;; suggested binding for `org-capture'.
  (define-key global-map (kbd "C-c C") #'cursory-set-preset))

;;;; Font

;;;;; Fontaine

(prot-emacs-elpa-package 'fontaine
  (setq x-underline-at-descent-line t)
  (setq-default text-scale-remap-header-line t)

  (setq fontaine-latest-state-file
        (locate-user-emacs-file "fontaine-latest-state.eld"))

  (setq fontaine-presets
        '((t
           :default-height 90
           :variable-pitch-family "Libertinus Serif"
           :variable-pitch-height 1.25)))

  ;; Set last preset or fall back to desired style from
  ;; `fontaine-presets'.
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))

  ;; The other side of `fontaine-restore-latest-preset'.
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)

  (define-key global-map (kbd "C-c f") #'fontaine-set-preset)
  (define-key global-map (kbd "C-c F") #'fontaine-set-face-font))

;;;;; Unicode Fonts

(prot-emacs-elpa-package 'unicode-fonts
  (unicode-fonts-setup))

;;;; Mode Line

(setq mode-line-percent-position '(-3 "%p"))
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

;;;;; Mode Line Recursion Indicators

(prot-emacs-elpa-package 'recursion-indicator
  (setq recursion-indicator-general "&")
  (setq recursion-indicator-minibuffer "@")
  (recursion-indicator-mode 1))

;;;;; Display Current Time

(prot-emacs-builtin-package 'time
  (setq display-time-format "%a %e %b, %H:%M")
  ;;;; Covered by `display-time-format'
  ;; (setq display-time-24hr-format t)
  ;; (setq display-time-day-and-date t)
  (setq display-time-interval 60)
  (setq display-time-default-load-average nil)
  ;; NOTE 2021-04-19: For all those, I have implemented a custom
  ;; solution that also shows the number of new items.  Refer to my
  ;; email settings, specifically `prot-mail-mail-indicator'.
  ;;
  ;; NOTE 2021-05-16: Or better check `prot-notmuch-mail-indicator'.
  (setq display-time-mail-directory nil)
  (setq display-time-mail-function nil)
  (setq display-time-use-mail-icon nil)
  (setq display-time-mail-string nil)
  (setq display-time-mail-face nil)

;;;;; World clock

  (setq zoneinfo-style-world-list
        '(("America/Los_Angeles" "Los Angeles")
          ("America/New_York" "New York")
          ("Europe/London" "London")
          ("Europe/Paris" "Paris")
          ("Europe/Amsterdam" "Amsterdam")
          ("Asia/Tokyo" "Tokyo")
          ("Australia/Brisbane" "Brisbane")
          ("Australia/Canberra" "Canberra")))

  ;; All of the following variables are for Emacs 28
  (setq world-clock-list t)
  (setq world-clock-time-format "%R %z  %A %d %B")
  (setq world-clock-buffer-name "*world-clock*") ; Placement handled by `display-buffer-alist'
  (setq world-clock-timer-enable t)
  (setq world-clock-timer-second 60)

  ;; ;; NOTE 2021-10-04: Check `prot-tab-status-line'.
  ;; (add-hook 'after-init-hook #'display-time-mode)
  )

;;;; Completion

;;;;; Orderless Completion Style

(prot-emacs-elpa-package 'orderless
  (setq orderless-component-separator " +")
  ;; NOTE 2022-02-06: I made some major changes and this list may need
  ;; to be refined further.  Remember to check my `completion-styles'
  ;; and the `completion-category-overrides'.
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
  ;; The `?' is a regexp construct.
  (let ((map minibuffer-local-completion-map))
    (define-key map (kbd "SPC") nil)
    (define-key map (kbd "?") nil)))

(prot-emacs-builtin-package 'prot-orderless)

;;;;; Completion Annotations (Marginalia)

(prot-emacs-elpa-package 'marginalia
  (setq marginalia-max-relative-age 0)  ; time is absolute here!
  (marginalia-mode 1))

;;;;; Minibuffer Configurations and Vertico

(prot-emacs-builtin-package 'minibuffer
  (setq completion-styles '(basic orderless)) ; also see `completion-category-overrides'
  (setq completion-category-defaults nil)

  ;; A list of known completion categories:
  ;;
  ;; - `bookmark'
  ;; - `buffer'
  ;; - `charset'
  ;; - `coding-system'
  ;; - `color'
  ;; - `command' (e.g. `M-x')
  ;; - `customize-group'
  ;; - `environment-variable'
  ;; - `expression'
  ;; - `face'
  ;; - `file'
  ;; - `function' (the `describe-function' command bound to `C-h f')
  ;; - `info-menu'
  ;; - `imenu'
  ;; - `input-method'
  ;; - `kill-ring'
  ;; - `library'
  ;; - `minor-mode'
  ;; - `multi-category'
  ;; - `package'
  ;; - `project-file'
  ;; - `symbol' (the `describe-symbol' command bound to `C-h o')
  ;; - `theme'
  ;; - `unicode-name' (the `insert-char' command bound to `C-x 8 RET')
  ;; - `variable' (the `describe-variable' command bound to `C-h v')
  ;;
  ;; From the `consult' package:
  ;;
  ;; - `consult-grep'
  ;; - `consult-isearch'
  ;; - `consult-kmacro'
  ;; - `consult-location'
  ;;
  ;; From the `embark' package:
  ;;
  ;; - `embark-keybinding'
  ;;
  (setq completion-category-overrides
        ;; NOTE 2021-10-25: I am adding `basic' because it works better as a
        ;; default for some contexts.  Read:
        ;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=50387>.
        ;;
        ;; `partial-completion' is a killer app for files, because it
        ;; can expand ~/.l/s/fo to ~/.local/share/fonts.
        ;;
        ;; If `basic' cannot match my current input, Emacs tries the
        ;; next completion style in the given order.  In other words,
        ;; `orderless' kicks in as soon as I input a space or one of its
        ;; style dispatcher characters.
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
  ;; Allow Emacs to resize mini windows, otherwise this does not work:
  ;;   (setq org-use-fast-todo-selection 'expert)
  (setq resize-mini-windows t)
  (setq minibuffer-eldef-shorten-default t)

  (setq read-answer-short t) ; also check `use-short-answers' for Emacs28
  (setq echo-keystrokes 0.25)
  (setq kill-ring-max 60)               ; Keep it small

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
  (minibuffer-electric-default-mode 1))

(prot-emacs-builtin-package 'savehist
  (setq savehist-file (locate-user-emacs-file "savehist"))
  (setq history-length 10000)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (add-hook 'after-init-hook #'savehist-mode))

(prot-emacs-elpa-package 'vertico
  ;; Those are the default values, but check the user option
  ;; `vertico-multiform-categories' for per-category tweaks.
  (setq vertico-scroll-margin 0)
  (setq vertico-count 10)
  (setq vertico-resize nil)
  (setq vertico-cycle t)

  (vertico-mode 1)

  (let ((map vertico-map))
    (define-key map (kbd "M-,") #'vertico-quick-insert)
    (define-key map (kbd "M-.") #'vertico-quick-exit))

  ;; This works with `file-name-shadow-mode'.  When you are in a
  ;; sub-directory and use, say, `find-file' to go to your home '~/' or
  ;; root '/' directory, Vertico will clear the old path to keep only
  ;; your current input.
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

;;;;; Enhanced Minibuffer Commands (consult.el)

(prot-emacs-elpa-package 'consult
  (setq consult-line-numbers-widen t)
  ;; (setq completion-in-region-function #'consult-completion-in-region)
  (setq consult-async-min-input 3)
  (setq consult-async-input-debounce 0.5)
  (setq consult-async-input-throttle 0.8)
  (setq consult-narrow-key ">")
  (setq consult-imenu-config
        '((emacs-lisp-mode :toplevel "Functions"
                           :types ((?f "Functions" font-lock-function-name-face)
                                   (?m "Macros"    font-lock-keyword-face)
                                   (?p "Packages"  font-lock-constant-face)
                                   (?t "Types"     font-lock-type-face)
                                   (?v "Variables" font-lock-variable-name-face)))))
  ;; Search C-h f for more "bookmark jump" handlers.
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

  (require 'consult-imenu) ; the `imenu' extension is in its own file

  (let ((map global-map))
    (define-key map (kbd "C-x r b") #'consult-bookmark) ; override `bookmark-jump'
    (define-key map (kbd "C-x M-:") #'consult-complex-command)
    (define-key map (kbd "C-x M-m") #'consult-minor-mode-menu)
    (define-key map (kbd "C-x M-k") #'consult-kmacro)
    (define-key map [remap goto-line] #'consult-goto-line)
    (define-key map (kbd "M-K") #'consult-keep-lines) ; M-S-k is similar to M-S-5 (M-%)
    (define-key map (kbd "M-F") #'consult-focus-lines) ; same principle
    (define-key map (kbd "M-s M-b") #'consult-buffer)
    (define-key map (kbd "M-s M-f") #'consult-find)
    (define-key map (kbd "M-s M-g") #'consult-grep)
    (define-key map (kbd "M-s M-h") #'consult-history)
    (define-key map (kbd "M-s M-i") #'consult-imenu)
    (define-key map (kbd "M-s M-l") #'consult-line)
    (define-key map (kbd "M-s M-m") #'consult-mark)
    (define-key map (kbd "M-s M-s") #'consult-outline)
    (define-key map (kbd "M-s M-y") #'consult-yank-pop)
    (define-key map (kbd "C-x r r") #'consult-register)) ; Use the register's prefix
  (define-key consult-narrow-map (kbd "?") #'consult-narrow-help)

  ;; see my `pulsar' package, which is declared further above:
  ;; <https://protesilaos.com/emacs/pulsar>
  (setq consult-after-jump-hook nil) ; reset it to avoid conflicts with my function
  (dolist (fn '(pulsar-recenter-top pulsar-reveal-entry))
    (add-hook 'consult-after-jump-hook fn)))

;;;;; Switch to Directories (consult-dir.el)

(prot-emacs-elpa-package 'consult-dir
  (setq consult-dir-shadow-filenames nil)
  (setq consult-dir-sources '( consult-dir--source-bookmark
                               consult-dir--source-default
                               consult-dir--source-project
                               consult-dir--source-recentf))

  ;; Overrides `list-directory' in the `global-map', though I never used
  ;; that anyway.
  (dolist (map (list global-map minibuffer-local-filename-completion-map))
    (define-key map (kbd "C-x C-d") #'consult-dir)))

;;;;; Extended Minibuffer Actions (embark.el)

(prot-emacs-elpa-package 'embark
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; (setq prefix-help-command #'describe-prefix-bindings) ; the default of the above
  (setq embark-quit-after-action t)     ; XXX: Read the doc string!
  (setq embark-cycle-key (kbd "M-."))   ; see the `embark-act' key
  (setq embark-confirm-act-all nil)
  (setq embark-indicators
        '(embark-mixed-indicator
          embark-highlight-indicator))
  ;; NOTE 2021-07-31: The mixed indicator starts out with a minimal view
  ;; and then pops up the verbose buffer, so those variables matter.
  (setq embark-verbose-indicator-excluded-actions
        '("\\`customize-" "\\(local\\|global\\)-set-key"
          set-variable embark-cycle embark-keymap-help embark-isearch))
  (setq embark-verbose-indicator-buffer-sections
        `(target "\n" shadowed-targets " " cycle "\n" bindings))
  (setq embark-mixed-indicator-both nil)
  (setq embark-mixed-indicator-delay 1.2)
  ;;  NOTE 2021-07-28: This is used when `embark-indicator' is set to
  ;;  `embark-mixed-indicator' or `embark-verbose-indicator'.  We can
  ;;  specify the window parameters here, but I prefer to do that in my
  ;;  `display-buffer-alist' (search this document) because it is easier
  ;;  to keep track of all my rules in one place.
  (setq embark-verbose-indicator-display-action nil)

  (let ((map global-map))
    (define-key map (kbd "M-.") #'embark-act)
    ;; The command ‘embark-dwim’ executes the default action at point, while
    ;; `embark-act’ displays a menu of actions.  With my home-row modifiers,
    ;; ‘C-.’ can be seen as a left-click at point and and ‘M-.’ as a
    ;; right-click context menu.  The keybindings are mnemonic, since both
    ;; act at the point (‘.’).
    (define-key map (kbd "C-.") #'embark-dwim) ; like left-click
    (define-key map (kbd "C-h B") #'embark-bindings))
  (define-key embark-collect-mode-map (kbd "M-.") #'embark-act)
  (let ((map minibuffer-local-completion-map))
    (define-key map (kbd "M-.") #'embark-act)
    (define-key map (kbd "C->") #'embark-become))
  (let ((map embark-region-map))
    (define-key map (kbd "a") #'align-regexp)
    (define-key map (kbd "i") #'epa-import-keys-region)
    (define-key map (kbd "r") #'repunctuate-sentences) ; overrides `rot13-region'
    (define-key map (kbd "s") #'sort-lines)
    (define-key map (kbd "u") #'untabify))
  (let ((map embark-symbol-map))
    (define-key map (kbd ".") #'embark-find-definition)
    (define-key map (kbd "k") #'describe-keymap)))

;; Needed for correct exporting while using Embark with Consult
;; commands.
(prot-emacs-elpa-package 'embark-consult)

(prot-emacs-builtin-package 'prot-embark
  (prot-embark-keymaps 1)
  (prot-embark-setup-packages 1))

;;;;; Completion for Recent Files and Directories

(prot-emacs-builtin-package 'recentf
  (setq recentf-save-file (locate-user-emacs-file "recentf"))
  (setq recentf-max-saved-items 200)
  (setq recentf-exclude '(".gz" ".xz" ".zip" "/elpa/" "/ssh:" "/sudo:"))
  (add-hook 'after-init-hook #'recentf-mode))

;;;;; Corfu (In-Buffer Completion Popup)

(prot-emacs-elpa-package 'corfu
  ;; (dolist (mode '( message-mode-hook text-mode-hook prog-mode-hook
  ;;                  shell-mode-hook eshell-mode-hook))
  ;;   (add-hook mode #'corfu-mode))
  (global-corfu-mode 1)
  (define-key corfu-map (kbd "<tab>") #'corfu-complete)

  ;; Adapted from Corfu's manual.
  (defun contrib/corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico is not active.
Useful for prompts such as `eval-expression' and `shell-command'."
    (unless (bound-and-true-p vertico--input)
      (corfu-mode 1)))

  (add-hook 'minibuffer-setup-hook #'contrib/corfu-enable-always-in-minibuffer 1))

;;;;; CAPE

(prot-emacs-elpa-package 'cape
  (setq cape-dabbrev-min-length 3)
  (require 'ispell)
  (setq cape-dict-file ispell-alternate-dictionary)

;;;;;; Git Commit

  (defun my/configure-capfs-git-commit ()
    "Configure my preferred CAPFs for Git Commit mode."
    (setq-local completion-at-point-functions
                '(cape-symbol
                  cape-tag
                  cape-dabbrev
                  cape-ispell)))

  (add-hook 'git-commit-mode-hook #'my/configure-capfs-git-commit)

;;;;;; Eshell

  (defun my/configure-capfs-eshell ()
    "Configure my preferred CAPFs for Eshell mode."
    (setq-local completion-at-point-functions
                '(cape-file
                  pcomplete-completions-at-point))
    ;; The following forms are recommended by the Corfu README.
    (advice-add 'pcomplete-completions-at-point
                :around #'cape-wrap-silent)
    (advice-add 'pcomplete-completions-at-point
                :around #'cape-wrap-purify))

  (add-hook 'eshell-mode-hook #'my/configure-capfs-eshell)

;;;;;; Emacs Lisp

  (defun my/configure-capfs-elisp ()
    "Configure my preferred CAPFs for Elisp mode."
    (setq-local completion-at-point-functions
                (list (cape-super-capf #'elisp-completion-at-point
                                       #'cape-dabbrev)
                      #'cape-file
                      #'cape-ispell)))

  (add-hook 'emacs-lisp-mode-hook #'my/configure-capfs-elisp)

;;;;;; Shell Script

  (defun my/configure-capfs-shell-script ()
    "Configure my preferred CAPFs for Shell Script mode."
    (setq-local completion-at-point-functions
                '(cape-dabbrev
                  cape-file
                  cape-ispell)))

  (add-hook 'sh-mode-hook #'my/configure-capfs-shell-script)

;;;;;; Keybindings for Specific Completion Functions

  (let ((map global-map))
    (define-key map (kbd "C-c p a")  #'cape-abbrev)
    (define-key map (kbd "C-c p d")  #'cape-dabbrev)
    (define-key map (kbd "C-c p w")  #'cape-dict)
    (define-key map (kbd "C-c p f")  #'cape-file)
    (define-key map (kbd "C-c p h")  #'cape-history)
    (define-key map (kbd "C-c p i")  #'cape-ispell)
    (define-key map (kbd "C-c p k")  #'cape-keyword)
    (define-key map (kbd "C-c p l")  #'cape-line)
    (define-key map (kbd "C-c p r")  #'cape-rfc1345)
    (define-key map (kbd "C-c p &")  #'cape-sgml)
    (define-key map (kbd "C-c p s")  #'cape-symbol)
    (define-key map (kbd "C-c p \\") #'cape-tex)
    (define-key map (kbd "C-c p ^")  #'cape-tex)
    (define-key map (kbd "C-c p _")  #'cape-tex)
    (define-key map (kbd "C-c p t")  #'complete-tag)
    (define-key map (kbd "C-c p p")  #'completion-at-point)))

;;;;; Enhance Command-Line Completion (pcmpl-args)

(prot-emacs-elpa-package 'pcmpl-args)

;;;;; Dabbrev (dynamic word completion)

(prot-emacs-builtin-package 'dabbrev
  (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
  (setq dabbrev-abbrev-skip-leading-regexp "[$*/=~']")
  (setq dabbrev-backward-only nil)
  (setq dabbrev-case-distinction 'case-replace)
  (setq dabbrev-case-fold-search nil)
  (setq dabbrev-case-replace 'case-replace)
  (setq dabbrev-check-other-buffers t)
  (setq dabbrev-eliminate-newlines t)
  (setq dabbrev-upcase-means-case-search t)
  (let ((map global-map))
    (define-key map (kbd "M-/") #'dabbrev-expand)
    (define-key map (kbd "C-x M-/") #'dabbrev-completion)))

;;;; Search

;;;;; Isearch, Occur, Grep, and Extras

(prot-emacs-builtin-package 'isearch
  (setq search-highlight t)
  (setq search-whitespace-regexp ".*?")
  (setq isearch-lax-whitespace t)
  (setq isearch-regexp-lax-whitespace nil)
  (setq isearch-lazy-highlight t)
  ;; All of the following variables were introduced in Emacs 27.1.
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format nil)
  (setq lazy-count-suffix-format " (%s/%s)")
  (setq isearch-yank-on-move 'shift)
  (setq isearch-allow-scroll 'unlimited)
  ;; These variables are from Emacs 28
  (setq isearch-repeat-on-direction-change t)
  (setq lazy-highlight-initial-delay 0.5)
  (setq lazy-highlight-no-delay-length 3)
  (setq isearch-wrap-pause t)

  (define-key minibuffer-local-isearch-map (kbd "M-/") #'isearch-complete-edit)
  (let ((map isearch-mode-map))
    (define-key map (kbd "C-g") #'isearch-cancel) ; instead of `isearch-abort'
    (define-key map (kbd "M-/") #'isearch-complete)))

(prot-emacs-builtin-package 'replace
  (setq list-matching-lines-jump-to-current-line nil)
  (add-hook 'occur-mode-hook #'hl-line-mode)
  (add-hook 'occur-mode-hook #'prot-common-truncate-lines-silently) ; from `prot-common.el'
  (define-key occur-mode-map (kbd "t") #'toggle-truncate-lines))

(prot-emacs-builtin-package 'grep)

(prot-emacs-builtin-package 'prot-search
  (setq prot-search-outline-regexp-alist
        '((emacs-lisp-mode . "^\\((\\|;;;+ \\)")
          (org-mode . "^\\(\\*+ +\\|#\\+[Tt][Ii][Tt][Ll][Ee]:\\)")
          (conf-toml-mode . "^\\[")
          (markdown-mode . "^#+ +")))
  (setq prot-search-todo-keywords
        (concat "TODO\\|FIXME\\|NOTE\\|REVIEW\\|XXX\\|KLUDGE"
                "\\|HACK\\|WARN\\|WARNING\\|DEPRECATED\\|BUG"))

  (let ((map global-map))
    (define-key map (kbd "M-s %") #'prot-search-isearch-replace-symbol)
    (define-key map (kbd "M-s M-%") #'prot-search-replace-markup) ; see `prot-search-markup-replacements'
    (define-key map (kbd "M-s M-<") #'prot-search-isearch-beginning-of-buffer)
    (define-key map (kbd "M-s M->") #'prot-search-isearch-end-of-buffer)
    (define-key map (kbd "M-s g") #'prot-search-grep)
    (define-key map (kbd "M-s u") #'prot-search-occur-urls)
    (define-key map (kbd "M-s t") #'prot-search-occur-todo-keywords)
    (define-key map (kbd "M-s M-t") #'prot-search-grep-todo-keywords) ; With C-u it runs `prot-search-git-grep-todo-keywords'
    (define-key map (kbd "M-s M-o") #'prot-search-occur-outline)
    (define-key map (kbd "M-s M-u") #'prot-search-occur-browse-url))
  (let ((map isearch-mode-map))
    (define-key map (kbd "<up>") #'prot-search-isearch-repeat-backward)
    (define-key map (kbd "<down>") #'prot-search-isearch-repeat-forward)
    (define-key map (kbd "<backspace>") #'prot-search-isearch-abort-dwim)
    (define-key map (kbd "<C-return>") #'prot-search-isearch-other-end)))

;;;;; Test regular expressions (re-builder)

(prot-emacs-builtin-package 're-builder
  (setq reb-re-syntax 'read))

;;;;; Wgrep (writable grep)

(prot-emacs-elpa-package 'wgrep
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t)
  (let ((map grep-mode-map))
    (define-key map (kbd "e") #'wgrep-change-to-wgrep-mode)
    (define-key map (kbd "C-x C-q") #'wgrep-change-to-wgrep-mode)
    (define-key map (kbd "C-c C-c") #'wgrep-finish-edit)))

;;;;; Cross-References (xref.el)

(prot-emacs-builtin-package 'xref
  ;; All those have been changed for Emacs 28
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read) ; for M-.
  (setq xref-show-xrefs-function #'xref-show-definitions-buffer) ; for grep and the like
  (setq xref-file-name-display 'project-relative)
  (setq xref-search-program 'grep))

;;;;; Bookmarking

(prot-emacs-builtin-package 'bookmark
  (setq bookmark-default-file "~/Sync/emacs/bookmarks")
  (setq bookmark-use-annotations nil)
  (setq bookmark-automatically-show-annotations t)
  (setq bookmark-set-fringe-mark t) ; Emacs28

  (add-hook 'bookmark-bmenu-mode-hook #'hl-line-mode))

(prot-emacs-builtin-package 'prot-bookmark
  (prot-bookmark-extra-keywords 1))

;; ;;;;; Jump to Visible Position (avy)

(prot-emacs-elpa-package 'avy
  (setq avy-all-windows nil)              ; only the current window
  (setq avy-all-windows-alt t)            ;  all windows with C-u
  (setq avy-keys '(?u ?h ?e ?t ?a ?s ?o ?n))

  (define-key global-map (kbd "C-,") #'avy-goto-char-timer))

;;;; Dired

(prot-emacs-builtin-package 'dired
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-AGFhlv --group-directories-first --time-style=long-iso")
  (setq dired-dwim-target t)
  (setq dired-auto-revert-buffer #'dired-directory-changed-p) ; also see `dired-do-revert-buffer'
  (setq dired-make-directory-clickable t) ; Emacs 29.1
  (setq dired-free-space nil) ; Emacs 29.1
  (setq dired-mouse-drag-files t) ; Emacs 29.1

  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'hl-line-mode)

  ;; In Emacs 29 there is a binding for `repeat-mode' which let you
  ;; repeat C-x C-j just by following it up with j.  For me, this is a
  ;; problem as j calls `dired-goto-file', which I often use.
                                        ;(define-key dired-jump-map (kbd "j") nil)
  )

(prot-emacs-builtin-package 'dired-aux
  (setq dired-isearch-filenames 'dwim)
  ;; The following variables were introduced in Emacs 27.1
  (setq dired-create-destination-dirs 'ask)
  (setq dired-vc-rename-file t)
  ;; And this is for Emacs 28
  (setq dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir))))

  (let ((map dired-mode-map))
    (define-key map (kbd "C-+") #'dired-create-empty-file)
    (define-key map (kbd "M-s f") #'nil)
    (define-key map (kbd "C-x v v") #'dired-vc-next-action))) ; Emacs 28

;; ;; NOTE 2021-05-10: I do not use `find-dired' and related commands
;; ;; because there are other tools that offer a better interface, such
;; ;; as `consult-find', `consult-grep', `project-find-file',
;; ;; `project-find-regexp', `prot-vc-git-grep'.
;; (prot-emacs-builtin-package 'find-dired
;;   (setq find-ls-option
;;         '("-ls" . "-AGFhlv --group-directories-first --time-style=long-iso"))
;;   (setq find-name-arg "-iname"))

(prot-emacs-builtin-package 'dired-x
  (setq dired-clean-up-buffers-too t)
  (setq dired-clean-confirm-killing-deleted-buffers t)
  (setq dired-x-hands-off-my-keys t)    ; easier to show the keys I use
  (setq dired-bind-man nil)
  (setq dired-bind-info nil)
  (define-key dired-mode-map (kbd "I") #'dired-info))

(prot-emacs-builtin-package 'prot-dired
  (setq prot-dired-image-viewers '("feh" "sxiv"))
  (setq prot-dired-media-players '("mpv" "vlc"))
  (setq prot-dired-media-extensions
        "\\.\\(mp[34]\\|ogg\\|flac\\|webm\\|mkv\\)")
  (setq prot-dired-image-extensions
        "\\.\\(png\\|jpe?g\\|tiff\\)")
  (setq dired-guess-shell-alist-user ; those are the defaults for ! and & in Dired
        `((,prot-dired-image-extensions (prot-dired-image-viewer))
          (,prot-dired-media-extensions (prot-dired-media-player))))

  (add-hook 'dired-mode-hook #'prot-dired-setup-imenu)

  (let ((map dired-mode-map))
    (define-key map (kbd "i") #'prot-dired-insert-subdir) ; override `dired-maybe-insert-subdir'
    (define-key map (kbd "/") #'prot-dired-limit-regexp)
    (define-key map (kbd "C-c C-l") #'prot-dired-limit-regexp)
    (define-key map (kbd "M-n") #'prot-dired-subdirectory-next)
    (define-key map (kbd "C-c C-n") #'prot-dired-subdirectory-next)
    (define-key map (kbd "M-p") #'prot-dired-subdirectory-previous)
    (define-key map (kbd "C-c C-p") #'prot-dired-subdirectory-previous)
    (define-key map (kbd "M-s G") #'prot-dired-grep-marked-files)))

(prot-emacs-elpa-package 'dired-subtree
  (setq dired-subtree-use-backgrounds nil)
  (let ((map dired-mode-map))
    (define-key map (kbd "<tab>") #'dired-subtree-toggle)
    (define-key map (kbd "<backtab>") #'dired-subtree-remove))) ; S-TAB

(prot-emacs-builtin-package 'wdired
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))

(prot-emacs-builtin-package 'image-dired
  (setq image-dired-external-viewer "xdg-open")
  (setq image-dired-thumb-size 80)
  (setq image-dired-thumb-margin 2)
  (setq image-dired-thumb-relief 0)
  (setq image-dired-thumbs-per-row 4)
  (define-key image-dired-thumbnail-mode-map
    (kbd "<return>") #'image-dired-thumbnail-display-external))

;;;;;; Ibuffer

(prot-emacs-builtin-package 'ibuffer
  (setq ibuffer-expert t)
  (setq ibuffer-display-summary nil)
  (setq ibuffer-use-other-window nil)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-movement-cycle nil)
  (setq ibuffer-default-sorting-mode 'filename/process)
  (setq ibuffer-use-header-line t)
  (setq ibuffer-default-shrink-to-minimum-size nil)
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
  (setq ibuffer-saved-filter-groups nil)
  (setq ibuffer-old-time 48)
  (add-hook 'ibuffer-mode-hook #'hl-line-mode)
  (define-key global-map (kbd "C-x C-b") #'ibuffer)
  (let ((map ibuffer-mode-map))
    (define-key map (kbd "* f") #'ibuffer-mark-by-file-name-regexp)
    (define-key map (kbd "* g") #'ibuffer-mark-by-content-regexp) ; "g" is for "grep"
    (define-key map (kbd "* n") #'ibuffer-mark-by-name-regexp)
    (define-key map (kbd "s n") #'ibuffer-do-sort-by-alphabetic)  ; "sort name" mnemonic
    (define-key map (kbd "/ g") #'ibuffer-filter-by-content)))

;;;; Windows

;;;;; Unique Names for Buffers

(prot-emacs-builtin-package 'uniquify
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-strip-common-suffix t)
  (setq uniquify-after-kill-buffer-p t))

;;;;;; Window Rules and Basic Tweaks

(prot-emacs-builtin-package 'window
  ;; (setq display-buffer-alist
  ;;       `(;; no window
  ;;         ("\\`\\*Async Shell Command\\*\\'"
  ;;          (display-buffer-no-window))
  ;;         ;; top side window
  ;;         ("\\*world-clock.*"
  ;;          (display-buffer-in-side-window)
  ;;          (window-height . 0.16)
  ;;          (side . top)
  ;;          (slot . -1))
  ;;         ((derived-mode . flymake-diagnostics-buffer-mode)
  ;;          (display-buffer-in-side-window)
  ;;          (window-height . 0.16)
  ;;          (side . top)
  ;;          (slot . 0))
  ;;         ((derived-mode . messages-buffer-mode)
  ;;          (display-buffer-in-side-window)
  ;;          (window-height . 0.16)
  ;;          (side . top)
  ;;          (slot . 1))
  ;;         ((or . ((derived-mode . backtrace-mode)
  ;;                 "\\*\\(Warnings\\|Compile-Log\\)\\*"))
  ;;          (display-buffer-in-side-window)
  ;;          (window-height . 0.16)
  ;;          (side . top)
  ;;          (slot . 2))
  ;;         ;; left side window
  ;;         ((derived-mode . help-mode) ; See the hooks for `visual-line-mode'
  ;;          (display-buffer-reuse-mode-window display-buffer-in-side-window)
  ;;          (window-width . 0.25)
  ;;          (side . left)
  ;;          (slot . 0))
  ;;         ;; right side window
  ;;         ("\\*keycast\\*"
  ;;          (display-buffer-in-side-window)
  ;;          (dedicated . t)
  ;;          (window-width . 0.25)
  ;;          (side . right)
  ;;          (slot . -1)
  ;;          (window-parameters . ((no-other-window . t)
  ;;                                (mode-line-format . none))))
  ;;         ;; bottom side window
  ;;         ("\\*Org Select\\*"
  ;;          (display-buffer-in-side-window)
  ;;          (dedicated . t)
  ;;          (side . bottom)
  ;;          (slot . 0)
  ;;          (window-parameters . ((mode-line-format . none))))
  ;;         ;; bottom buffer (NOT side window)
  ;;         ("\\*Embark Actions\\*"
  ;;          (display-buffer-reuse-mode-window display-buffer-at-bottom)
  ;;          (window-height . fit-window-to-buffer)
  ;;          (window-parameters . ((no-other-window . t)
  ;;                                (mode-line-format . none))))
  ;;         ("\\*\\(Output\\|Register Preview\\).*"
  ;;          (display-buffer-reuse-mode-window display-buffer-at-bottom))
  ;;         ;; below current window
  ;;         ("\\*\\vc-\\(incoming\\|outgoing\\|git : \\).*"
  ;;          (display-buffer-reuse-mode-window display-buffer-below-selected)
  ;;          ;; NOTE 2021-10-06: we cannot `fit-window-to-buffer' because
  ;;          ;; the height is not known in advance.
  ;;          (window-height . 0.2))
  ;;         ("\\*\\(Calendar\\|Bookmark Annotation\\).*"
  ;;          (display-buffer-reuse-mode-window display-buffer-below-selected)
  ;;          (window-height . fit-window-to-buffer))
  ;;         ;; new frame
  ;;         (prot/display-buffer-shell-or-term-p ; see definition below
  ;;          (display-buffer-pop-up-frame)
  ;;          (pop-up-frame-parameters . ((width . (text-pixels . 640))
  ;;                                      (height . (text-pixels . 360))
  ;;                                      (tab-bar-lines . 0)
  ;;                                      ;; ;; Emacs 29 transparency, if you want:
  ;;                                      ;; (alpha-background . 90)
  ;;                                      ))
  ;;          (window-parameters . ((no-other-window . t)
  ;;                                (mode-line-format . none))))
  ;;         ((or . ((derived-mode . Man-mode)
  ;;                 (derived-mode . woman-mode)
  ;;                 "\\*\\(Man\\|woman\\).*"))
  ;;          (display-buffer-reuse-window display-buffer-pop-up-frame)
  ;;          (pop-up-frame-parameters . ((width . (text-pixels . 640))
  ;;                                      (height . (text-pixels . 360)))))))

  (defun prot/display-buffer-shell-or-term-p (buffer &rest _)
    "Check if BUFFER is a shell or terminal.
This is a predicate function for `buffer-match-p', intended for
use in `display-buffer-alist'."
    (when (string-match-p "\\*.*\\(e?shell\\|v?term\\).*" (buffer-name buffer))
      (with-current-buffer buffer
        ;; REVIEW 2022-07-14: Is this robust?
        (and (or (not (derived-mode-p 'message-mode))
                 (not (derived-mode-p 'text-mode)))
             (or (derived-mode-p 'eshell-mode)
                 (derived-mode-p 'shell-mode)
                 (derived-mode-p 'comint-mode)
                 (derived-mode-p 'fundamental-mode))))))

  (setq window-combination-resize t)
  (setq even-window-sizes 'height-only)
  (setq window-sides-vertical nil)
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
    (define-key map "<" #'shrink-window-horizontally)))

;;;;; Window History (winner-mode)

(prot-emacs-builtin-package 'winner
  (add-hook 'after-init-hook #'winner-mode)

  ;; ;; NOTE 2021-07-31: Those are superseded by the commands
  ;; ;; `prot-tab-winner-undo' and `prot-tab-winner-redo' in prot-tab.el
  ;; ;; (search this document).
  ;; (let ((map global-map))
  ;;   (define-key map (kbd "C-x <right>") #'winner-redo)
  ;;   (define-key map (kbd "C-x <left>") #'winner-undo))
  )

;;;;; Directional Window Motions (windmove)

(prot-emacs-builtin-package 'windmove
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
    (define-key map (kbd "C-M-S-<left>") #'windmove-swap-states-left)))

;;;;;; Tabs for Window Layouts

(prot-emacs-builtin-package 'tab-bar
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-close-last-tab-choice nil)
  (setq tab-bar-close-tab-select 'recent)
  (setq tab-bar-new-tab-choice t)
  (setq tab-bar-new-tab-to 'right)
  (setq tab-bar-position nil)
  (setq tab-bar-show nil)
  (setq tab-bar-tab-hints nil)
  (setq tab-bar-tab-name-function 'tab-bar-tab-name-current)

  (tab-bar-mode -1)                     ; see `prot-tab-status-line'

  ;; Same concept as `winner-mode'.  See the `prot-tab-winner-undo' and
  ;; its counterpart.
  (tab-bar-history-mode 1))

(prot-emacs-builtin-package 'prot-tab
  (setq prot-tab-tab-select-num-threshold 3)
  (setq tab-bar-format                    ; Emacs 28
        '(tab-bar-format-tabs-groups
          tab-bar-format-align-right
          tab-bar-format-global))

  (add-hook 'after-init-hook #'prot-tab-status-line)

  (let ((map global-map))
    (define-key map (kbd "C-x <right>") #'prot-tab-winner-redo)
    (define-key map (kbd "C-x <left>") #'prot-tab-winner-undo)
    (define-key map (kbd "<f8>") #'prot-tab-status-line) ; unopinionated alternative: `prot-tab-bar-toggle'
    (define-key map (kbd "C-x t t") #'prot-tab-select-tab-dwim)))

;;;; Git

;;;;; Diff Mode

(prot-emacs-builtin-package 'diff-mode
  (setq diff-default-read-only t)
  (setq diff-advance-after-apply-hunk t)
  (setq diff-update-on-the-fly t)
  ;; The following are from Emacs 27.1
  (setq diff-refine nil)                ; I do it on demand
  (setq diff-font-lock-prettify nil)    ; better for patches
  ;; The following is further controlled by
  ;; `prot-diff-modus-themes-diffs'
  (setq diff-font-lock-syntax 'hunk-also))

(prot-emacs-builtin-package 'prot-diff
  (prot-diff-modus-themes-diffs)
  (add-hook 'modus-themes-after-load-theme-hook #'prot-diff-modus-themes-diffs)

  (prot-diff-extra-keywords 1)

  ;; `prot-diff-buffer-dwim' replaces the default for `vc-diff' (which I
  ;; bind to another key---see VC section).
  (define-key global-map (kbd "C-x v =") #'prot-diff-buffer-dwim)
  (let ((map diff-mode-map))
    (define-key map (kbd "C-c C-b") #'prot-diff-refine-cycle) ; replace `diff-refine-hunk'
    (define-key map (kbd "C-c C-n") #'prot-diff-narrow-dwim)))

;;;;; Version Control Framework (vc.el and prot-vc.el)

(prot-emacs-builtin-package 'vc
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

  (setq vc-follow-symlinks t)

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
    (define-key map (kbd "P") #'vc-push)))

(prot-emacs-builtin-package 'prot-vc
  (setq prot-vc-log-limit 100)
  (setq prot-vc-log-bulk-action-limit 50)
  (setq prot-vc-git-log-edit-show-commits t)
  (setq prot-vc-git-log-edit-show-commit-count 10)
  (setq prot-vc-shell-output "*prot-vc-output*")
  (setq prot-vc-patch-output-dirs (list "~/" "~/Desktop/"))
  (add-to-list 'log-edit-headers-alist '("Amend"))

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
    (define-key map (kbd "w") #'prot-vc-log-kill-hash)))

;;;;; Magit

;; There is no need to install the package, as transient.el is built
;; into Emacs.  By requiring it, I prevent the installation of the
;; package, which would be done by Magit.
(prot-emacs-builtin-package 'transient)

(prot-emacs-elpa-package 'magit
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
        '(("~/src" . 1)
          ("~/dotfiles" . 0))))

;;;;; Smerge and Ediff

(prot-emacs-builtin-package 'smerge-mode)

(prot-emacs-builtin-package 'ediff
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
    (flush-lines ".*prot-ediff.*" (point-min) (point-max) nil)))

;;;; Shell

;;;;; Eshell

(prot-emacs-builtin-package 'eshell
  (require 'esh-mode)
  (require 'esh-module)
  (setq eshell-modules-list             ; It works but may need review
        '(eshell-alias
          eshell-basic
          eshell-cmpl
          eshell-dirs
          eshell-glob
          eshell-hist
          eshell-ls
          eshell-pred
          eshell-prompt
          eshell-script
          eshell-term
          eshell-tramp
          eshell-unix))
  (setenv "PAGER" "cat") ; solves issues, such as with 'git log' and the default 'less'
  (require 'em-cmpl)
  (require 'em-dirs)
  (setq eshell-cd-on-directory t)

  (require 'em-tramp)
  (setq password-cache t)
  (setq password-cache-expiry 600)

  (require 'em-hist)
  (setq eshell-hist-ignoredups t)
  (setq eshell-save-history-on-exit t))

(prot-emacs-builtin-package 'prot-eshell
  (setq prot-eshell-output-buffer "*Exported Eshell output*")
  (setq prot-eshell-output-delimiter "* * *")
  (let ((map eshell-mode-map))
    (define-key map (kbd "M-k") #'eshell-kill-input)
    (define-key map (kbd "C-c C-f") #'prot-eshell-ffap-find-file)
    (define-key map (kbd "C-c C-j") #'prot-eshell-ffap-dired-jump)
    (define-key map (kbd "C-c C-w") #'prot-eshell-ffap-kill-save)
    (define-key map (kbd "C-c C->") #'prot-eshell-redirect-to-buffer)
    (define-key map (kbd "C-c C-e") #'prot-eshell-export)
    (define-key map (kbd "C-c C-r") #'prot-eshell-root-dir))
  (let ((map eshell-cmpl-mode-map))
    (define-key map (kbd "C-c TAB") #'prot-eshell-ffap-insert) ; C-c C-i
    (define-key map (kbd "C-c C-h") #'prot-eshell-narrow-output-highlight-regexp))
  (let ((map eshell-hist-mode-map))
    (define-key map (kbd "M-s") #'nil) ; I use this prefix for lots of more useful commands
    (define-key map (kbd "M-r") #'prot-eshell-complete-history)
    (define-key map (kbd "C-c C-d") #'prot-eshell-complete-recent-dir)
    (define-key map (kbd "C-c C-s") #'prot-eshell-find-subdirectory-recursive)))

;;;;; Shell (M-x shell)

(prot-emacs-builtin-package 'shell
  (setq shell-command-prompt-show-cwd t) ; Emacs 27.1
  (setq ansi-color-for-comint-mode t))

;;;;; Sudo

(prot-emacs-builtin-package 'sudo-edit
  (define-key embark-file-map (kbd "S") #'sudo-edit-find-file))

;;;;; Tools For Manual Pages

(prot-emacs-builtin-package 'man
  (let ((map Man-mode-map))
    (define-key map (kbd "i") #'Man-goto-section)
    (define-key map (kbd "g") #'Man-update-manpage)))

;;;;; Proced

(prot-emacs-builtin-package 'proced
  (setq proced-auto-update-flag t)
  (setq proced-auto-update-interval 5)
  (setq proced-descend t)
  (setq proced-filter 'user))

(prot-emacs-builtin-package 'prot-proced
  (prot-proced-extra-keywords 1))

;;;;; Pass Interface (password-store)

(prot-emacs-elpa-package 'password-store
  (setq password-store-time-before-clipboard-restore 30)
  ;; Mnemonic is the root of the "code" word (κώδικας).  But also to add
  ;; the password to the kill-ring.  Other options are already taken.
  (define-key global-map (kbd "C-c k") #'password-store-copy))

(prot-emacs-elpa-package 'pass)

;;;; Write

;;;;; Whitespace

(prot-emacs-builtin-package 'whitespace
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

  (defun my/whitespace-style-ignore-indentation ()
    "Set `whitespace-style’ to ignore ignore indentation."
    (setq-local whitespace-style
                (remove 'indentation whitespace-style)))

  ;; Long lines are allowed in certain modes.
  (dolist (hook '(markdown-mode-hook
                  org-mode-hook))
    (add-hook hook #'my/whitespace-style-allow-long-lines)))

;;;;; Outline Mode

(prot-emacs-builtin-package 'outline
  (setq outline-minor-mode-highlight 'override) ; emacs28
  (setq outline-minor-mode-cycle t)             ; emacs28
  (setq outline-minor-mode-use-buttons nil) ; emacs29---bless you for the nil option!
  (let ((map outline-minor-mode-map))
    ;; ;; NOTE 2021-07-25: Those two are already defined (emacs28).
    ;; (define-key map (kbd "TAB") #'outline-cycle)
    ;; (define-key map (kbd "<backtab>") #'outline-cycle-buffer) ; S-TAB

    ;; I normally don't use those as I usually move around with my
    ;; `logos' package.  Otherwise I use Consult's outline/imenu
    ;; commands or `prot-search-occur-outline'.
    (define-key map (kbd "C-c C-n") #'outline-next-visible-heading)
    (define-key map (kbd "C-c C-p") #'outline-previous-visible-heading)
    (define-key map (kbd "C-c C-f") #'outline-forward-same-level)
    (define-key map (kbd "C-c C-b") #'outline-backward-same-level)
    (define-key map (kbd "C-c C-a") #'outline-show-all)
    (define-key map (kbd "C-c C-o") #'outline-hide-other)
    (define-key map (kbd "C-c C-u") #'outline-up-heading))
  (define-key global-map (kbd "<f10>") #'outline-minor-mode))

;;;;;; Titlecasing

(prot-emacs-elpa-package 'titlecase
  (setq titlecase-style 'mla)

  (with-eval-after-load "embark"
    (progn
      (define-key embark-heading-map (kbd "t") #'titlecase-line)
      (define-key embark-region-map (kbd "t") #'titlecase-region))))

;;;;; Denote

(prot-emacs-elpa-package 'denote
  ;; Remember to check the doc strings of those variables.
  (setq denote-directory (expand-file-name "~/Sync/notes/"))
  (setq denote-known-keywords '("emacs" "philosophy"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)

  ;; We allow multi-word keywords by default.  The author's personal
  ;; preference is for single-word keywords for a more disciplined
  ;; workflow.
  (setq denote-allow-multi-word-keywords nil)

  (setq denote-date-format nil) ; read its doc string

  ;; By default, we fontify backlinks in their bespoke buffer.
  (setq denote-link-fontify-backlinks t)

  ;; Also see `denote-link-backlinks-display-buffer-action' which is a bit
  ;; advanced.

  ;; If you use Markdown or plain text files you want to buttonise
  ;; existing buttons upon visiting the file (Org renders links as
  ;; buttons right away).
  (add-hook 'find-file-hook #'denote-link-buttonize-buffer)

  (setq denote-dired-directories
        (list denote-directory
              "~/Sync/inbox"
              "~/Sync/journal"))

  ;; Generic (great if you rename files Denote-style in lots of places):
  (add-hook 'dired-mode-hook #'denote-dired-mode)
  ;;
  ;; OR if only want it in `denote-dired-directories':
  ;; (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

  (defun my/denote-journal ()
    "Create an entry tagged 'journal', prompting for the title."
    (interactive)
    (let ((denote-directory "~/Sync/journal/"))
      (denote (denote--title-prompt) "journal")))

  ;; Denote DOES NOT define any key bindings.  This is for the user to
  ;; decide.  For example:
  (let ((map global-map))
    (define-key map (kbd "C-c n j") #'prot/denote-journal)
    (define-key map (kbd "C-c n n") #'denote)
    (define-key map (kbd "C-c n N") #'denote-type)
    (define-key map (kbd "C-c n d") #'denote-date)
    ;; If you intend to use Denote with a variety of file types, it is
    ;; easier to bind the link-related commands to the `global-map', as
    ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
    ;; `markdown-mode-map', and/or `text-mode-map'.
    (define-key map (kbd "C-c n i") #'denote-link) ; "insert" mnemonic
    (define-key map (kbd "C-c n I") #'denote-link-add-links)
    (define-key map (kbd "C-c n l") #'denote-link-find-file) ; "list" links
    (define-key map (kbd "C-c n b") #'denote-link-backlinks)
    ;; Note that `denote-rename-file' can work from any context, not
    ;; just Dired buffers.  That is why we bind it here to the
    ;; `global-map'.
    (define-key map (kbd "C-c n r") #'denote-rename-file)
    (define-key map (kbd "C-c n R") #'denote-rename-file-using-front-matter))

  ;; Key bindings specifically for Dired.
  (let ((map dired-mode-map))
    (define-key map (kbd "C-c C-d C-i") #'denote-link-dired-marked-notes)
    (define-key map (kbd "C-c C-d C-r") #'denote-dired-rename-marked-files)
    (define-key map (kbd "C-c C-d C-R") #'denote-dired-rename-marked-files-using-front-matter)))

;;;;; Focus Mode (logos.el)

(prot-emacs-elpa-package 'olivetti
  (setq olivetti-body-width 0.7)
  (setq olivetti-minimum-body-width 80)
  (setq olivetti-recall-visual-line-mode-entry-state t))

(prot-emacs-elpa-package 'logos
  (setq logos-outlines-are-pages t)
  (setq logos-outline-regexp-alist
        `((emacs-lisp-mode . ,(format "\\(^;;;+ \\|%s\\)" logos--page-delimiter))
          (org-mode . ,(format "\\(^\\*+ +\\|^-\\{5\\}$\\|%s\\)" logos--page-delimiter))
          (markdown-mode . ,(format "\\(^\\#+ +\\|^[*-]\\{5\\}$\\|^\\* \\* \\*$\\|%s\\)" logos--page-delimiter))
          (conf-toml-mode . "^\\[")
          (t . ,(or outline-regexp logos--page-delimiter))))

  ;; These apply when `logos-focus-mode' is enabled.  Their value is
  ;; buffer-local.
  (setq-default logos-hide-mode-line t)
  (setq-default logos-hide-buffer-boundaries t)
  (setq-default logos-hide-fringe t)
  (setq-default logos-variable-pitch t) ; see my `fontaine' configurations
  (setq-default logos-buffer-read-only nil)
  (setq-default logos-scroll-lock nil)
  (setq-default logos-olivetti t)

  ;; I don't need to do `with-eval-after-load' for the `modus-themes' as
  ;; I always load them before other relevant potentially packages.
  (add-hook 'modus-themes-after-load-theme-hook #'logos-update-fringe-in-buffers)

  (let ((map global-map))
    (define-key map [remap narrow-to-region] #'logos-narrow-dwim)
    (define-key map [remap forward-page] #'logos-forward-page-dwim)
    (define-key map [remap backward-page] #'logos-backward-page-dwim)
    ;; I don't think I ever saw a package bind M-] or M-[...
    (define-key map (kbd "M-]") #'logos-forward-page-dwim)
    (define-key map (kbd "M-[") #'logos-backward-page-dwim)
    (define-key map (kbd "<f9>") #'logos-focus-mode))

;;;;;; Extra Tweaks

  ;; place point at the top when changing pages, but not in `prog-mode'
  (defun prot/logos--recenter-top ()
    "Use `recenter' to reposition the view at the top."
    (unless (derived-mode-p 'prog-mode)
      (recenter 1))) ; Use 0 for the absolute top

  (add-hook 'logos-page-motion-hook #'prot/logos--recenter-top))

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
  (define-key map (kbd "C-c z c") #'my/insert-zotero-citation)
  (define-key map (kbd "C-c z r") #'my/insert-zotero-reference))

;;;;; Anki Card Creation

(prot-emacs-elpa-package 'anki-editor

  ;; Add some keybindings to anki-editor-mode.
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c a n") #'anki-editor-insert-note)
    (define-key map (kbd "C-c a c") #'anki-editor-cloze-region)
    (define-key map (kbd "C-c a p") #'anki-editor-push-notes)
    (add-to-list 'minor-mode-map-alist (cons 'anki-editor-mode map)))

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
    "    /Communication progressive du français - Débutant, 2^e édition/\n"))

;;;; Org Mode

(prot-emacs-builtin-package 'org
  (setq org-directory (convert-standard-filename "~/Sync/org"))
  (setq org-imenu-depth 7)

;;;;; General Settings

  (setq org-adapt-indentation nil)      ; No, non, nein, όχι!
  (setq org-special-ctrl-a/e nil)
  (setq org-special-ctrl-k nil)
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-hide-emphasis-markers nil)
  (setq org-hide-macro-markers nil)
  (setq org-hide-leading-stars nil)
  (setq org-cycle-separator-lines 0)
  (setq org-structure-template-alist    ; CHANGED in Org 9.3, Emacs 27.1
        '(("s" . "src")
          ("E" . "src emacs-lisp")
          ("e" . "example")
          ("q" . "quote")
          ("v" . "verse")
          ("V" . "verbatim")
          ("c" . "center")
          ("C" . "comment")))
  (setq org-catch-invisible-edits 'show)
  (setq org-return-follows-link nil)
  (setq org-loop-over-headlines-in-active-region 'start-level)
  (setq org-modules '(ol-info ol-eww))
  (setq org-use-sub-superscripts '{})
  (setq org-insert-heading-respect-content t)

;;;;; Refile, Todo

  (setq org-refile-targets
        '((org-agenda-files . (:maxlevel . 2))
          (nil . (:maxlevel . 2))))
  (setq org-refile-use-outline-path t)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-cache t)
  (setq org-reverse-note-order nil)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "MAYBE(m)" "WAIT(w@/!)" "|" "CANCEL(c@)" "DONE(d!)")))
  (setq org-todo-keyword-faces
        '(("WAIT" . '(bold org-todo))
          ("MAYBE" . '(bold shadow))
          ("CANCEL" . '(bold org-done))))
  (setq org-use-fast-todo-selection 'expert)
  (setq org-priority-faces
        '((?A . '(bold org-priority))
          (?B . org-priority)
          (?C . '(shadow org-priority))))
  (setq org-fontify-done-headline nil)
  (setq org-fontify-todo-headline nil)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-fontify-whole-heading-line nil)
  (setq org-fontify-whole-block-delimiter-line t)
  (setq org-highlight-latex-and-related nil) ; other options affect elisp regexp in src blocks
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-track-ordered-property-with-tag t)
  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?C)
  (setq org-default-priority ?A)

;;;;; Tags

  (setq org-auto-align-tags nil)
  (setq org-tags-column 0)

;;;;; Log

  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-log-note-clock-out nil)
  (setq org-log-redeadline 'time)
  (setq org-log-reschedule 'time)
  (setq org-read-date-prefer-future 'time)

;;;;; Links

  (setq org-link-keep-stored-after-insertion nil)
  ;; TODO 2021-10-15 org-link-make-description-function

;;;;; Capture

  (setq org-capture-templates
        `(("n" "Note" entry
           (file "")
           ,(concat "* %U\n"
                    "%i\n"
                    "%?\n"
                    "%l\n")
           :empty-lines 1)))

  (setq org-capture-templates-contexts
        '(("e" ((in-mode . "notmuch-search-mode")
                (in-mode . "notmuch-show-mode")
                (in-mode . "notmuch-tree-mode")))))

;;;;; Agenda

;;;;;; Basic Agenda Setup

  (setq org-default-notes-file (expand-file-name "~/Sync/inbox/notes.org"))
  (setq org-agenda-span 'week)
  (setq org-agenda-start-on-weekday 1)  ; Monday
  (setq org-agenda-confirm-kill t)
  (setq org-agenda-show-all-dates t)
  (setq org-agenda-show-outline-path nil)
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-skip-comment-trees t)
  (setq org-agenda-menu-show-matcher t)
  (setq org-agenda-menu-two-columns nil)
  (setq org-agenda-sticky nil)
  (setq org-agenda-custom-commands-contexts nil)
  (setq org-agenda-max-entries nil)
  (setq org-agenda-max-todos nil)
  (setq org-agenda-max-tags nil)
  (setq org-agenda-max-effort nil)

  ;; NOTE 2021-12-07: In my `prot-org.el' (see further below), I add
  ;; `org-agenda-to-appt' to various relevant hooks.
  ;;
  ;; Create reminders for tasks with a due date when this file is read.
  (run-at-time (* 60 5) nil #'org-agenda-to-appt)

;;;;;; General Agenda View Options

  ;; NOTE 2021-12-07: Check further below my `org-agenda-custom-commands'
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c%?-12t% s")
          (todo . " %i %-12:c")
          (tags . " %i %-12:c")
          (search . " %i %-12:c")))
  (setq org-agenda-sorting-strategy
        '((agenda deadline-up priority-down scheduled-up)
          (todo priority-down category-keep)
          (tags priority-down category-keep)
          (search category-keep)))
  (setq org-agenda-breadcrumbs-separator "->")
  (setq org-agenda-todo-keyword-format "%-1s")
  (setq org-agenda-fontify-priorities 'cookies)
  (setq org-agenda-category-icon-alist nil)
  (setq org-agenda-remove-times-when-in-prefix nil)
  (setq org-agenda-remove-timeranges-from-blocks nil)
  (setq org-agenda-compact-blocks nil)
  (setq org-agenda-block-separator ?—)

;;;;;; Agenda Marks

  (setq org-agenda-bulk-mark-char "#")
  (setq org-agenda-persistent-marks nil)

;;;;;; Agenda Diary Entries

  (setq org-agenda-insert-diary-strategy 'date-tree)
  (setq org-agenda-insert-diary-extract-time nil)
  (setq org-agenda-include-diary nil)

;;;;;; Agenda Follow Mode

  (setq org-agenda-start-with-follow-mode nil)
  (setq org-agenda-follow-indirect t)

;;;;;; Agenda Multi-Item Tasks

  (setq org-agenda-dim-blocked-tasks t)
  (setq org-agenda-todo-list-sublevels t)

;;;;;; Agenda Filters and Restricted Views

  (setq org-agenda-persistent-filter nil)
  (setq org-agenda-restriction-lock-highlight-subtree t)

;;;;;; Agenda Items with Deadline and Scheduled Timestamps

  (setq org-agenda-include-deadlines t)
  (setq org-deadline-warning-days 5)
  (setq org-agenda-skip-scheduled-if-done nil)
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  (setq org-agenda-skip-timestamp-if-deadline-is-shown t)
  (setq org-agenda-skip-deadline-if-done nil)
  (setq org-agenda-skip-deadline-prewarning-if-scheduled 1)
  (setq org-agenda-skip-scheduled-delay-if-deadline nil)
  (setq org-agenda-skip-additional-timestamps-same-entry nil)
  (setq org-agenda-skip-timestamp-if-done nil)
  (setq org-agenda-search-headline-for-time nil)
  (setq org-scheduled-past-days 365)
  (setq org-deadline-past-days 365)
  (setq org-agenda-move-date-from-past-immediately-to-today t)
  (setq org-agenda-show-future-repeats t)
  (setq org-agenda-prefer-last-repeat nil)
  (setq org-agenda-timerange-leaders
        '("" "(%d/%d): "))
  (setq org-agenda-scheduled-leaders
        '("Scheduled: " "Sched.%2dx: "))
  (setq org-agenda-inactive-leader "[")
  (setq org-agenda-deadline-leaders
        '("Deadline:  " "In %3d d.: " "%2d d. ago: "))
  ;; Time grid
  (setq org-agenda-time-leading-zero t)
  (setq org-agenda-timegrid-use-ampm nil)
  (setq org-agenda-use-time-grid t)
  (setq org-agenda-show-current-time-in-grid t)
  (setq org-agenda-current-time-string
        (concat "Now " (make-string 70 ?-)))
  (setq org-agenda-time-grid
        '((daily today require-timed)
          (0600 0700 0800 0900 1000 1100
                1200 1300 1400 1500 1600
                1700 1800 1900 2000 2100)
          " ....." "-----------------"))
  (setq org-agenda-default-appointment-duration nil)

;;;;;; Agenda Global To-do List

  (setq org-agenda-todo-ignore-with-date t)
  (setq org-agenda-todo-ignore-timestamp t)
  (setq org-agenda-todo-ignore-scheduled t)
  (setq org-agenda-todo-ignore-deadlines t)
  (setq org-agenda-todo-ignore-time-comparison-use-seconds t)
  (setq org-agenda-tags-todo-honor-ignore-options nil)

;;;;;; Agenda Tagged Items

  (setq org-agenda-show-inherited-tags t)
  (setq org-agenda-use-tag-inheritance
        '(todo search agenda))
  (setq org-agenda-hide-tags-regexp nil)
  (setq org-agenda-remove-tags nil)
  (setq org-agenda-tags-column -100)

;;;;;; Agenda Entry

  ;; NOTE: I do not use this right now.  Leaving everything to its
  ;; default value.
  (setq org-agenda-start-with-entry-text-mode nil)
  (setq org-agenda-entry-text-maxlines 5)
  (setq org-agenda-entry-text-exclude-regexps nil)
  (setq org-agenda-entry-text-leaders "    > ")

;;;;;; Agenda Logging and Clocking

  ;; NOTE: I do not use these yet, though I plan to.  Leaving everything
  ;; to its default value for the time being.
  (setq org-agenda-log-mode-items '(closed clock))
  (setq org-agenda-clock-consistency-checks
        '((:max-duration "10:00" :min-duration 0 :max-gap "0:05" :gap-ok-around
                         ("4:00")
                         :default-face ; This should definitely be reviewed
                         ((:background "DarkRed")
                          (:foreground "white"))
                         :overlap-face nil :gap-face nil :no-end-time-face nil
                         :long-face nil :short-face nil)))
  (setq org-agenda-log-mode-add-notes t)
  (setq org-agenda-start-with-log-mode nil)
  (setq org-agenda-start-with-clockreport-mode nil)
  (setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 2))
  (setq org-agenda-search-view-always-boolean nil)
  (setq org-agenda-search-view-force-full-words nil)
  (setq org-agenda-search-view-max-outline-level 0)
  (setq org-agenda-search-headline-for-time t)
  (setq org-agenda-use-time-grid t)
  (setq org-agenda-cmp-user-defined nil)
  (setq org-agenda-sort-notime-is-late t) ; Org 9.4
  (setq org-agenda-sort-noeffort-is-high t) ; Org 9.4

;;;;;; Agenda Column View

  ;; NOTE I do not use these, but may need them in the future.
  (setq org-agenda-view-columns-initially nil)
  (setq org-agenda-columns-show-summaries t)
  (setq org-agenda-columns-compute-summary-properties t)
  (setq org-agenda-columns-add-appointments-to-effort-sum nil)
  (setq org-agenda-auto-exclude-function nil)
  (setq org-agenda-bulk-custom-functions nil)

;;;;;; Agenda Habits

  (require 'org-habit)
  (setq org-habit-graph-column 50)
  (setq org-habit-preceding-days 9)

;;;;; Code Blocks

  (setq org-confirm-babel-evaluate nil)
  (setq org-src-window-setup 'current-window)
  (setq org-edit-src-persistent-message nil)
  (setq org-src-fontify-natively t)
  (setq org-src-preserve-indentation t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 0)

;;;;; Export

  (setq org-export-with-toc t)
  (setq org-export-headline-levels 8)
  (setq org-export-dispatch-use-expert-ui nil)
  (setq org-html-htmlize-output-type nil)
  (setq org-html-head-include-default-style nil)
  (setq org-html-head-include-scripts nil)
  (require 'ox-texinfo)
  (require 'ox-md)
  ;; FIXME: how to remove everything else?
  (setq org-export-backends '(html texinfo md))

;;;;; IDs

  (setq org-id-link-to-org-use-id
        'create-if-interactive-and-no-custom-id)

;;;;; Hooks and Key Bindings

  ;; See my `pulsar' package, which is declared further above (otherwise
  ;; I would wrap this in `with-eval-after-load'):
  ;; <https://protesilaos.com/emacs/pulsar>
  (dolist (hook '(org-agenda-after-show-hook org-follow-link-hook))
    (add-hook hook #'pulsar-recenter-middle)
    (add-hook hook #'pulsar-reveal-entry))

  (let ((map global-map))
    (define-key map (kbd "C-c c") #'org-capture)
    (define-key map (kbd "C-c l") #'org-store-link)
    (define-key map (kbd "C-c o") #'org-open-at-point-global))
  (let ((map org-mode-map))
    (define-key map (kbd "C-'") nil)
    (define-key map (kbd "C-,") nil)
    (define-key map (kbd "<C-return>") nil)
    (define-key map (kbd "<C-S-return>") nil)
    (define-key map (kbd "C-M-S-<right>") nil)
    (define-key map (kbd "C-M-S-<left>") nil)
    (define-key map (kbd "C-c M-l") #'org-insert-last-stored-link)
    (define-key map (kbd "C-c C-M-l") #'org-toggle-link-display)))

;;;;; Custom Extensions (prot-org.el)

(prot-emacs-builtin-package 'prot-org
  (setq org-agenda-format-date #'prot-org-agenda-format-date-aligned))

;;;;; Prettier Org Constructs (org-modern.el)

(prot-emacs-elpa-package 'org-modern
  (setq org-modern-label-border 1)
  (setq org-modern-variable-pitch nil)
  (setq org-modern-timestamp t)
  (setq org-modern-table t)
  (setq org-modern-table-vertical 1)
  (setq org-modern-table-horizontal 0)
  (setq org-modern-list ; I swap the defaults for + and *
        '((?+ . "•")
          (?- . "–")
          (?* . "◦")))
  ;; I don't use those in documents anyway, and if I ever do I need to
  ;; remember what their standard looks are.
  (setq org-modern-internal-target nil)
  (setq org-modern-radio-target nil)

  ;; NOTE 2022-03-05: The variables that are commented out are the
  ;; defaults.

  ;; (setq org-modern-star ["◉""○""◈""◇""⁕"])
  ;; (setq org-modern-hide-stars 'leading)
  ;; (setq org-modern-checkbox
  ;;       '((?X . #("▢✓" 0 2 (composition ((2)))))
  ;;         (?- . #("▢–" 0 2 (composition ((2)))))
  ;;         (?\s . #("▢" 0 1 (composition ((1)))))))
  ;; (setq org-modern-horizontal-rule t)
  ;; (setq org-modern-priority t)
  ;; (setq org-modern-todo t)
  ;; (setq org-modern-tag t)
  ;; (setq org-modern-block t)
  ;; (setq org-modern-keyword t)
  ;; (setq org-modern-statistics t)
  ;; (setq org-modern-progress ["○""◔""◐""◕""●"])

  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

;;;;; Calendar

(prot-emacs-builtin-package 'calendar
  (setq calendar-mark-diary-entries-flag t)
  (setq calendar-mark-holidays-flag t)
  (setq calendar-mode-line-format nil)
  (setq calendar-time-display-form
        '(24-hours ":" minutes
                   (when time-zone
                     (format "(%s)" time-zone))))
  (setq calendar-week-start-day 1)      ; Monday
  (setq calendar-date-style 'iso)
  (setq calendar-date-display-form calendar-iso-date-display-form)
  (setq calendar-time-zone-style 'numeric) ; Emacs 28.1

  (setq calendar-christian-all-holidays-flag nil)
  (setq calendar-holidays (append holiday-christian-holidays
                                  holiday-solar-holidays
                                  holiday-local-holidays
                                  holiday-other-holidays))
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

  (add-hook 'calendar-today-visible-hook #'calendar-mark-today)

  (require 'solar)
  (setq calendar-latitude 48.86
        calendar-longitude 2.35)

  (require 'cal-dst)
  (setq calendar-standard-time-zone-name "+0100")
  (setq calendar-daylight-time-zone-name "+0200"))

;;;;; Appt

(prot-emacs-builtin-package 'appt
  (setq appt-display-diary nil)
  (setq appt-disp-window-function #'appt-disp-window)
  (setq appt-display-mode-line t)
  (setq appt-display-interval 3)
  (setq appt-audible nil)
  (setq appt-warning-time-regexp "appt \\([0-9]+\\)")
  (setq appt-message-warning-time 6)

  (run-at-time 10 nil #'appt-activate 1))

;;;;; Diary

(prot-emacs-builtin-package 'diary
  (setq diary-file "~/Sync/emacs/diary")
  (setq diary-date-forms diary-iso-date-forms)
  (setq diary-comment-start ";;")
  (setq diary-nonmarking-symbol "!")
  (setq diary-number-of-entries 2)
  (setq diary-mail-days 2)
  (setq diary-abbreviated-year-flag nil)

  (add-hook 'diary-list-entries-hook #'diary-sort-entries t)
  (add-hook 'diary-mode-hook #'goto-address-mode)

  ;; These presuppose (setq diary-display-function #'diary-fancy-display)
  (add-hook 'diary-list-entries-hook #'diary-include-other-diary-files)
  (add-hook 'diary-mark-entries-hook #'diary-mark-included-diary-files))

;;;;; Org-GTD

(prot-emacs-elpa-package 'org-gtd
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
    (define-key map (kbd "C-c c") #'org-gtd-capture)
    (define-key map (kbd "C-c d e") #'org-gtd-engage)
    (define-key map (kbd "C-c d n") #'org-gtd-show-all-next)
    (define-key map (kbd "C-c d p") #'org-gtd-process-inbox)
    (define-key map (kbd "C-c d s") #'org-gtd-show-stuck-projects))

  (define-key org-agenda-mode-map (kbd "g") #'org-gtd-engage)
  (define-key org-gtd-process-map (kbd "C-c c") #'org-gtd-choose))

;;;; Languages

;;;;; Plain Text

(prot-emacs-builtin-package 'text-mode)

(prot-emacs-builtin-package 'prot-text
  (add-to-list 'auto-mode-alist '("\\(README\\|CHANGELOG\\|COPYING\\|LICENSE\\)\\'" . text-mode))
  (define-key text-mode-map (kbd "<M-return>") #'prot-text-insert-heading)
  (define-key org-mode-map (kbd "<M-return>") #'org-meta-return) ; don't override M-RET here
  (define-key org-mode-map (kbd "M-;") nil))

;;;;; Markdown

(prot-emacs-elpa-package 'markdown-mode
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (setq markdown-fontify-code-blocks-natively t))

;;;;; YAML

(prot-emacs-elpa-package 'yaml-mode
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode)))

;;;;; CSS

(prot-emacs-builtin-package 'css-mode
  (add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
  (setq css-fontify-colors nil)

  (smart-tabs-add-language-support css css-mode-hook
    ((smie-indent-line . css-indent-offset)))
  (smart-tabs-insinuate 'css)

  (defun my/configure-css-mode ()
    "Configure CSS mode according to my preferences."
    (setq css-indent-offset 2)
    (setq indent-tabs-mode t)
    (setq tab-width css-indent-offset))

  (add-hook 'css-mode-hook #'my/configure-css-mode)
  (add-hook 'css-mode-hook #'my/whitespace-style-ignore-indentation))

;;;;; Emacs Lisp

(prot-emacs-builtin-package 'emacs-lisp-mode
  (defun my/configure-emacs-lisp-mode ()
    "Configure Emacs Lisp mode according to my preferences."
    (setq outline-regexp ";;;+ [^ ]")
    (outline-minor-mode 1))

  (add-hook 'emacs-lisp-mode-hook #'my/configure-emacs-lisp-mode))

;;;;; Shell Script

(prot-emacs-builtin-package 'sh-script
  (smart-tabs-add-language-support sh sh-mode-hook
    ((smie-indent-line . sh-basic-offset)))
  (smart-tabs-insinuate 'sh)

  (defun my/configure-sh-mode ()
    "Configure Shell-Script mode according to my preferences."
    (setq indent-tabs-mode t)
    (setq tab-width sh-basic-offset))

  (add-hook 'sh-mode-hook #'my/configure-sh-mode)
  (add-hook 'sh-mode-hook #'my/whitespace-style-ignore-indentation))

;;;;; Python

(prot-emacs-builtin-package 'python-mode
  (smart-tabs-insinuate 'python)

  (defun my/configure-python-mode ()
    "Configure Python mode according to my preferences."
    (setq indent-tabs-mode t)
    (setq tab-width python-indent-offset))

  (add-hook 'python-mode-hook #'my/configure-python-mode)
  (add-hook 'python-mode-hook #'my/whitespace-style-ignore-indentation))

;;;;; HTML

(prot-emacs-builtin-package 'mhtml-mode
  (smart-tabs-add-language-support mhtml mhtml-mode-hook
    ((mhtml-indent-line . sgml-basic-offset)))
  (smart-tabs-insinuate 'mhtml)

  (defun my/configure-html-mode ()
    "Configure HTML mode according to my preferences."
    (setq indent-tabs-mode t)
    (setq tab-width sgml-basic-offset))

  (add-hook 'mhtml-mode-hook #'my/configure-html-mode)
  (add-hook 'mhtml-mode-hook #'my/whitespace-style-ignore-indentation))

;;;;; JavaScript

(prot-emacs-builtin-package 'javascript-mode
  (smart-tabs-insinuate 'javascript))

;;;;; XML

(prot-emacs-builtin-package 'nxml-mode
  (smart-tabs-insinuate 'nxml)

  (defun my/configure-nxml-mode ()
    "Configure nXML mode according to my preferences."
    (setq indent-tabs-mode t)
    (setq tab-width nxml-child-indent))

  (add-hook 'nxml-mode-hook #'my/configure-nxml-mode)
  (add-hook 'nxml-mode-hook #'my/whitespace-style-ignore-indentation))

;;;;; Paragraphs

(prot-emacs-builtin-package 'prot-fill
  (setq prot-fill-default-column 72)
  (setq prot-fill-prog-mode-column 72)  ; Set this to another value if you want
  ;; Those variables come from various sources, though they feel part of the
  ;; same conceptual framework.
  (setq sentence-end-double-space t)
  (setq sentence-end-without-period nil)
  (setq colon-double-space nil)
  (setq use-hard-newlines nil)
  (setq adaptive-fill-mode t)
  (prot-fill-fill-mode 1)
  (column-number-mode 1)
  (add-hook 'after-init-hook #'column-number-mode)
  (add-hook 'text-mode-hook #'turn-on-visual-line-mode))

(prot-emacs-elpa-package 'adaptive-wrap
  (add-hook 'text-mode-hook #'adaptive-wrap-prefix-mode))

;;;;; Comments (newcomment.el and prot-comment.el)

(prot-emacs-builtin-package 'newcomment
  (setq comment-empty-lines t)
  (setq comment-fill-column nil)
  (setq comment-multi-line t)
  (setq comment-style 'multi-line)
  (let ((map global-map))
    (define-key map (kbd "C-:") #'comment-kill)         ; C-S-;
    (define-key map (kbd "M-;") #'comment-indent)))

;;;;; Electric Behaviour

(prot-emacs-builtin-package
    'electric
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  (setq electric-pair-preserve-balance t)
  (setq electric-quote-context-sensitive t)
  (setq electric-quote-replace-double t)
  (electric-quote-mode 1)

  (electric-pair-mode 1))

;;;;; Parentheses

(prot-emacs-builtin-package 'paren
  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-in-periphery nil)
  (setq show-paren-when-point-inside-paren nil)
  (setq show-paren-context-when-offscreen 'child-frame) ; Emacs 29
  (add-hook 'after-init-hook #'show-paren-mode))

;;;;; Tabs, Indentation, and the TAB Key

(setq-default tab-always-indent 'complete)
(setq-default tab-first-completion 'word-or-paren-or-punct)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq backward-delete-char-untabify-method nil)

(defun my/turn-on-indent-tabs-mode ()
  "Enable `indent-tabs-mode’."
  (setq indent-tabs-mode t))

;;;;; Smart Tabs for Indenting With Tabs and Aligning With Spaces

(prot-emacs-elpa-package 'smart-tabs-mode)

;;;;; Spell Checking

(prot-emacs-builtin-package 'flyspell
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  (setq ispell-dictionary "australian-w_accents"))

(prot-emacs-builtin-package 'prot-spell
  (setq prot-spell-dictionaries
        '(("EN English" . "australian-w_accents")
          ("FR Français" . "francais-lrg")
          ("NL Nederlands" . "dutch")
          ("ES Espanõl" . "español")))

  (let ((map global-map))
    (define-key map (kbd "M-$") #'prot-spell-spell-dwim)
    (define-key map (kbd "C-M-$") #'prot-spell-change-dictionary)))

;;;;; Flymake

(prot-emacs-builtin-package 'flymake
  (setq flymake-fringe-indicator-position 'left-fringe)
  (setq flymake-suppress-zero-counters t)
  (setq flymake-start-on-flymake-mode t)
  (setq flymake-no-changes-timeout nil)
  (setq flymake-start-on-save-buffer t)
  (setq flymake-proc-compilation-prevents-syntax-check t)
  (setq flymake-wrap-around nil)
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
    (define-key map (kbd "C-c ! p") #'flymake-goto-prev-error)))

;;;;; Flymake + Shellcheck

(prot-emacs-elpa-package 'flymake-shellcheck
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

;;;;; Flymake + Proselint

(prot-emacs-elpa-package 'flymake-proselint
  (dolist (hook '(markdown-mode-hook
                  org-mode-hook
                  text-mode-hook))
    (add-hook hook #'flymake-proselint-setup)
    (add-hook hook #'flymake-mode)))

;;;;; Flymake + Markdown

(prot-emacs-elpa-package 'flymake-markdownlint
  (add-hook 'markdown-mode-hook #'flymake-markdownlint-setup))

;;;;; Flymake + CSS

(prot-emacs-elpa-package 'flymake-css
  (add-hook 'css-mode-hook 'flymake-css-load))

;;;;; Eldoc

(prot-emacs-builtin-package 'eldoc
  (global-eldoc-mode 1))

;;;;; Handle performance for very long lines (so-long.el)

(prot-emacs-builtin-package 'so-long
  (global-so-long-mode 1))

;;;; Email

;;;;; Client-Agnostic Email Settings

(prot-emacs-builtin-package 'auth-source
  (setq auth-source-pass-filename "~/Sync/password-store")
  (auth-source-pass-enable)
  (setq user-full-name "David Porter")
  (setq user-mail-address "david@daporter.net"))

(prot-emacs-builtin-package 'message
  (setq mail-user-agent 'message-user-agent)
  (setq message-elide-ellipsis "\n> [... %l lines elided]\n")
  (setq compose-mail-user-agent-warnings nil)
  (setq message-mail-user-agent t)      ; use `mail-user-agent'
  (setq mail-signature "David Porter\n")
  (setq message-signature "David Porter\n")

  ;; Instead of using a citation format like this:
  ;;
  ;; On DATE, PERSON wrote:
  ;; > MESSAGE
  ;;
  ;; I disable the citation line and `message-ignored-cited-headers' to
  ;; get this template instead:
  ;;
  ;; > From: PERSON
  ;; > Date: DATE
  ;; >
  ;; > MESSAGE
  ;;
  ;; (setq message-citation-line-format "On %Y-%m-%d, %R %z, %f wrote:\n")
  ;; (setq message-citation-line-function 'message-insert-formatted-citation-line)
  (setq message-citation-line-function nil)
  (setq message-ignored-cited-headers nil) ; default is "." for all headers

  (setq message-confirm-send nil)
  (setq message-kill-buffer-on-exit t)
  (setq message-wide-reply-confirm-recipients t)
  (add-to-list 'mm-body-charset-encoding-alist '(utf-8 . base64))

  (add-hook 'message-setup-hook #'message-sort-headers))

(prot-emacs-builtin-package 'gnus-dired ; does not require `gnus'
  (add-hook 'dired-mode-hook #'gnus-dired-mode))

(prot-emacs-builtin-package 'prot-mail
  ;; NOTE 2021-05-14: This is a generic indicator for new mail in the
  ;; maildir.  As I now use notmuch (see relevant section in this
  ;; document) I have an alternative approach in prot-notmuch.el.
  (setq prot-mail-maildir-path-regexp "~/Mail/Inbox/new/") ; shell regexp
  (setq prot-mail-mode-line-indicator-commands
        '(notmuch-refresh-this-buffer))
  ;; mode line indicator with the number of new mails
  (prot-mail-mail-indicator -1))

;;;;; Notmuch

;; I install notmuch from the distro's repos because the CLI program is
;; not dependent on Emacs.  Though the package also includes notmuch.el
;; which is what we use here (they are maintained by the same people).
(add-to-list 'load-path "/usr/share/emacs/site-lisp/notmuch")

(prot-emacs-builtin-package 'notmuch

;;;;;; Account Settings

  (setq notmuch-identities '("David Porter <david@daporter.net>"))
  (setq notmuch-fcc-dirs "Sent")

;;;;;; General UI

  (setq notmuch-show-logo nil)
  (setq notmuch-column-control 1.0)
  (setq notmuch-hello-auto-refresh t)
  (setq notmuch-hello-recent-searches-max 20)
  (setq notmuch-hello-thousands-separator "")
  (setq notmuch-hello-sections '(notmuch-hello-insert-saved-searches))
  (setq notmuch-show-all-tags-list t)

;;;;;; Search

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
  (setq notmuch-search-line-faces
        '(("unread" . notmuch-search-unread-face)
          ("flag" . notmuch-search-flagged-face)))
  (setq notmuch-show-empty-saved-searches t)
  (setq notmuch-saved-searches
        '((:name "unread (inbox)"
                 :query "tag:unread and tag:inbox" :key "u")
          (:name "unread all"
                 :query "tag:unread not tag:archived" :key "U")
          (:name "inbox" :query "tag:inbox" :key "i")
          (:name "flagged" :query "tag:flagged" :key "f")
          (:name "reference"
                 :query "tag:reference not tag:archived" :key "r")
          (:name "emacs-humanities"
                 :query "tag:list/emacs-humanities" :key "eh")
          (:name "emacs-paris" :query "tag:list/emacs-paris" :key "ep")
          (:name "notmuch" :query "tag:list/notmuch" :key "n")
          (:name "great-conversation"
                 :query "tag:list/great-conversation" :key "g")
          (:name "prot-dotfiles"
                 :query "tag:list/prot-dotfiles" :key "pd")
          (:name "mailing lists" :query "tag:lists" :key "m")))

;;;;;; Tags

  (setq notmuch-archive-tags '("-inbox" "-unread" "+archived"))
  (setq notmuch-draft-folder "Drafts")
  (setq notmuch-tag-formats
        '(("unread" (propertize tag 'face 'notmuch-tag-unread))
          ("flag" (propertize tag 'face 'notmuch-tag-flagged))))
  (setq notmuch-tag-deleted-formats
        '(("unread" (notmuch-apply-face bare-tag `notmuch-tag-deleted))
          (".*" (notmuch-apply-face tag `notmuch-tag-deleted))))

  (setq notmuch-tagging-keys
        '(("r" notmuch-show-mark-read-tags "Mark read")
          ("a" notmuch-archive-tags "Archive")
          ("f" ("+flagged") "Flag")
          ("s" ("+spam" "-inbox") "Mark as spam")
          ("d" ("+deleted" "-inbox") "Delete")))

;;;;;; Email Composition

  (setq notmuch-address-command nil)    ; FIXME 2021-05-13: Make it work with EBDB
  (setq notmuch-always-prompt-for-sender t)
  (setq notmuch-mua-cite-function
        #'message-cite-original-without-signature)
  (setq notmuch-mua-user-agent-function #'notmuch-mua-user-agent-full)


;;;;;; Reading Messages

  (setq notmuch-show-indent-messages-width 0)
  (setq notmuch-wash-wrap-lines-length 100)
  (setq notmuch-unthreaded-show-out nil)

;;;;;; Hooks and Key Bindings

  (add-hook 'notmuch-mua-send-hook #'notmuch-mua-attachment-check)
  (remove-hook 'notmuch-show-hook #'notmuch-show-turn-on-visual-line-mode)
  (remove-hook 'notmuch-search-hook 'notmuch-hl-line-mode) ; Check my `lin' package
  (add-hook 'notmuch-show-hook (lambda ()
                                 (setq-local header-line-format nil)))

  (let ((map global-map))
    (define-key map (kbd "C-c m") #'notmuch)
    (define-key map (kbd "C-x m") #'notmuch-mua-new-mail)))

(prot-emacs-builtin-package 'prot-notmuch
  (add-to-list 'notmuch-tag-formats
               '("encrypted" (propertize tag 'face 'prot-notmuch-encrypted-tag)))
  (add-to-list 'notmuch-tag-formats
               '("sent" (propertize tag 'face 'prot-notmuch-sent-tag)))
  (add-to-list 'notmuch-tag-formats
               '("ref" (propertize tag 'face 'prot-notmuch-ref-tag)))
  (add-to-list 'notmuch-tag-formats
               '("todo" (propertize tag 'face 'prot-notmuch-todo-tag)))
  (add-to-list 'notmuch-tag-formats
               '("spam" (propertize tag 'face 'prot-notmuch-spam-tag))))

(prot-emacs-elpa-package 'ol-notmuch)

;;;;; Sending Email

(prot-emacs-builtin-package 'sendmail
  (setq send-mail-function 'smtpmail-send-it))

;; ;;;;; EBDB

(prot-emacs-elpa-package 'ebdb
  (require 'ebdb-message)
  (require 'ebdb-notmuch) ; FIXME 2021-05-13: does not activate the corfu-mode UI
  (setq ebdb-sources "~/Sync/emacs/ebdb.gpg")
  (setq ebdb-permanent-ignores-file "~/Sync/emacs/ebdb-permanent-ignores")

  (setq ebdb-mua-pop-up nil)
  (setq ebdb-default-window-size 0.25)
  (setq ebdb-mua-default-formatter ebdb-default-multiline-formatter)

  (setq ebdb-mua-auto-update-p 'existing)
  (setq ebdb-mua-reader-update-p 'existing)
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

  (with-eval-after-load 'prot-mail ; check my `prot-mail.el'
    (add-hook 'message-setup-hook #'prot-mail-ebdb-message-setup))

  (let ((map ebdb-mode-map))
    (define-key map (kbd "D") #'ebdb-delete-field-or-record)
    (define-key map (kbd "M") #'ebdb-mail) ; disables `ebdb-mail-each'
    (define-key map (kbd "m") #'ebdb-toggle-record-mark)
    (define-key map (kbd "t") #'ebdb-toggle-all-record-marks)
    (define-key map (kbd "T") #'ebdb-toggle-records-format) ; disables `ebdb-toggle-all-records-format'
    (define-key map (kbd "U") #'ebdb-unmark-all-records)))

;;;; Web

;;;;; Simple HTML Renderer (shr) and EWW

(prot-emacs-builtin-package 'browse-url
  (setq browse-url-browser-function #'eww-browse-url)
  (setq browse-url-secondary-browser-function
        #'browse-url-default-browser))

(prot-emacs-builtin-package 'shr
  (setq shr-use-colors nil)             ; t is bad for accessibility
  (setq shr-use-fonts nil)              ; t is not for me
  (setq shr-max-image-proportion 0.6)
  (setq shr-image-animate nil)          ; No GIFs, thank you!
  (setq shr-width fill-column)          ; check `prot-eww-readable'
  (setq shr-max-width fill-column)
  (setq shr-discard-aria-hidden t)
  (setq shr-cookie-policy nil))

(prot-emacs-builtin-package 'url-cookie
  (setq url-cookie-untrusted-urls '(".*")))

(prot-emacs-builtin-package 'eww
  (setq eww-restore-desktop t)
  (setq eww-desktop-remove-duplicates t)
  (setq eww-header-line-format nil)
  (setq eww-search-prefix "https://duckduckgo.com/html/?q=")
  (setq eww-download-directory (expand-file-name "~/Downloads"))
  (setq eww-suggest-uris '(eww-links-at-point
                           thing-at-point-url-at-point))
  (setq eww-bookmarks-directory "~/Sync/emacs/eww-bookmarks/")
  (setq eww-history-limit 150)
  (setq eww-use-external-browser-for-content-type
        "\\`\\(video/\\|audio\\)") ; On GNU/Linux check your mimeapps.list
  (setq eww-browse-url-new-window-is-tab nil)
  ;; NOTE `eww-retrieve-command' is for Emacs28.  I tried the following
  ;; two values.  The first would not render properly some plain text
  ;; pages, such as by messing up the spacing between paragraphs.  The
  ;; second is more reliable but feels slower.  So I just use the
  ;; default (nil), though I find wget to be a bit faster.  In that case
  ;; one could live with the occasional errors by using `eww-download'
  ;; on the offending page, but I prefer consistency.
  ;;
  ;; '("wget" "--quiet" "--output-document=-")
  ;; '("chromium" "--headless" "--dump-dom")
  (setq eww-retrieve-command nil)

  (define-key eww-link-keymap (kbd "v") nil) ; stop overriding `eww-view-source'
  (define-key eww-mode-map (kbd "L") #'eww-list-bookmarks)
  (define-key dired-mode-map (kbd "E") #'eww-open-file) ; to render local HTML files
  (define-key eww-buffers-mode-map (kbd "d") #'eww-bookmark-kill)   ; it actually deletes
  (define-key eww-bookmark-mode-map (kbd "d") #'eww-bookmark-kill))

;;;;; Elfeed Feed Reader

(prot-emacs-elpa-package 'elfeed
  (setq elfeed-use-curl nil)
  (setq elfeed-curl-max-connections 10)
  (setq elfeed-db-directory "~/Sync/emacs/elfeed/")
  (setq elfeed-enclosure-default-dir "~/Downloads/")
  (setq elfeed-search-filter "@4-months-ago +unread")
  (setq elfeed-sort-order 'ascending)
  (setq elfeed-search-clipboard-type 'CLIPBOARD)
  (setq elfeed-search-title-max-width 100)
  (setq elfeed-search-title-min-width 30)
  (setq elfeed-search-trailing-width 25)
  (setq elfeed-show-truncate-long-urls t)
  (setq elfeed-show-unique-buffers t)
  (setq elfeed-search-date-format '("%F %R" 16 :left))

  ;; Make sure to also check the section on shr and eww for how I handle
  ;; `shr-width' there.
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

  (define-key global-map (kbd "C-c e") #'my/elfeed-load-db-and-start)
  (let ((map elfeed-search-mode-map))
    (define-key map (kbd "q") #'my/elfeed-save-db-and-bury)
    (define-key map (kbd "w") #'elfeed-search-yank)
    (define-key map (kbd "g") #'elfeed-update)
    (define-key map (kbd "G") #'elfeed-search-update--force))
  (let ((map elfeed-show-mode-map))
    (define-key map (kbd "w") #'elfeed-show-yank))

  (with-eval-after-load 'elfeed
    (prot-emacs-builtin-package 'prot-elfeed
      (setq prot-elfeed-feeds-file "~/Sync/emacs/feeds.el")
      (setq prot-elfeed-tag-faces t)
      (prot-elfeed-fontify-tags)
      (add-hook 'elfeed-search-mode-hook #'prot-elfeed-load-feeds)

      (let ((map elfeed-search-mode-map))
        (define-key map (kbd "s") #'prot-elfeed-search-tag-filter)
        (define-key map (kbd "+") #'prot-elfeed-toggle-tag))
      (define-key elfeed-show-mode-map (kbd "+") #'prot-elfeed-toggle-tag))))

;;;;; Elfeed extensions for watching videos (elfeed-tube)

(prot-emacs-elpa-package 'elfeed-tube
  ;; (setq elfeed-tube-auto-save-p nil) ; default value
  ;; (setq elfeed-tube-auto-fetch-p t)  ; default value
  (elfeed-tube-setup)

  (let ((map elfeed-show-mode-map))
    (define-key map (kbd "F") #'elfeed-tube-fetch)
    (define-key map [remap save-buffer] #'elfeed-tube-save))
  (let ((map elfeed-search-mode-map))
    (define-key map (kbd "F") #'elfeed-tube-fetch)
    (define-key map [remap save-buffer] #'elfeed-tube-save)))

(prot-emacs-elpa-package 'mpv)

(prot-emacs-elpa-package 'elfeed-tube-mpv
  (define-key elfeed-search-mode-map (kbd "v") 'elfeed-tube-mpv)
  (let ((map elfeed-show-mode-map))
    (define-key map (kbd "v") 'elfeed-tube-mpv)
    (define-key map (kbd "C-c C-v") 'elfeed-tube-mpv)
    (define-key map (kbd "C-c C-f") 'elfeed-tube-mpv-follow-mode)
    (define-key map (kbd "C-c C-w") 'elfeed-tube-mpv-where)))

;;;; Conveniences

;;;;; Keymap for Buffers

(let ((map ctl-x-x-map))
  (define-key map (kbd "e") #'eval-buffer)
  (define-key map (kbd "f") #'follow-mode)
  (define-key map (kbd "r") #'rename-uniquely))

(with-eval-after-load 'org
  (define-key ctl-x-x-map "i" #'prot-org-id-headlines)
  (define-key ctl-x-x-map "h" #'prot-org-ox-html))

;;;;; Mouse wheel behaviour

(prot-emacs-builtin-package 'mouse
  ;; In Emacs 27+, use Control + mouse wheel to scale text.
  (setq mouse-wheel-scroll-amount
        '(1
          ((shift) . 5)
          ((meta) . 0.5)
          ((control) . text-scale)))
  (setq mouse-drag-copy-region nil)
  (setq make-pointer-invisible t)
  (setq mouse-wheel-progressive-speed t)
  (setq mouse-wheel-follow-mouse t)
  (add-hook 'after-init-hook #'mouse-wheel-mode)
  (define-key global-map (kbd "C-M-<mouse-3>") #'tear-off-window))

;;;;; Scrolling Behaviour

(setq-default scroll-preserve-screen-position t)
(setq-default scroll-conservatively 1)
(setq-default scroll-margin 0)
(setq-default next-screen-context-lines 0)

;;;;; Delete Selection

(prot-emacs-builtin-package 'delsel
  (add-hook 'after-init-hook #'delete-selection-mode))

;;;;; Tooltips (tooltip-mode)

(prot-emacs-builtin-package 'tooltip
  (setq tooltip-delay 0.5)
  (setq tooltip-short-delay 0.5)
  (setq x-gtk-use-system-tooltips nil)
  (setq tooltip-frame-parameters
        '((name . "tooltip")
          (internal-border-width . 6)
          (border-width . 0)
          (no-special-glyphs . t)))
  (add-hook 'after-init-hook #'tooltip-mode))

;;;;; Keychord Hints

(prot-emacs-elpa-package 'which-key
  (setq which-key-idle-delay 0.8)
  (setq which-key-idle-secondary-delay 0.05)
  (setq which-key-add-column-padding 2)

  (which-key-mode 1))

;;;;; Auto-Revert Mode

(prot-emacs-builtin-package 'autorevert
  (setq auto-revert-verbose t)
  (add-hook 'after-init-hook #'global-auto-revert-mode))

;;;;; Preserve Contents of System Clipboard

(setq save-interprogram-paste-before-kill t)

;;;;; Newline Characters for File Ending

(setq mode-require-final-newline 'visit-save)

;;;;; Go to Last Change

(prot-emacs-elpa-package 'goto-last-change
  (define-key global-map (kbd "C-c z z") #'goto-last-change))

;;;;; Repeatable Keychords

(prot-emacs-builtin-package 'repeat
  (setq repeat-on-final-keystroke t)
  (setq set-mark-command-repeat-pop t)

  (repeat-mode 1))

;;;;;; Automatic Time Stamps for Files

(add-hook 'before-save-hook #'time-stamp)

;;;;; Make Custom UI Code Disposable

(prot-emacs-builtin-package 'cus-edit
  ;; Disable the damn thing
  (setq custom-file (make-temp-file "emacs-custom-")))

;;;;; Bongo Music Manager

(prot-emacs-elpa-package 'bongo
  (setq bongo-enabled-backends '(mpv vlc))
  (setq bongo-mark-played-tracks t))

;;;;; Viewing PDFs

(prot-emacs-elpa-package 'pdf-tools
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

  ;; The following functions and hooks are adapted from the manual
  ;; modus-themes.

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
  (add-hook 'modus-themes-after-load-theme-hook
            #'my/pdf-tools-midnight-mode-toggle))

;;;;;; Open PDFs From Org Mode

(prot-emacs-elpa-package 'org-pdftools
  (add-hook 'org-mode-hook #'org-pdftools-setup-link))

;;;;; Viewing EPUBs (nov.el)

(prot-emacs-elpa-package 'nov
  (setq nov-text-width t)                 ; disable filling
  (add-hook 'nov-mode-hook #'logos-focus-mode)

  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;;;;; Kbd-Mode for KMonad

(add-to-list 'load-path (locate-user-emacs-file "lisp/kbd-mode"))
(require 'kbd-mode)

;;;; History

;;;;; Emacs Server and Desktop

(prot-emacs-builtin-package 'server
  (add-hook 'after-init-hook #'server-start))

(prot-emacs-builtin-package 'desktop
  (setq desktop-dirname user-emacs-directory)
  (setq desktop-base-file-name "desktop")
  (setq desktop-files-not-to-save ".*")
  (setq desktop-buffers-not-to-save ".*")
  (setq desktop-globals-to-clear nil)
  (setq desktop-restore-eager 0)
  (setq desktop-restore-frames nil)
  (dolist (symbol '(kill-ring
                    log-edit-comment-ring))
    (add-to-list 'desktop-globals-to-save symbol))

  (desktop-save-mode 1))

;;;;; Record Cursor Position

(prot-emacs-builtin-package 'saveplace
  (setq save-place-file (locate-user-emacs-file "saveplace"))
  (setq save-place-forget-unreadable-files t)
  (save-place-mode 1))

;;;;; Backups

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backup/"))))
(setq backup-by-copying t)
(setq version-control t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq create-lockfiles nil)

(setq safe-local-variable-values
      '((org-hide-leading-stars . t)
        (org-hide-macro-markers . t)))

;;; init.el ends here

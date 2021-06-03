;;; init.el --- Personal configuration file -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(defvar prot-emacs-autoinstall-elpa nil
  "Whether `prot-emacs-elpa-package' should install packages.
The default nil value means never to automatically install
packages.  A non-nil value is always interpreted as consent for
auto-installing everything---this process does not cover manually
maintained git repos, controlled by `prot-emacs-manual-package'.")

(defvar prot-emacs-basic-init "basic-init.el"
  "Name of 'basic init' file.

This file is meant to store user configurations that are evaluated
before loading `prot-emacs-configuration-main-file' and, when
available, `prot-emacs-configuration-user-file'.  Those values
control the behaviour of the Emacs setup.

The only variable that is currently expected to be in the 'basic
init' file is `prot-emacs-autoinstall-elpa'.

See `prot-emacs-basic-init-setup' for the actual initialisation
process.")

(defun prot-emacs-basic-init-setup ()
  "Load 'basic-init.el' if it exists.
This is meant to evaluate forms that control the rest of my Emacs
setup."
  (let* ((init prot-emacs-basic-init)
         (file (locate-user-emacs-file init)))
    (when (file-exists-p file)
      (load-file file))))

;; This variable is incremented in prot-emacs.org.  The idea is to
;; produce a list of packages that we want to install on demand from an
;; ELPA, when `prot-emacs-autoinstall-elpa' is set to nil (the default).
;;
;; So someone who tries to reproduce my Emacs setup will first get a
;; bunch of warnings about unavailable packages, though not
;; show-stopping errors, and will then have to use the command
;; `prot-emacs-install-ensured'.  After that command does its job, a
;; re-run of my Emacs configurations will yield the expected results.
;;
;; The assumption is that such a user will want to inspect the elements
;; of `prot-emacs-ensure-install', remove from the setup whatever code
;; block they do not want, and then call the aforementioned command.
;;
;; I do not want to maintain a setup that auto-installs everything on
;; first boot without requiring explicit consent.  I think that is a bad
;; practice because it teaches the user to simply put their faith in the
;; provider.
(defvar prot-emacs-ensure-install nil
  "List of package names used by `prot-emacs-install-ensured'.")

(defun prot-emacs-install-ensured ()
  "Install all `prot-emacs-ensure-install' packages, if needed.
If a package is already installed, no further action is performed
on it."
  (interactive)
  (when (yes-or-no-p (format "Try to install %d packages?"
                             (length prot-emacs-ensure-install)))
    (package-refresh-contents)
    (mapc (lambda (package)
            (unless (package-installed-p package)
              (package-install package)))
          prot-emacs-ensure-install)))

(defmacro prot-emacs-builtin-package (package &rest body)
  "Set up builtin PACKAGE with rest BODY.
PACKAGE is a quoted symbol, while BODY consists of balanced
expressions."
  (declare (indent 1))
  `(progn
     (unless (require ,package nil 'noerror)
       (display-warning 'prot-emacs (format "Loading `%s' failed" ,package) :warning))
     ,@body))

(defmacro prot-emacs-elpa-package (package &rest body)
  "Set up PACKAGE from an Elisp archive with rest BODY.
PACKAGE is a quoted symbol, while BODY consists of balanced
expressions.

When `prot-emacs-autoinstall-elpa' is non-nil try to install the
package if it is missing."
  (declare (indent 1))
  `(progn
     (when (and prot-emacs-autoinstall-elpa
                (not (package-installed-p ,package)))
       (package-install ,package))
     (if (require ,package nil 'noerror)
         (progn ,@body)
       (display-warning 'prot-emacs (format "Loading `%s' failed" ,package) :warning)
       (add-to-list 'prot-emacs-ensure-install ,package)
       (display-warning
        'prot-emacs
        (format "Run `prot-emacs-install-ensured' to install all packages in `prot-emacs-ensure-install'")
        :warning))))

(defmacro prot-emacs-manual-package (package &rest body)
  "Set up manually installed PACKAGE with rest BODY.
PACKAGE is a quoted symbol, while BODY consists of balanced
expressions."
  (declare (indent 1))
  (let ((path (thread-last user-emacs-directory
                (expand-file-name "contrib-lisp")
                (expand-file-name (symbol-name (eval package))))))
    `(progn
       (eval-and-compile
         (add-to-list 'load-path ,path))
       (if (require ,package nil 'noerror)
	       (progn ,@body)
         (display-warning 'prot-emacs (format "Loading `%s' failed" ,package) :warning)
         (display-warning 'prot-emacs (format "This must be available at %s" ,path) :warning)))))

(require 'vc)
(setq vc-follow-symlinks t) ; Because my dotfiles are managed that way

;; For my custom libraries
(add-to-list 'load-path (locate-user-emacs-file "lisp/"))

;; "prot-lisp" is for all my custom libraries; "contrib-lisp" is for
;; third-party code that I handle manually; while "modus-themes"
;; contains my themes which I use directly from source for development
;; purposes.
(dolist (path '("prot-lisp" "contrib-lisp" "modus-themes"))
  (add-to-list 'load-path (locate-user-emacs-file path)))

;; Some basic settings
(setq frame-title-format '("%b"))
(setq ring-bell-function 'ignore)

(setq use-short-answers t)

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'overwrite-mode 'disabled t)

(setq initial-buffer-choice t)			; always start with *scratch*

(defun prot-emacs--expand-file-name (file extension)
  "Return canonical path to FILE with EXTENSION."
  (expand-file-name
   (concat user-emacs-directory file extension)))

(prot-emacs-builtin-package 'prot-common)

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
          ("_ underscores"         . (95 95))))   ; _ _
  (setq prot-simple-date-specifier "%F")
  (setq prot-simple-time-specifier "%R %z")
  (setq delete-pair-blink-delay 0.15) ; Emacs28 -- see `prot-simple-delete-pair-dwim'

  ;; General commands
  (let ((map global-map))
    (define-key map (kbd "<insert>") nil)
    (define-key map (kbd "C-z") nil)
    (define-key map (kbd "C-x C-z") nil)
    (define-key map (kbd "C-h h") nil)
    (define-key map (kbd "M-`") nil)
    (define-key map (kbd "s-h") #'prot-simple-describe-symbol)
    (define-key map (kbd "C-h K") #'describe-keymap) ; overrides `Info-goto-emacs-key-command-node'
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
    (define-key map (kbd "C-'") #'prot-simple-insert-pair-completion)
    (define-key map (kbd "M-'") #'prot-simple-insert-pair-completion)
    (define-key map (kbd "M-\\") #'prot-simple-delete-pair-dwim)
    (define-key map (kbd "C-M-;") #'prot-simple-cite-region)
    (define-key map (kbd "C-M-^") #'prot-simple-insert-undercaret)
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
    ;; Commands for windows
    (define-key map (kbd "s-m") #'prot-simple-monocle)
    ;; Commands for buffers
    (define-key map (kbd "M-=") #'count-words)
    (define-key map (kbd "<C-f2>") #'prot-simple-rename-file-and-buffer)
    (define-key map (kbd "s-k") #'prot-simple-kill-buffer-current)))

(prot-emacs-builtin-package 'prot-pulse
  (setq prot-pulse-pulse-command-list
        '(recenter-top-bottom
          reposition-window
          bookmark-jump
          other-window))
  (prot-pulse-advice-commands-mode 1)
  (define-key global-map (kbd "<s-escape>") #'prot-pulse-pulse-line))

(prot-emacs-builtin-package 'cus-edit
  ;; Disable the damn thing
  (setq custom-file (make-temp-file "emacs-custom-")))

(prot-emacs-elpa-package 'modus-themes
  (setq modus-themes-bold-constructs t
        modus-themes-slanted-constructs t
        modus-themes-completions 'opinionated
        modus-themes-lang-checkers 'subtle-foreground-straight-underline
        modus-themes-region 'bg-only
        modus-themes-diffs 'desaturated
        modus-themes-org-blocks 'grayscale
        modus-themes-headings '((t . section))
        modus-themes-variable-pitch-headings t)

  ;; Load the theme files before enabling a theme (else you get an error).
  (modus-themes-load-themes)

  ;; Enable the theme at startup.  This is done after loading the files.
  ;; You only need `modus-themes-load-operandi' for the light theme or
  ;; `modus-themes-load-vivendi' for the dark one.  What I have here is
  ;; a simple test to load a light/dark theme based on some general time
  ;; ranges (just accounting for the hour and without checking for the
  ;; actual sunrise/sunset times).  Plus we have `modus-themes-toggle'
  ;; to switch themes at will.
  (let ((time (string-to-number (format-time-string "%H"))))
    (if (and (> time 5) (< time 18))
        (modus-themes-load-operandi)
      (modus-themes-load-vivendi)))

  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

(prot-emacs-builtin-package 'prot-fonts
  ;; This is defined in Emacs C code: it belongs to font settings.
  (setq x-underline-at-descent-line t)

  ;; Make sure to read the `prot-fonts-typeface-sets-alist' doc string,
  ;; as it explains what those property lists should contain.
  (setq prot-fonts-typeface-sets-alist

        '((laptop . ( :fixed-pitch-family "Hack"
                      :fixed-pitch-regular-weight normal
                      :fixed-pitch-heavy-weight bold
                      :fixed-pitch-height 90
                      :fixed-pitch-line-spacing 1
                      :variable-pitch-family "FiraGO"
                      :variable-pitch-height 0.9
                      :variable-pitch-regular-weight normal))

          (desktop . ( :fixed-pitch-family "Hack"
                       :fixed-pitch-regular-weight normal
                       :fixed-pitch-heavy-weight bold
                       :fixed-pitch-height 110
                       :fixed-pitch-line-spacing nil
                       :variable-pitch-family "FiraGO"
                       :variable-pitch-height 0.9
                       :variable-pitch-regular-weight normal))

          (presentation . ( :fixed-pitch-family "Hack"
                            :fixed-pitch-regular-weight normal
                            :fixed-pitch-heavy-weight bold
                            :fixed-pitch-height 150
                            :fixed-pitch-line-spacing nil
                            :variable-pitch-family "Source Sans Pro"
                            :variable-pitch-height 1.0
                            :variable-pitch-regular-weight normal))))

  ;; The value of `prot-fonts-laptop-desktop-keys-list' becomes '(laptop
  ;; desktop) based on the car of the first two cons cells found in
  ;; `prot-fonts-typeface-sets-alist'.  The assumption is that those
  ;; contain sets from smaller to larger display types.
  (setq prot-fonts-laptop-desktop-keys-list
        (prot-fonts-laptop-desktop-keys))

  ;; This is the breakpoint, in pixels, for determining whether we are
  ;; on the small or large screen layout.  The number here is my
  ;; laptop's screen width, while it expands beyond that when I connect
  ;; it to an external monitor (how I normally set it up on my desk).
  (setq prot-fonts-max-small-resolution-width 1600)

  ;; And this just sets the right font depending on whether my laptop is
  ;; connected to an external monitor or not.
  (prot-fonts-fonts-per-monitor)

  ;; See theme section for this hook and also read the doc string of
  ;; `prot-fonts-restore-last'.
  (add-hook 'modus-themes-after-load-theme-hook #'prot-fonts-restore-last)

  (define-key global-map (kbd "C-c f") #'prot-fonts-set-fonts))

(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

(prot-emacs-builtin-package 'so-long
  (global-so-long-mode 1))

(prot-emacs-builtin-package 'repeat
  (repeat-mode 1))

(prot-emacs-elpa-package 'which-key
  ;; NOTE: I only use this for `embark' and `consult' and for the sake
  ;; of producing more user-friendly video demonstrations.
  (setq which-key-dont-use-unicode t)
  (setq which-key-add-column-padding 2)
  (setq which-key-show-early-on-C-h nil)
  (setq which-key-idle-delay most-positive-fixnum) ; set this to something like 0.8
  (setq which-key-idle-secondary-delay 0.05)
  (setq which-key-popup-type 'side-window)
  (setq which-key-show-prefix 'echo)
  (setq which-key-max-display-columns 3)
  (setq which-key-separator "  ")
  (setq which-key-special-keys nil)
  (setq which-key-paging-key "<next>")
  (which-key-mode -1))	   ; and turn this on, if you want to use this

(prot-emacs-builtin-package 'prot-orderless
  (setq prot-orderless-default-styles
        '(orderless-prefixes
          orderless-strict-leading-initialism
          orderless-regexp))
  (setq prot-orderless-alternative-styles
        '(orderless-literal
          orderless-prefixes
          orderless-strict-leading-initialism
          orderless-regexp)))

(prot-emacs-elpa-package 'orderless
  (setq orderless-component-separator " +")
  (setq orderless-matching-styles prot-orderless-default-styles)
  (setq orderless-style-dispatchers
        '(prot-orderless-literal-dispatcher
          prot-orderless-initialism-dispatcher
          prot-orderless-flex-dispatcher))
  ;; SPC should never complete: use it for `orderless' groups.
  (let ((map minibuffer-local-completion-map))
    (define-key map (kbd "SPC") nil)
    (define-key map (kbd "?") nil)))

(prot-emacs-elpa-package 'marginalia
  (setq marginalia-annotators
        '(marginalia-annotators-heavy
          marginalia-annotators-light))
  (marginalia-mode 1))

(prot-emacs-builtin-package 'minibuffer
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
  ;;  FIXME 2021-05-21: the group headings break prot-minibuffer cycling
  (setq completions-group nil)
  ;; (setq completions-group-sort 'alphabetical)
  ;; (setq completions-group-format
  ;;       (concat
  ;;        (propertize "    " 'face 'completions-group-separator)
  ;;        (propertize " %s " 'face 'completions-group-title)
  ;;        (propertize " " 'face 'completions-group-separator
  ;;                    'display '(space :align-to right))))

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

  ;; Defines, among others, aliases for common minibuffer commands to
  ;; Super-KEY.  Normally these should go in individual package
  ;; configurations, but their grouping here makes things easier to
  ;; understand.  Besides, they are related to the minibuffer.
  (define-key completion-list-mode-map (kbd "<tab>") #'choose-completion)
  (let ((map global-map))
    (define-key map (kbd "s-b") #'switch-to-buffer)
    (define-key map (kbd "s-B") #'switch-to-buffer-other-window)
    (define-key map (kbd "s-f") #'find-file)
    (define-key map (kbd "s-F") #'find-file-other-window)
    (define-key map (kbd "s-d") #'dired)
    (define-key map (kbd "s-D") #'dired-other-window))
  (let ((map minibuffer-local-completion-map))
    (define-key map (kbd "C-j") #'exit-minibuffer)
    (define-key map (kbd "<tab>") #'minibuffer-force-complete))
  (let ((map minibuffer-local-must-match-map))
    ;; I use this prefix for other searches
    (define-key map (kbd "M-s") nil)))

(prot-emacs-builtin-package 'prot-minibuffer
  (setq-default prot-minibuffer-mini-cursors t) ; also check `prot-cursor.el'
  (setq prot-minibuffer-remove-shadowed-file-names t)
  (setq prot-minibuffer-minimum-input 3)
  (setq prot-minibuffer-live-update-delay 0.5)

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
        '( dired-goto-file vc-retrieve-tag
           prot-bongo-playlist-insert-playlist-file))

  (define-key global-map (kbd "s-v") #'prot-minibuffer-focus-mini-or-completions)
  (let ((map completion-list-mode-map))
    (define-key map (kbd "h") #'prot-simple-describe-symbol) ; from `prot-simple.el'
    (define-key map (kbd "M-g") #'prot-minibuffer-choose-completion-number)
    (define-key map (kbd "M-v") #'prot-minibuffer-focus-minibuffer)
    (define-key map (kbd "C-g") #'prot-minibuffer-keyboard-quit-dwim)
    (define-key map (kbd "C-n") #'prot-minibuffer-next-completion-or-mini)
    (define-key map (kbd "<down>") #'prot-minibuffer-next-completion-or-mini)
    (define-key map (kbd "C-p") #'prot-minibuffer-previous-completion-or-mini)
    (define-key map (kbd "<up>") #'prot-minibuffer-previous-completion-or-mini)
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
    (define-key map (kbd "C-n") #'prot-minibuffer-switch-to-completions-top)
    (define-key map (kbd "<down>") #'prot-minibuffer-switch-to-completions-top)
    (define-key map (kbd "C-p") #'prot-minibuffer-switch-to-completions-bottom)
    (define-key map (kbd "<up>") #'prot-minibuffer-switch-to-completions-bottom)
    (define-key map (kbd "C-l") #'prot-minibuffer-toggle-completions)) ; "list" mnemonic
  (let ((map minibuffer-local-filename-completion-map))
    (define-key map (kbd "<M-backspace>") #'prot-minibuffer-backward-updir))
  (add-hook 'minibuffer-setup-hook #'prot-minibuffer-mini-cursor)
  (add-hook 'completion-list-mode-hook #'prot-minibuffer-completions-cursor)
  (add-hook 'completion-list-mode-hook #'prot-minibuffer-hl-line)
  (add-hook 'completion-list-mode-hook
            #'prot-minibuffer-display-line-numbers))

(prot-emacs-elpa-package 'consult
  (setq consult-line-numbers-widen t)
  ;; ;; FIXME 2021-04-10: This does not work with `prot-minibuffer.el'.
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
  (setq register-preview-delay 0.8
        register-preview-function #'consult-register-format)
  (setq consult-find-command "find . -iname *ARG* OPTS")
  (setq consult-preview-key 'any)

  ;; Enables previews inside the standard *Completions* buffer (what
  ;; `prot-minibuffer.el' uses).
  (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)

  (let ((map global-map))
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
  (define-key consult-narrow-map (kbd "?") #'consult-narrow-help))

(prot-emacs-builtin-package 'prot-consult
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
    (define-key map (kbd "M-s M-l") #'prot-consult-line)))

(prot-emacs-elpa-package 'embark
  (setq embark-collect-initial-view-alist
        '((file . list)
          (buffer . list)
          (symbol . list)
          (line . list)
          (xref-location . list)
          (kill-ring . zebra)
          (t . list)))
  (setq embark-quit-after-action t)     ; XXX: Read the doc string!
  (setq embark-collect-live-update-delay 0.5)
  (setq embark-collect-live-initial-delay 0.8)

  (define-key global-map (kbd "C-,") #'embark-act)
  (let ((map minibuffer-local-completion-map))
    (define-key map (kbd "C-,") #'embark-act)
    (define-key map (kbd "C->") #'embark-become)
    (define-key map (kbd "M-q") #'embark-collect-toggle-view)) ; parallel of `fill-paragraph'
  (let ((map embark-collect-mode-map))
    (define-key map (kbd "C-,") #'embark-act)
    (define-key map (kbd ",") #'embark-act)
    (define-key map (kbd "M-q") #'embark-collect-toggle-view))
  (let ((map embark-region-map))
    (define-key map (kbd "a") #'align-regexp)
    (define-key map (kbd "i") #'epa-import-keys-region)
    (define-key map (kbd "s") #'sort-lines)
    (define-key map (kbd "u") #'untabify))
  (let ((map embark-symbol-map))
    (define-key map (kbd ".") #'embark-find-definition)
    (define-key map (kbd "k") #'describe-keymap)))

(prot-emacs-builtin-package 'prot-embark
  (prot-embark-keymaps 1)
  (prot-embark-setup-packages 1))

(prot-emacs-elpa-package 'keyfreq
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(prot-emacs-builtin-package 'project
  ;; ;; Use this for Emacs 27 (I am on 28)
  ;; (add-to-list 'prot-emacs-ensure-install 'project)
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
  (define-key global-map (kbd "C-x p q") #'project-query-replace-regexp)) ; C-x p is `project-prefix-map'

(prot-emacs-builtin-package 'prot-project
  (setq prot-project-project-roots '("~/src"))
  (setq prot-project-commit-log-limit 25)
  (setq prot-project-large-file-lines 1000)
  (let ((map global-map))
    (define-key map (kbd "C-x p <delete>") #'prot-project-remove-project)
    (define-key map (kbd "C-x p l") #'prot-project-commit-log)
    (define-key map (kbd "C-x p m") #'prot-project-magit-status)
    (define-key map (kbd "C-x p s") #'prot-project-find-subdir)
    (define-key map (kbd "C-x p t") #'prot-project-retrieve-tag)))

(prot-emacs-builtin-package 'recentf
  (setq recentf-save-file (locate-user-emacs-file "recentf"))
  (setq recentf-max-saved-items 200)
  (setq recentf-exclude '(".gz" ".xz" ".zip" "/elpa/" "/ssh:" "/sudo:"))
  (add-hook 'after-init-hook #'recentf-mode))

(prot-emacs-builtin-package 'prot-recentf
  (add-to-list 'recentf-keep 'prot-recentf-keep-predicate)
  (let ((map global-map))
    (define-key map (kbd "s-r") #'prot-recentf-recent-files)
    (define-key map (kbd "C-x C-r") #'prot-recentf-recent-dirs)))

(prot-emacs-elpa-package 'corfu
  (dolist (mode '( message-mode-hook text-mode-hook prog-mode-hook
                   shell-mode-hook eshell-mode-hook))
    (add-hook mode #'corfu-mode))
  (define-key corfu-map (kbd "<tab>") #'corfu-complete))

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
    (define-key map (kbd "s-/") #'dabbrev-completion)))

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
  (define-key minibuffer-local-isearch-map (kbd "M-/") #'isearch-complete-edit)
  (let ((map isearch-mode-map))
    (define-key map (kbd "C-g") #'isearch-cancel) ; instead of `isearch-abort'
    (define-key map (kbd "M-/") #'isearch-complete)))

(prot-emacs-builtin-package 'replace
  (setq list-matching-lines-jump-to-current-line t)
  (add-hook 'occur-mode-hook #'hl-line-mode)
  (add-hook 'occur-mode-hook #'prot-common-truncate-lines-silently) ; from `prot-common.el'
  (define-key global-map (kbd "M-s M-o") #'multi-occur)
  (define-key occur-mode-map (kbd "t") #'toggle-truncate-lines))

(prot-emacs-builtin-package 'grep)

(prot-emacs-builtin-package 'prot-search
  (let ((map global-map))
    (define-key map (kbd "M-s %") #'prot-search-isearch-replace-symbol)
    (define-key map (kbd "M-s M-<") #'prot-search-isearch-beginning-of-buffer)
    (define-key map (kbd "M-s M->") #'prot-search-isearch-end-of-buffer)
    (define-key map (kbd "M-s g") #'prot-search-grep)
    (define-key map (kbd "M-s u") #'prot-search-occur-urls)
    (define-key map (kbd "M-s M-u") #'prot-search-occur-browse-url))
  (let ((map isearch-mode-map))
    (define-key map (kbd "<up>") #'prot-search-isearch-repeat-backward)
    (define-key map (kbd "<down>") #'prot-search-isearch-repeat-forward)
    (define-key map (kbd "<backspace>") #'prot-search-isearch-abort-dwim)
    (define-key map (kbd "<C-return>") #'prot-search-isearch-other-end)))

(prot-emacs-builtin-package 're-builder
  (setq reb-re-syntax 'read))

(prot-emacs-elpa-package 'wgrep
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t)
  (let ((map grep-mode-map))
    (define-key map (kbd "e") #'wgrep-change-to-wgrep-mode)
    (define-key map (kbd "C-x C-q") #'wgrep-change-to-wgrep-mode)
    (define-key map (kbd "C-c C-c") #'wgrep-finish-edit)))

(prot-emacs-builtin-package 'xref
  ;; All those have been changed for Emacs 28
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read) ; for M-.
  (setq xref-show-xrefs-function #'xref-show-definitions-buffer) ; for grep and the like
  (setq xref-file-name-display 'project-relative)
  (setq xref-search-program 'grep))

(prot-emacs-builtin-package 'dired
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-AGFhlv --group-directories-first --time-style=long-iso")
  (setq dired-dwim-target t)
  (setq dired-auto-revert-buffer #'dired-directory-changed-p) ; also see `dired-do-revert-buffer'

  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'hl-line-mode))

(prot-emacs-builtin-package 'dired-aux
  (setq dired-isearch-filenames 'dwim)
  ;; The following variables were introduced in Emacs 27.1
  (setq dired-create-destination-dirs 'ask)
  (setq dired-vc-rename-file t)
  ;; And this is for Emacs 28
  (setq dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir))))

  ;; Those two functions are copied from the Emacs config of Omar
  ;; Antolín Camarena: <https://github.com/oantolin/emacs-config>.
  (defun contrib/cdb--bookmarked-directories ()
    (bookmark-maybe-load-default-file)
    (cl-loop for (name . props) in bookmark-alist
             for fn = (cdr (assq 'filename props))
             when (and fn (string-suffix-p "/" fn))
             collect (cons name fn)))

  (defun contrib/cd-bookmark (bm)
    "Insert the path of a bookmarked directory."
    (interactive
     (list (let ((enable-recursive-minibuffers t))
             (completing-read
              "Directory: " (contrib/cdb--bookmarked-directories) nil t))))
    (when (minibufferp)
      (delete-region (minibuffer-prompt-end) (point-max)))
    (insert (cdr (assoc bm (contrib/cdb--bookmarked-directories)))))

  (let ((map dired-mode-map))
    (define-key map (kbd "C-+") #'dired-create-empty-file)
    (define-key map (kbd "M-s f") #'nil)
    (define-key map (kbd "C-x v v") #'dired-vc-next-action)) ; Emacs 28
  (define-key minibuffer-local-filename-completion-map (kbd "C-c d") #'contrib/cd-bookmark))

(prot-emacs-builtin-package 'prot-dired
  (let ((map dired-mode-map))
    (define-key map (kbd "M-n") #'prot-dired-subdirectory-next)
    (define-key map (kbd "C-c C-n") #'prot-dired-subdirectory-next)
    (define-key map (kbd "M-p") #'prot-dired-subdirectory-previous)
    (define-key map (kbd "C-c C-p") #'prot-dired-subdirectory-next)))

(prot-emacs-builtin-package 'dired-x
  (setq dired-clean-up-buffers-too t)
  (setq dired-clean-confirm-killing-deleted-buffers t)
  (setq dired-x-hands-off-my-keys t)    ; easier to show the keys I use
  (setq dired-bind-man nil)
  (setq dired-bind-info nil)
  (let ((map global-map))
    (define-key map (kbd "C-x C-j") #'dired-jump)
    (define-key map (kbd "s-j") #'dired-jump)
    (define-key map (kbd "C-x 4 C-j") #'dired-jump-other-window)
    (define-key map (kbd "s-J") #'dired-jump-other-window))
  (define-key dired-mode-map (kbd "I") #'dired-info))

(prot-emacs-elpa-package 'dired-subtree
  (setq dired-subtree-use-backgrounds nil)
  (let ((map dired-mode-map))
    (define-key map (kbd "<tab>") #'dired-subtree-toggle)
    (define-key map (kbd "<C-tab>") #'dired-subtree-cycle)
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

(let ((map ctl-x-x-map))              ; Emacs 28
  (define-key map "e" #'eval-buffer)
  (define-key map "f" #'follow-mode)  ; override `font-lock-update'
  (define-key map "r" #'rename-uniquely))

(with-eval-after-load 'org
  (define-key ctl-x-x-map "i" #'contrib/org-id-headlines)
  (define-key ctl-x-x-map "h" #'prot/ox-html))

(prot-emacs-builtin-package 'uniquify
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-strip-common-suffix t)
  (setq uniquify-after-kill-buffer-p t))

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

(prot-emacs-builtin-package 'prot-ibuffer
  (let ((map global-map))
    (define-key map (kbd "M-s b") #'prot-ibuffer-buffers-major-mode)
    (define-key map (kbd "M-s v") #'prot-ibuffer-buffers-vc-root)))

(prot-emacs-elpa-package 'scratch
  ;; TODO 2021-01-19: refine `prot/scratch-buffer-setup'
  (defun prot/scratch-buffer-setup ()
    "Add contents to `scratch' buffer and name it accordingly.
If region is active, add its contents to the new buffer."
    (let* ((mode major-mode)
           (string (format "Scratch buffer for: %s\n\n" mode))
           (region (with-current-buffer (current-buffer)
                     (if (region-active-p)
                         (buffer-substring-no-properties
                          (region-beginning)
                          (region-end)))
                     ""))
           (text (concat string region)))
      (when scratch-buffer
	    (save-excursion
          (insert text)
          (goto-char (point-min))
          (comment-region (point-at-bol) (point-at-eol)))
	    (forward-line 2))
      (rename-buffer (format "*Scratch for %s*" mode) t)))
  (add-hook 'scratch-create-buffer-hook #'prot/scratch-buffer-setup)
  (define-key global-map (kbd "C-c s") #'scratch))

(prot-emacs-builtin-package 'window
  (setq display-buffer-alist
        `(;; top side window
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
          ("\\*\\(Flymake\\|Package-Lint\\|vc-git :\\).*"
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
           (slot . 2)
           (window-parameters . ((no-other-window . t))))
          ;; bottom side window
          ("\\*\\(Embark\\)?.*Completions.*"
           (display-buffer-in-side-window)
           (side . bottom)
           (slot . 0)
           (window-parameters . ((no-other-window . t)
                                 (mode-line-format . none))))
          ;; left side window
          ("\\*Help.*"
           (display-buffer-in-side-window)
           (window-width . 0.20)       ; See the :hook
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
          ("\\*Faces\\*"
           (display-buffer-in-side-window)
           (window-width . 0.25)
           (side . right)
           (slot . 0))
          ("\\*Custom.*"
           (display-buffer-in-side-window)
           (window-width . 0.25)
           (side . right)
           (slot . 1))
          ;; bottom buffer (NOT side window)
          ("\\*\\vc-\\(incoming\\|outgoing\\).*"
           (display-buffer-at-bottom))
          ("\\*\\(Output\\|Register Preview\\).*"
           (display-buffer-at-bottom))
          ("\\*.*\\(e?shell\\|v?term\\).*"
           (display-buffer-reuse-mode-window display-buffer-at-bottom)
           (window-height . 0.2))
          ;; below currect window
          ("\\*Calendar.*"
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (window-height . shrink-window-if-larger-than-buffer))))
  (setq window-combination-resize t)
  (setq even-window-sizes 'height-only)
  (setq window-sides-vertical nil)
  (setq switch-to-buffer-in-dedicated-window 'pop)
  (add-hook 'help-mode-hook #'visual-line-mode)
  (add-hook 'custom-mode-hook #'visual-line-mode)
  (let ((map global-map))
    (define-key map (kbd "s-n") #'next-buffer)
    (define-key map (kbd "s-p") #'previous-buffer)
    (define-key map (kbd "s-o") #'other-window)
    (define-key map (kbd "s-2") #'split-window-below)
    (define-key map (kbd "s-3") #'split-window-right)
    (define-key map (kbd "s-0") #'delete-window)
    (define-key map (kbd "s-1") #'delete-other-windows)
    (define-key map (kbd "s-!") #'delete-other-windows-vertically) ; s-S-1
    (define-key map (kbd "s-5") #'delete-frame)
    (define-key map (kbd "C-x _") #'balance-windows)      ; underscore
    (define-key map (kbd "C-x -") #'fit-window-to-buffer) ; hyphen
    (define-key map (kbd "C-x +") #'balance-windows-area)
    (define-key map (kbd "s-q") #'window-toggle-side-windows)
    (define-key map (kbd "C-x }") #'enlarge-window)
    (define-key map (kbd "C-x {") #'shrink-window)
    (define-key map (kbd "C-x >") #'enlarge-window-horizontally) ; override `scroll-right'
    (define-key map (kbd "C-x <") #'shrink-window-horizontally)) ; override `scroll-left'
  (let ((map resize-window-repeat-map))
    (define-key map ">" #'enlarge-window-horizontally)
    (define-key map "<" #'shrink-window-horizontally)))

(prot-emacs-builtin-package 'tab-bar
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
  (setq tab-bar-close-tab-select 'recent)
  (setq tab-bar-new-tab-choice t)
  (setq tab-bar-new-tab-to 'right)
  (setq tab-bar-position nil)
  (setq tab-bar-show nil)
  (setq tab-bar-tab-hints nil)
  (setq tab-bar-tab-name-function 'tab-bar-tab-name-current)
  (setq tab-bar-format                  ; Emacs 28
        '(tab-bar-format-tabs
          tab-bar-separator
          tab-bar-format-align-right
          tab-bar-format-global))
  (tab-bar-mode -1)
  (tab-bar-history-mode -1)
  (let ((map global-map))
    (define-key map (kbd "<s-tab>") #'tab-next)
    (define-key map (kbd "<s-backtab>") #'tab-previous))) ; S-s-TAB

(prot-emacs-builtin-package 'prot-tab
  (let ((map global-map))
    (define-key map (kbd "<f8>") #'prot-tab-tab-bar-toggle)
    (define-key map (kbd "C-x t t") #'prot-tab-select-tab-dwim)
    (define-key map (kbd "s-t") #'prot-tab-select-tab-dwim)))

(prot-emacs-elpa-package 'tab-bar-echo-area
  (tab-bar-echo-area-mode 1))

(prot-emacs-elpa-package 'tab-bar-groups
  (tab-bar-groups-activate)

  (let ((map tab-prefix-map))           ; the prefix is C-x t
    (define-key map (kbd "g 0") #'tab-bar-groups-close-group)
    (define-key map (kbd "g 2") #'tab-bar-groups-new-tab)
    (define-key map (kbd "g a") #'tab-bar-groups-assign-group)
    (define-key map (kbd "g g") #'tab-bar-groups-regroup-tabs)
    (define-key map (kbd "g d") #'tab-bar-groups-duplicate-tab)
    (define-key map (kbd "g e") #'tab-bar-groups-eject-tab)
    (define-key map (kbd "g r") #'tab-bar-groups-rename-group)))

(prot-emacs-elpa-package 'transpose-frame
  (let ((map global-map))
    (define-key map (kbd "C-s-t") #'flop-frame) ; what I consider "transpose" in this context
    (define-key map (kbd "C-s-r") #'rotate-frame-clockwise)))

(prot-emacs-builtin-package 'face-remap)

(prot-emacs-elpa-package 'olivetti
  (setq olivetti-body-width 0.7)
  (setq olivetti-minimum-body-width 80)
  (setq olivetti-recall-visual-line-mode-entry-state t))

(prot-emacs-builtin-package 'prot-logos
  (setq prot-logos-org-presentation nil)
  (setq prot-logos-variable-pitch nil)
  (setq prot-logos-scroll-lock nil)
  (setq prot-logos-hidden-modeline t)
  (define-key global-map (kbd "<f11>") #'prot-logos-focus-mode))

(prot-emacs-builtin-package 'tmr
  (setq tmr-sound-file
        "/usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga"))

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

  ;; NOTE: I override lots of the defaults
  (let ((map global-map))
    (define-key map (kbd "C-x v a") #'vc-annotate) ; `vc-update-change-log' is not in git
    (define-key map (kbd "C-x v b") #'vc-retrieve-tag)  ; "branch" switch
    (define-key map (kbd "C-x v t") #'vc-create-tag)
    (define-key map (kbd "C-x v f") #'vc-log-incoming)  ; the actual git fetch
    (define-key map (kbd "C-x v o") #'vc-log-outgoing)
    (define-key map (kbd "C-x v F") #'vc-update)        ; "F" because "P" is push
    (define-key map (kbd "C-x v d") #'vc-diff))
  (let ((map vc-dir-mode-map))
    (define-key map (kbd "a") #'vc-annotate)
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
    (define-key map (kbd "C-x v c") #'prot-vc-git-patch-dwim)
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
    (define-key map (kbd "c") #'prot-vc-git-patch-dwim)
    (define-key map (kbd "R") #'prot-vc-git-log-reset)
    (define-key map (kbd "w") #'prot-vc-log-kill-hash)))

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
        '(("~/src" . 1))))

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
  (define-key global-map (kbd "<s-return>") #'eshell)
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
    (define-key map (kbd "C-c M-h") #'prot-eshell-narrow-output-highlight-regexp))
  (let ((map eshell-hist-mode-map))
    (define-key map (kbd "M-s") #'nil) ; I use this prefix for lots of more useful commands
    (define-key map (kbd "M-r") #'prot-eshell-complete-history)
    (define-key map (kbd "C-c C-d") #'prot-eshell-complete-recent-dir)
    (define-key map (kbd "C-c C-s")
      #'prot-eshell-find-subdirectory-recursive)))

(prot-emacs-builtin-package 'shell
  (setq ansi-color-for-comint-mode t)
  (setq shell-command-prompt-show-cwd t) ; Emacs 27.1
  (define-key global-map (kbd "<s-S-return>") #'shell))

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

  (require 'solar)
  (setq calendar-latitude 35.17         ; Not my actual coordinates
        calendar-longitude 33.36)

  (require 'cal-dst)
  (setq calendar-standard-time-zone-name "+0200")
  (setq calendar-daylight-time-zone-name "+0300")

  (require 'diary-lib)
  (setq diary-mail-addr user-mail-address)
  (setq diary-date-forms diary-iso-date-forms)
  (setq diary-comment-start ";;")
  (setq diary-comment-end "")
  (setq diary-nonmarking-symbol "!")
  (setq diary-show-holidays-flag t)
  (setq diary-display-function #'diary-fancy-display) ; better than its alternative
  (setq diary-header-line-format nil)
  (setq diary-list-include-blanks nil)
  (setq diary-number-of-entries 2)
  (setq diary-mail-days 2)
  (setq diary-abbreviated-year-flag nil)

  (add-hook 'calendar-today-visible-hook #'calendar-mark-today)
  (add-hook 'diary-list-entries-hook 'diary-sort-entries t)
  (add-hook 'diary-mode-hook #'goto-address-mode) ; buttonise plain text links

  ;; Those presuppose (setq diary-display-function #'diary-fancy-display)
  (add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
  (add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files)

  ;; Prevent Org from interfering with my key bindings.
  (remove-hook 'calendar-mode-hook #'org--setup-calendar-bindings)

  (let ((map calendar-mode-map))
    (define-key map (kbd "s") #'calendar-sunrise-sunset)
    (define-key map (kbd "l") #'lunar-phases)
    (define-key map (kbd "i") nil) ; Org sets this, much to my chagrin (see `remove-hook' above)
    (define-key map (kbd "i a") #'diary-insert-anniversary-entry)
    (define-key map (kbd "i b") #'diary-insert-block-entry)
    (define-key map (kbd "i c") #'diary-insert-cyclic-entry)
    (define-key map (kbd "i d") #'diary-insert-entry) ; for current "day"
    (define-key map (kbd "i i") #'diary-insert-entry) ; most common action, easier to type
    (define-key map (kbd "i m") #'diary-insert-monthly-entry)
    (define-key map (kbd "i w") #'diary-insert-weekly-entry)
    (define-key map (kbd "i y") #'diary-insert-yearly-entry)
    (define-key map (kbd "M-n") #'calendar-forward-month)
    (define-key map (kbd "M-p") #'calendar-backward-month)))

(prot-emacs-builtin-package 'mm-encode
  (setq mm-encrypt-option nil) ; use 'guided if you need more control
  (setq mm-sign-option nil))   ; same

(prot-emacs-builtin-package 'mml-sec
  (setq mml-secure-openpgp-encrypt-to-self t)
  (setq mml-secure-openpgp-sign-with-sender t)
  (setq mml-secure-smime-encrypt-to-self t)
  (setq mml-secure-smime-sign-with-sender t))

(prot-emacs-builtin-package 'message
  (setq mail-user-agent 'message-user-agent)
  (setq mail-header-separator (purecopy "*****"))
  (setq compose-mail-user-agent-warnings nil)
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

  (defun prot/message-header-add-gcc ()
    "While `gnus' is running, add pre-populated Gcc header.

The Gcc header places a copy of the outgoing message to the
appropriate directory of the IMAP server, as per the contents of
~/.authinfo.gpg.

In the absence of a Gcc header, the outgoing message will not
appear in the appropriate maildir directory, though it will still
be sent.

Add this function to `message-header-setup-hook'."
    (if (gnus-alive-p)
        (progn
          (when (message-fetch-field "Gcc")
            (message-remove-header "Gcc"))
          (message-add-header "Gcc: nnmaildir+pub:Sent"))
      (message "Gnus is not running. No GCC field inserted.")))

  (add-hook 'message-header-setup-hook #'prot/message-header-add-gcc)
  (add-hook 'message-setup-hook #'message-sort-headers))

(prot-emacs-builtin-package 'gnus-dired ; does not require `gnus'
  (add-hook 'dired-mode-hook #'gnus-dired-mode))

(prot-emacs-builtin-package 'prot-mail
  ;; NOTE 2021-05-14: This is a generic indicator for new mail in the
  ;; maildir.  As I now use notmuch (see relevant section in this
  ;; document) I have an alternative approach in prot-notmuch.el.
  (setq prot-mail-maildir-path-regexp "~/Mail/*/INBOX/new/") ; shell regexp
  (setq prot-mail-mode-line-indicator-commands
        '(notmuch-refresh-this-buffer))
  ;; mode line indicator with the number of new mails
  (prot-mail-mail-indicator -1))

;;; Notmuch
(prot-emacs-elpa-package 'notmuch

;;;; Account settings
  (setq notmuch-identities '("David Porter <david@daporter.net>"))
  (setq notmuch-fcc-dirs "Sent")

;;;; General UI
  (setq notmuch-show-logo nil)
  (setq notmuch-column-control t)
  (setq notmuch-hello-auto-refresh t)
  (setq notmuch-hello-recent-searches-max 20)
  (setq notmuch-hello-thousands-separator "")
  ;; ;; See my variant of it in `prot-notmuch' below.
  ;; (setq notmuch-hello-sections '(notmuch-hello-insert-saved-searches))
  (setq notmuch-show-all-tags-list nil)

;;;; Search
  (setq notmuch-search-oldest-first nil)
  (setq notmuch-search-result-format
        '(("date" . "%12s  ")
          ("count" . "%-7s  ")
          ("authors" . "%-20s  ")
          ("subject" . "%-80s  ")
          ("tags" . "(%s)")))
  (setq notmuch-tree-result-format
        '(("date" . "%12s  ")
          ("authors" . "%-20s  ")
          ((("tree" . "%s")
            ("subject" . "%s"))
           . " %-80s  ")
          ("tags" . "(%s)")))
  (setq notmuch-search-line-faces
        '(("unread" . notmuch-search-unread-face)
          ("flag" . notmuch-search-flagged-face)))
  (setq notmuch-show-empty-saved-searches t)
  (setq notmuch-saved-searches
        `(( :name "unread (inbox)"
            :query "tag:unread and tag:inbox"
            :sort-order oldest-first
            :key ,(kbd "u"))
          ( :name "unread all"
            :query "tag:unread not tag:archived"
            :sort-order oldest-first
            :key ,(kbd "U"))
          ( :name "todo"
            :query "tag:todo not tag:archived"
            :sort-order newest-first
            :key ,(kbd "t"))
          ( :name "references"
            :query "tag:ref not tag:archived"
            :sort-order newest-first
            :key ,(kbd "r"))
          ( :name "inbox"
            :query "tag:inbox"
            :sort-order newest-first
            :key ,(kbd "i"))
          ( :name "mailing lists"
            :query "tag:list not tag:archived"
            :sort-order newest-first
            :key ,(kbd "m"))
          ;; Emacs
          ( :name "emacs-humanities"
            :query "(from:emacs-humanities@gnu.org or to:emacs-humanities@gnu.org) not tag:archived"
            :sort-order newest-first
            :key ,(kbd "e h"))
          ;; Books
          ( :name "great-conversation"
            :query "(from:great-conversation@googlegroups.com or from:GreatConversation@yahoogroups.com or to:GreatConversation@yahoogroups.com or to:great-conversation@googlegroups.com) not tag:archived"
            :sort-order newest-first
            :key ,(kbd "b g"))))

;;;; Tags
  (setq notmuch-archive-tags '("-inbox" "+archived"))
  (setq notmuch-message-replied-tags '("+replied"))
  (setq notmuch-message-forwarded-tags '("+forwarded"))
  (setq notmuch-show-mark-read-tags '("-unread"))
  (setq notmuch-draft-tags '("+draft"))
  (setq notmuch-draft-folder "Drafts")
  (setq notmuch-draft-save-plaintext 'ask)
  (setq notmuch-tagging-keys
        `((,(kbd "a") notmuch-archive-tags "Archive (remove from inbox)")
          (,(kbd "c") ("+archived" "-inbox" "-list" "-todo" "-ref" "-unread") "Complete and archive")
          (,(kbd "d") ("+deleted" "-inbox" "-archived" "-unread") "Mark for deletion")
          (,(kbd "f") ("+flag" "-unread") "Flag as important")
          ;; (,(kbd "r") notmuch-show-mark-read-tags "Mark as read")
          (,(kbd "r") ("+ref" "-unread") "Reference for the future")
          (,(kbd "s") ("+spam" "+deleted" "-inbox" "-unread") "Mark as spam")
          (,(kbd "t") ("+todo" "-unread") "To-do")
          (,(kbd "u") ("+unread") "Mark as unread")))
  (setq notmuch-tag-formats
        '(("unread" (propertize tag 'face 'notmuch-tag-unread))
          ("flag" (propertize tag 'face 'notmuch-tag-flagged))))
  (setq notmuch-tag-deleted-formats
        '(("unread" (notmuch-apply-face bare-tag `notmuch-tag-deleted))
          (".*" (notmuch-apply-face tag `notmuch-tag-deleted))))

;;;; Email composition
  (setq notmuch-mua-compose-in 'current-window)
  (setq notmuch-mua-hidden-headers nil) ; TODO 2021-05-12: Review hidden headers
  (setq notmuch-address-command nil)    ; FIXME 2021-05-13: Make it work with EBDB
  (setq notmuch-always-prompt-for-sender t)
  (setq notmuch-mua-cite-function 'message-cite-original-without-signature)
  (setq notmuch-mua-reply-insert-header-p-function 'notmuch-show-reply-insert-header-p-never)
  (setq notmuch-mua-user-agent-function #'notmuch-mua-user-agent-full)
  (setq notmuch-maildir-use-notmuch-insert t)
  (setq notmuch-crypto-process-mime t)
  (setq notmuch-crypto-get-keys-asynchronously t)
  (setq notmuch-mua-attachment-regexp   ; see `notmuch-mua-send-hook'
        (concat "\\b\\(attache\?ment\\|attached\\|attach\\|"
                "pi[èe]ce\s+jointe?\\|"
                "συνημμ[εέ]νο\\|επισυν[αά]πτω\\)\\b"))

;;;; Reading messsages
  (setq notmuch-show-relative-dates t)
  (setq notmuch-show-all-multipart/alternative-parts nil)
  (setq notmuch-show-indent-messages-width 0)
  (setq notmuch-show-indent-multipart nil)
  (setq notmuch-show-part-button-default-action 'notmuch-show-save-part)
  (setq notmuch-show-text/html-blocked-images ".") ; block everything
  (setq notmuch-wash-citation-lines-prefix 6)
  (setq notmuch-wash-citation-lines-suffix 6)
  (setq notmuch-wash-wrap-lines-length 100)
  (setq notmuch-unthreaded-show-out nil)
  (setq notmuch-message-headers '("To" "Cc" "Subject" "Date"))
  (setq notmuch-message-headers-visible t)

;;;; Hooks and key bindings
  (add-hook 'notmuch-mua-send-hook #'notmuch-mua-attachment-check)
  (remove-hook 'notmuch-show-hook #'notmuch-show-turn-on-visual-line-mode)
  (add-hook 'notmuch-show-hook (lambda () (setq-local header-line-format nil)))

  (let ((map global-map))
    (define-key map (kbd "C-c m") #'notmuch)
    (define-key map (kbd "C-x m") #'notmuch-mua-new-mail)) ; override `compose-mail'
  (let ((map notmuch-search-mode-map))
    (define-key map (kbd "/") #'notmuch-search-filter)))

(prot-emacs-builtin-package 'prot-notmuch
  (setq prot-notmuch-search-field-width 100)
  (setq notmuch-hello-sections '(prot-notmuch-hello-insert-saved-searches
                                 ;; prot-notmuch-hello-insert-recent-searches
                                 ))

  (add-to-list 'notmuch-tag-formats
               '("encrypted" (propertize tag 'face 'prot-notmuch-encrypted-tag)))
  (add-to-list 'notmuch-tag-formats
               '("sent" (propertize tag 'face 'prot-notmuch-sent-tag)))
  (add-to-list 'notmuch-tag-formats
               '("ref" (propertize tag 'face 'prot-notmuch-ref-tag)))
  (add-to-list 'notmuch-tag-formats
               '("todo" (propertize tag 'face 'prot-notmuch-todo-tag)))
  (add-to-list 'notmuch-tag-formats
               '("spam" (propertize tag 'face 'prot-notmuch-spam-tag)))

  ;; NOTE 2021-05-14: I have an alternative method of finding new mail
  ;; in a maildir tree by using the find command.  It is somewhat
  ;; simplistic, though it worked just fine: see prot-mail.el.  I prefer
  ;; this implementation instead, as it leverages notmuch and so I can
  ;; pass arbitrary search terms to it.
  (setq prot-notmuch-mode-line-count-args "tag:unread and tag:inbox")
  (setq prot-notmuch-mode-line-indicator-commands
        '(notmuch notmuch-refresh-this-buffer))
  ;; Mode line indicator with the number of new mails.
  (prot-notmuch-mail-indicator 1)

  (add-hook 'notmuch-hello-mode-hook #'prot-notmuch-widget-field-face-remap)

  (define-key notmuch-search-mode-map (kbd "g")
    #'prot-notmuch-refresh-buffer))

(prot-emacs-builtin-package 'smtpmail
  (setq smtpmail-default-smtp-server "smtp.migadu.com")
  (setq smtpmail-smtp-server "smtp.migadu.com")
  (setq smtpmail-smtp-user "david@daporter.net")
  (setq smtpmail-smtp-service 465)
  (setq smtpmail-stream-type 'ssl)
  (setq smtpmail-queue-mail nil))

(prot-emacs-builtin-package 'sendmail
  (setq send-mail-function 'smtpmail-send-it))

(prot-emacs-elpa-package 'ebdb
  (require 'ebdb-message)
  (require 'ebdb-notmuch) ; FIXME 2021-05-13: does not activate the corfu-mode UI
  (setq ebdb-sources (locate-user-emacs-file "ebdb"))
  (setq ebdb-permanent-ignores-file (locate-user-emacs-file "ebdb-permanent-ignores"))

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

  (setq ebdb-use-diary nil)

  (with-eval-after-load 'prot-mail ; check my `prot-mail.el'
    (add-hook 'message-setup-hook #'prot-mail-ebdb-message-setup))

  (let ((map ebdb-mode-map))
    (define-key map (kbd "D") #'ebdb-delete-field-or-record)
    (define-key map (kbd "M") #'ebdb-mail) ; disables `ebdb-mail-each'
    (define-key map (kbd "m") #'ebdb-toggle-record-mark)
    (define-key map (kbd "t") #'ebdb-toggle-all-record-marks)
    (define-key map (kbd "T") #'ebdb-toggle-records-format) ; disables `ebdb-toggle-all-records-format'
    (define-key map (kbd "U") #'ebdb-unmark-all-records)))


(prot-emacs-elpa-package 'elfeed
  (setq elfeed-use-curl t)
  (setq elfeed-curl-max-connections 10)
  (setq elfeed-db-directory (concat user-emacs-directory "elfeed/"))
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
            (lambda () (setq-local shr-width (current-fill-column))))

  (define-key global-map (kbd "C-c e") #'elfeed)
  (let ((map elfeed-search-mode-map))
    (define-key map (kbd "w") #'elfeed-search-yank)
    (define-key map (kbd "g") #'elfeed-update)
    (define-key map (kbd "G") #'elfeed-search-update--force)
    (define-key map (kbd "b") #'prot-elfeed-bongo-insert-item)
    (define-key map (kbd "h") #'prot-elfeed-bongo-switch-to-playlist)) ; "hop" mnemonic
  (let ((map elfeed-show-mode-map))
    (define-key map (kbd "w") #'elfeed-show-yank)
    (define-key map (kbd "b") #'prot-elfeed-bongo-insert-item)))

(with-eval-after-load 'elfeed
  (prot-emacs-builtin-package 'prot-elfeed
    (setq prot-elfeed-tag-faces t)
    (prot-elfeed-fontify-tags)
    (add-hook 'elfeed-search-mode-hook #'prot-elfeed-load-feeds)
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
      (define-key map (kbd "+") #'prot-elfeed-toggle-tag))))

(prot-emacs-elpa-package 'proced
  (setq proced-auto-update-flag t)
  (setq proced-auto-update-interval 1)
  (setq proced-descend t)
  (setq proced-filter 'user))

(prot-emacs-elpa-package 'password-store
  (setq password-store-time-before-clipboard-restore 30))

(prot-emacs-elpa-package 'pass)

(prot-emacs-builtin-package 'browse-url
  (setq browse-url-browser-function 'eww-browse-url)
  (setq browse-url-secondary-browser-function 'browse-url-default-browser))

(prot-emacs-builtin-package 'shr
  (setq shr-use-colors nil)             ; t is bad for accessibility
  (setq shr-use-fonts nil)              ; t is not for me
  (setq shr-max-image-proportion 0.6)
  (setq shr-image-animate nil)          ; No GIFs, thank you!
  (setq shr-width nil)                  ; check `prot-eww-readable'
  (setq shr-discard-aria-hidden t)
  (setq shr-cookie-policy nil))

(prot-emacs-builtin-package 'eww
  (setq eww-restore-desktop t)
  (setq eww-desktop-remove-duplicates t)
  (setq eww-header-line-format nil)
  (setq eww-search-prefix "https://duckduckgo.com/html/?q=")
  (setq eww-download-directory (expand-file-name "~/Downloads/eww"))
  (setq eww-suggest-uris
        '(eww-links-at-point
          thing-at-point-url-at-point))
  (setq eww-bookmarks-directory (locate-user-emacs-file "eww-bookmarks/"))
  (setq eww-history-limit 150)
  (setq eww-use-external-browser-for-content-type
        "\\`\\(video/\\|audio\\)") ; On GNU/Linux check your mimeapps.list
  (setq eww-browse-url-new-window-is-tab nil)
  (setq eww-form-checkbox-selected-symbol "[X]")
  (setq eww-form-checkbox-symbol "[ ]")
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
  (define-key eww-bookmark-mode-map (kbd "d") #'eww-bookmark-kill)) ; same

(prot-emacs-builtin-package 'prot-eww
  (define-prefix-command 'prot-eww-map)
  (define-key global-map (kbd "C-c w") 'prot-eww-map)
  (define-key global-map (kbd "s-w") 'prot-eww-map)
  (let ((map prot-eww-map))
    (define-key map (kbd "b") #'prot-eww-visit-bookmark)
    (define-key map (kbd "e") #'prot-eww-browse-dwim)
    (define-key map (kbd "a") #'prot-eww-search-arch-wiki)
    (define-key map (kbd "A") #'prot-eww-search-arch-aur)
    (define-key map (kbd "d") #'prot-eww-search-debbugs)
    (define-key map (kbd "w") #'prot-eww-search-wikipedia)
    (define-key map (kbd "s") #'prot-eww-search-engine))
  (let ((map eww-mode-map))
    (define-key map (kbd "B") #'prot-eww-bookmark-page)
    (define-key map (kbd "D") #'prot-eww-download-html)
    (define-key map (kbd "F") #'prot-eww-find-feed)
    (define-key map (kbd "b") #'prot-eww-visit-bookmark)
    (define-key map (kbd "e") #'prot-eww-browse-dwim)
    (define-key map (kbd "o") #'prot-eww-open-in-other-window)
    (define-key map (kbd "E") #'prot-eww-visit-url-on-page)
    (define-key map (kbd "J") #'prot-eww-jump-to-url-on-page)
    (define-key map (kbd "R") #'prot-eww-readable)))

(prot-emacs-elpa-package 'pdf-tools
  (setq pdf-tools-enabled-modes         ; simplified from the defaults
        '(pdf-history-minor-mode
          pdf-isearch-minor-mode
          pdf-links-minor-mode
          pdf-outline-minor-mode
          pdf-misc-size-indication-minor-mode
          pdf-occur-global-minor-mode))
  (setq pdf-view-display-size 'fit-height)
  (setq pdf-view-continuous t)
  (setq pdf-view-use-dedicated-register nil)
  (setq pdf-view-max-image-width 1080)
  (setq pdf-outline-imenu-use-flat-menus t)

  (pdf-loader-install)

  ;; Those functions and hooks are adapted from the manual of my
  ;; modus-themes.  The idea is to (i) add a backdrop that is distinct
  ;; from the background of the PDF's page and (ii) make pdf-tools adapt
  ;; to theme switching via, e.g., `modus-themes-toggle'.
  (defun prot/pdf-tools-backdrop ()
    (face-remap-add-relative
     'default
     `(:background ,(modus-themes-color 'bg-alt))))

  (defun prot/pdf-tools-midnight-mode-toggle ()
    (when (derived-mode-p 'pdf-view-mode)
      (if (eq (car custom-enabled-themes) 'modus-vivendi)
          (pdf-view-midnight-minor-mode 1)
        (pdf-view-midnight-minor-mode -1))
      (prot/pdf-tools-backdrop)))

  (add-hook 'pdf-tools-enabled-hook #'prot/pdf-tools-midnight-mode-toggle)
  (add-hook 'modus-themes-after-load-theme-hook #'prot/pdf-tools-midnight-mode-toggle))

(prot-emacs-elpa-package 'beginend
  (beginend-global-mode 1))

(prot-emacs-elpa-package 'goto-last-change
  (define-key global-map (kbd "C-z") #'goto-last-change))

(setq mode-line-percent-position '(-3 "%p"))
(setq mode-line-position-column-line-format '(" %l,%c")) ; Emacs 28
(setq mode-line-defining-kbd-macro
      (propertize " Macro" 'face 'mode-line-emphasis))

;; Thanks to Daniel Mendler for this!  It removes the square brackets
;; that denote recursive edits in the modeline.  I do not need them
;; because I am using Daniel's `recursion-indicator':
;; <https://github.com/minad/recursion-indicator>.
(setq-default mode-line-modes
              (seq-filter (lambda (s)
                            (not (and (stringp s)
                                      (string-match-p
                                       "^\\(%\\[\\|%\\]\\)$" s))))
                          mode-line-modes))

(setq mode-line-compact nil)            ; Emacs 28
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

(prot-emacs-elpa-package 'moody)

(prot-emacs-builtin-package 'prot-moody
  ;; Addjust this and then evaluate `prot-moody-set-height'.  Not all
  ;; fonts work equally well with the same value.
  (setq prot-moody-font-height-multiplier 1.35)

  ;; Also check the Modus themes' `modus-themes-mode-line' which can set
  ;; the styles specifically for Moody.
  (prot-moody-set-height -1))

(prot-emacs-elpa-package 'minions
  (setq minions-mode-line-lighter ";")
  ;; NOTE: This will be expanded whenever I find a mode that should not
  ;; be hidden
  (setq minions-direct (list 'defining-kbd-macro
                             'flymake-mode
                             'prot-simple-monocle))
  (minions-mode 1))

(prot-emacs-elpa-package 'recursion-indicator
  (setq recursion-indicator-general "&")
  (setq recursion-indicator-minibuffer "@")
  (recursion-indicator-mode 1))

(prot-emacs-builtin-package 'battery
  (setq battery-mode-line-format " [%b%p%%]")
  (setq battery-mode-line-limit 95)
  (setq battery-update-interval 180)
  (setq battery-load-low 20)
  (setq battery-load-critical 10)
  (add-hook 'after-init-hook #'display-battery-mode))

(prot-emacs-builtin-package 'time
  (setq display-time-format "%H:%M  %Y-%m-%d")
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

;;; World clock
  (setq zoneinfo-style-world-list
        '(("America/Los_Angeles" "Los Angeles")
          ("America/New_York" "New York")
          ("Europe/Amsterdam" "Amsterdam")
          ("Asia/Shanghai" "Shanghai")
          ("Asia/Tokyo" "Tokyo")
          ("Australia/Brisbane" "Brisbane")
          ("Australia/Sydney" "Sydney")))

  ;; All of the following variables are for Emacs 28
  (setq world-clock-list t)
  (setq world-clock-time-format "%R %z  %A %d %B")
  (setq world-clock-buffer-name "*world-clock*") ; Placement handled by `display-buffer-alist'
  (setq world-clock-timer-enable t)
  (setq world-clock-timer-second 60)

                                        ;(add-hook 'after-init-hook #'display-time-mode)
  )

(setq window-divider-default-right-width 1)
(setq window-divider-default-bottom-width 1)
(setq window-divider-default-places 'right-only)
(add-hook 'after-init-hook #'window-divider-mode)

(prot-emacs-builtin-package 'fringe
  (fringe-mode nil)
  (setq-default fringes-outside-margins nil)
  (setq-default indicate-buffer-boundaries nil)
  (setq-default indicate-empty-lines nil)
  (setq-default overflow-newline-into-fringe t))

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

  (require 'whitespace)

  (let ((map global-map))
    (define-key map (kbd "<f6>") #'prot-sideline-negative-space-toggle)
    (define-key map (kbd "<f7>") #'prot-sideline-mode)
    (define-key map (kbd "C-c w") #'delete-trailing-whitespace)))

(prot-emacs-builtin-package 'outline
  (setq-default outline-minor-mode-highlight 'override) ; emacs28
  (setq-default outline-minor-mode-cycle t)     ; emacs28
  (let ((map outline-minor-mode-map))
    (define-key map (kbd "C-<tab>") #'outline-cycle)
    (define-key map (kbd "<backtab>") #'outline-cycle-buffer) ; S-TAB
    (define-key map (kbd "C-c C-n") #'outline-next-visible-heading)
    (define-key map (kbd "C-c C-p") #'outline-previous-visible-heading)
    (define-key map (kbd "C-c C-f") #'outline-forward-same-level)
    (define-key map (kbd "C-c C-b") #'outline-backward-same-level)
    (define-key map (kbd "C-c C-a") #'outline-show-all)
    (define-key map (kbd "C-c C-o") #'outline-hide-other)
    (define-key map (kbd "C-c C-u") #'outline-up-heading)))

(prot-emacs-builtin-package 'prot-outline
  (let ((map outline-minor-mode-map))
    (define-key map (kbd "C-c C-v") #'prot-outline-move-major-heading-down)
    (define-key map (kbd "M-<down>") #'prot-outline-move-major-heading-down)
    (define-key map (kbd "C-c M-v") #'prot-outline-move-major-heading-up)
    (define-key map (kbd "M-<up>") #'prot-outline-move-major-heading-up)
    (define-key map (kbd "C-x n s") #'prot-outline-narrow-to-subtree))
  (define-key global-map (kbd "<f10>") #'prot-outline-minor-mode-safe))

(prot-emacs-builtin-package 'prot-cursor
  (prot-cursor-presentation-mode -1))

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

(setq-default scroll-preserve-screen-position t)
(setq-default scroll-conservatively 1) ; affects `scroll-step'
(setq-default scroll-margin 0)
(setq-default next-screen-context-lines 0)

(prot-emacs-builtin-package 'delsel
  (add-hook 'after-init-hook #'delete-selection-mode))

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

(prot-emacs-builtin-package 'time-stamp
  (add-hook 'before-save-hook #'time-stamp))

(prot-emacs-builtin-package 'autorevert
  (setq auto-revert-verbose t)
  (add-hook 'after-init-hook #'global-auto-revert-mode))

(setq save-interprogram-paste-before-kill t)

(prot-emacs-elpa-package 'goggles
  (setq-default goggles-pulse t)
  (dolist (mode '(prog-mode-hook text-mode-hook))
    (add-hook mode #'goggles-mode)))

(setq mode-require-final-newline 'visit-save)

(setq repeat-on-final-keystroke t)
(setq set-mark-command-repeat-pop t)
(define-key global-map (kbd "M-z") #'zap-up-to-char)
(define-key global-map (kbd "s-z") #'repeat)

(prot-emacs-builtin-package 'package
  ;; All variables are for Emacs 28+
  (setq package-name-column-width 40)
  (setq package-version-column-width 14)
  (setq package-status-column-width 12)
  (setq package-archive-column-width 8)
  (add-hook 'package-menu-mode-hook #'hl-line-mode))

(prot-emacs-builtin-package 'text-mode)

(prot-emacs-builtin-package 'prot-text
  (add-to-list 'auto-mode-alist '("\\(README\\|CHANGELOG\\|COPYING\\|LICENSE\\)$" . text-mode))
  (define-key text-mode-map (kbd "<M-return>") #'prot-text-insert-heading)
                                        ;(define-key org-mode-map (kbd "M-;") nil)
  )

(prot-emacs-elpa-package 'markdown-mode
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
  (setq markdown-fontify-code-blocks-natively t))
;; Allows for fenced block focus with C-c ' (same as Org blocks).
(prot-emacs-elpa-package 'edit-indirect)

(prot-emacs-elpa-package 'yaml-mode
  (add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode)))

(prot-emacs-builtin-package 'css-mode
  (add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
  (add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
  (setq css-fontify-colors nil))

(prot-emacs-builtin-package 'sh-script
  (add-to-list 'auto-mode-alist '("PKGBUILD" . sh-mode)))

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
  (add-hook 'after-init-hook #'column-number-mode))

(prot-emacs-builtin-package 'newcomment
  (setq comment-empty-lines t)
  (setq comment-fill-column nil)
  (setq comment-multi-line t)
  (setq comment-style 'multi-line)
  (let ((map global-map))
    (define-key map (kbd "C-:") #'comment-kill)         ; C-S-;
    (define-key map (kbd "M-;") #'comment-indent)))

(prot-emacs-builtin-package 'prot-comment
  (setq prot-comment-comment-keywords
        '("TODO" "NOTE" "XXX" "REVIEW" "FIXME"))
  (let ((map global-map))
    (define-key map (kbd "C-;") #'prot-comment-comment-dwim)
    (define-key map (kbd "C-x C-;") #'prot-comment-timestamp-keyword)))

(prot-emacs-builtin-package 'electric
  (setq electric-pair-inhibit-predicate'electric-pair-conservative-inhibit)
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
  (electric-indent-mode 1)
  (electric-pair-mode -1)
  (electric-quote-mode -1))

(prot-emacs-builtin-package 'paren
  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-in-periphery nil)
  (setq show-paren-when-point-inside-paren nil)
  (add-hook 'after-init-hook #'show-paren-mode))

(setq-default tab-always-indent 'complete)
(setq-default tab-first-completion 'word-or-paren-or-punct) ; Emacs 27
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(prot-emacs-builtin-package 'flyspell
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  (setq ispell-program-name "aspell")
  (setq ispell-dictionary "en_GB")
  (define-key flyspell-mode-map (kbd "C-;") nil))

(prot-emacs-builtin-package 'prot-spell
  (setq prot-spell-dictionaries
        '(("EN English" . "en")
          ("FR Français" . "fr")
          ("NL Nederlands" . "nl")
          ("ES Espanõl" . "es")))
  (let ((map global-map))
    (define-key map (kbd "M-$") #'prot-spell-spell-dwim)
    (define-key map (kbd "C-M-$") #'prot-spell-change-dictionary)))

(prot-emacs-builtin-package 'flymake
  (setq flymake-fringe-indicator-position 'left-fringe)
  (setq flymake-suppress-zero-counters t)
  (setq flymake-start-on-flymake-mode t)
  (setq flymake-no-changes-timeout nil)
  (setq flymake-start-on-save-buffer t)
  (setq flymake-proc-compilation-prevents-syntax-check t)
  (setq flymake-wrap-around nil)
  (let ((map flymake-mode-map))
    (define-key map (kbd "C-c ! s") #'flymake-start)
    (define-key map (kbd "C-c ! d") #'flymake-show-diagnostics-buffer)
    (define-key map (kbd "C-c ! n") #'flymake-goto-next-error)
    (define-key map (kbd "C-c ! p") #'flymake-goto-prev-error))

  (add-hook 'markdown-mode-hook #'flymake-mode))

(prot-emacs-elpa-package 'flymake-diagnostic-at-point
  (setq flymake-diagnostic-at-point-display-diagnostic-function
        'flymake-diagnostic-at-point-display-minibuffer))

(prot-emacs-elpa-package 'flymake-proselint
  (add-hook 'markdown-mode-hook #'flymake-proselint-setup)
  (add-hook 'org-mode-hook #'flymake-proselint-setup)
  (add-hook 'text-mode-hook #'flymake-proselint-setup))

(prot-emacs-builtin-package 'eldoc
  (global-eldoc-mode 1))

(prot-emacs-builtin-package 'man
  (let ((map Man-mode-map))
    (define-key map (kbd "i") #'Man-goto-section)
    (define-key map (kbd "g") #'Man-update-manpage)))

(prot-emacs-builtin-package 'server
  (add-hook 'after-init-hook #'server-start))

(prot-emacs-builtin-package 'desktop
  (setq desktop-auto-save-timeout 300)
  (setq desktop-path `(,user-emacs-directory))
  (setq desktop-base-file-name "desktop")
  (setq desktop-files-not-to-save nil)
  (setq desktop-globals-to-clear nil)
  (setq desktop-load-locked-desktop t)
  (setq desktop-missing-file-warning nil)
  (setq desktop-restore-eager 0)
  (setq desktop-restore-frames nil)
  (setq desktop-save 'ask-if-new)
  (dolist (symbol '(kill-ring log-edit-comment-ring))
    (add-to-list 'desktop-globals-to-save symbol))

  (desktop-save-mode 1))

(prot-emacs-builtin-package 'savehist
  (setq savehist-file (locate-user-emacs-file "savehist"))
  (setq history-length 1000)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (add-hook 'after-init-hook #'savehist-mode))

(prot-emacs-builtin-package 'saveplace
  (setq save-place-file (locate-user-emacs-file "saveplace"))
  (setq save-place-forget-unreadable-files t)
  (save-place-mode 1))

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backup/"))))
(setq backup-by-copying t)
(setq version-control t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq create-lockfiles nil)

;; My configuration.

(prot-emacs-elpa-package 'auth-source-pass
  (auth-source-pass-enable))
(setq user-full-name "David Porter")
(setq user-mail-address "david@daporter.net")

(prot-emacs-builtin-package 'org-journal
  (setq org-journal-dir "~/Documents/journal")
  (setq org-journal-file-format "%Y-%m-%d")
  (setq org-journal-date-format "%A, %d %B %Y"))

(prot-emacs-elpa-package 'deft
  (setq deft-extensions '("md" "txt" "org"))
  (setq deft-default-extension "md")
  (setq deft-directory "~/Documents/notes")
  (setq deft-use-filename-as-title t))

(prot-emacs-elpa-package 'zetteldeft
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

  ;; Zetteldeft doesn't include the ID in the titles it generates. The
  ;; following function inserts a title that does include the ID.
  (defun dp-zetteldeft-insert-title ()
    "Insert a title based on the file name."
    (interactive)
    (let ((title (file-name-base (buffer-file-name))))
      (save-excursion
        (goto-char (point-min))
        (zetteldeft--insert-title title)
        (newline)))))

(prot-emacs-elpa-package 'god-mode
  (global-set-key (kbd "<escape>") #'god-local-mode)
  (global-set-key (kbd "C-x C-1") #'delete-other-windows)
  (global-set-key (kbd "C-x C-2") #'split-window-below)
  (global-set-key (kbd "C-x C-3") #'split-window-right)
  (global-set-key (kbd "C-x C-0") #'delete-window)
  (define-key god-local-mode-map (kbd ".") #'repeat)

  (require 'god-mode-isearch)
  (define-key isearch-mode-map (kbd "<escape>") #'god-mode-isearch-activate)
  (define-key god-mode-isearch-map (kbd "<escape>") #'god-mode-isearch-disable)

  (defun my-god-mode-update-modeline ()
    (if (bound-and-true-p god-local-mode)
        (progn
          (setq cursor-type 'box)
          (set-face-background 'mode-line
                               (modus-themes-color 'bg-active)))
      (progn
        (setq cursor-type 'bar)
        (set-face-background 'mode-line
                             (modus-themes-color 'red-subtle-bg)))))

  (add-hook 'god-mode-enabled-hook #'my-god-mode-update-modeline)
  (add-hook 'god-mode-disabled-hook #'my-god-mode-update-modeline)

  (god-mode))

(prot-emacs-builtin-package 'reftex
  (setq reftex-default-bibliography
        '("~/Documents/bibliography/references.bib"))

  (defun dp/reftex-citation ()
    (interactive)
    (let ((reftex-cite-format
           '((?\C-m . "%l")
             ;; MLA citation style
             (?f . "[#%l]: %a. *%t*. %d, %u, %y, %p %<."))))
      (reftex-citation))))

(prot-emacs-builtin-package 'bibtex
  (setq bibtex-dialect 'biblatex)
  (setq bibtex-align-at-equal-sign t)
  (setq bibtex-autokey-name-year-separator "")
  (setq bibtex-autokey-year-title-separator "")
  (setq bibtex-autokey-titleword-first-ignore '("the" "a" "if" "and" "an"))
  (setq bibtex-autokey-titleword-length 30)
  (setq bibtex-autokey-titlewords 1)
  (setq bibtex-autokey-titlewords-stretch 0))

(prot-emacs-elpa-package 'ebib
  (setq ebib-bibtex-dialect bibtex-dialect)
  (setq ebib-preload-bib-files reftex-default-bibliography)
  (add-to-list 'ebib-reference-templates
               '("Book" . "{Author|Editor} ({Date|Year}). {\"Title\".} {Publisher.} {Doi|Url.}")))

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

(let ((map text-mode-map))
  (define-key map (kbd "C-c z b") #'dp-insert-zotero-bibliography)
  (define-key map (kbd "C-c z c") #'dp-insert-zotero-citation))

(prot-emacs-elpa-package 'debian-el)

(global-set-key (kbd "C-c p") #'package-list-packages)

(prot-emacs-builtin-package 'titlecase
  (global-set-key (kbd "C-c t") #'titlecase-dwim))

(prot-emacs-elpa-package 'pandoc-mode
  (add-hook 'markdown-mode-hook #'pandoc-mode)
  (add-hook 'pandoc-mode-hook #'pandoc-load-default-settings)
  (add-hook 'pandoc-async-success-hook #'pandoc-view-output))

(prot-emacs-builtin-package 'flymake-markdownlint
  (add-hook 'markdown-mode-hook #'flymake-markdownlint-setup))

(provide 'init)
;;; init.el ends here

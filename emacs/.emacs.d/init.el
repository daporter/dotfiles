;; Emacs Configuration
;; =============================================================================

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(defvar prot-emacs-ensure-install nil
  "List of package names to install, if missing.")

(defun prot-emacs-install-ensured ()
  "Install all `prot-emacs-ensure-install' packages, if needed."
  (interactive)
  (package-refresh-contents)
  (mapcar (lambda (package)
            (unless (package-installed-p package)
              (package-install package)))
          prot-emacs-ensure-install))

(require 'vc)
(setq vc-follow-symlinks t) ; Because my dotfiles are managed that way

;; For my custom libraries
(add-to-list 'load-path (concat user-emacs-directory "prot-lisp/"))

;; For other libraries
(add-to-list 'load-path (concat user-emacs-directory "contrib-lisp/"))

;; Some basic settings
(setq frame-title-format '("%b"))
(setq ring-bell-function 'ignore)

(defalias 'yes-or-no-p 'y-or-n-p)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'overwrite-mode 'disabled t)

(setq initial-buffer-choice t)			; always start with *scratch*

(require 'prot-common)

(require 'prot-simple)
(with-eval-after-load 'prot-simple
  (setq prot-simple-insert-pair-alist
	    '(("' Single quote" . (39 39))           ; ' '
	      ("\" Double quotes" . (34 34))         ; " "
	      ("` Elisp quote" . (96 39))            ; ` '
	      ("‘ Single apostrophe" . (8216 8217))  ; ‘ ’
	      ("“ Double apostrophes" . (8220 8221)) ; “ ”
	      ("( Parentheses" . (40 41))            ; ( )
	      ("{ Curly brackets" . (123 125))       ; { }
	      ("[ Square brackets" . (91 93))        ; [ ]
	      ("< Angled brackets" . (60 62))        ; < >
	      ("« Εισαγωγικά Gr quote" . (171 187))  ; « »
	      ("= Equals signs" . (61 61))           ; = =
	      ("* Asterisks" . (42 42))              ; * *
	      ("_ underscores" . (95 95))))          ; _ _
  (setq prot-simple-date-specifier "%F")
  (setq prot-simple-time-specifier "%R %z")

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
    (define-key map (kbd "C-=") #'prot-simple-inset-date)
    (define-key map (kbd "C-'") #'prot-simple-insert-pair-completion)
    (define-key map (kbd "M-'") #'prot-simple-insert-pair-completion)
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

(require 'prot-pulse)
(with-eval-after-load 'prot-pulse
  (setq prot-pulse-pulse-command-list
        '(recenter-top-bottom
          reposition-window))
  (prot-pulse-advice-commands-mode 1)
  (define-key global-map (kbd "<s-escape>") #'prot-pulse-pulse-line))

(require 'cus-edit)
(with-eval-after-load 'cus-edit
  ;; Disable the damn thing
  (setq custom-file (make-temp-file "emacs-custom-")))

(require 'modus-themes)
(with-eval-after-load 'modus-themes
  (add-to-list 'prot-emacs-ensure-install 'modus-themes)
  ;; Add all your customizations prior to loading the themes
  ;;
  ;; NOTE: these are not my preferences!  I am always testing various
  ;; configurations.  Though I still like what I have here.
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
  (modus-themes-load-operandi)
  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

(require 'prot-fonts)
(with-eval-after-load 'prot-fonts
  ;; Note that the light weight I pass to Iosevka Comfy is thicker than
  ;; the equivalent for standard Iosevka.  In my build instructions, I
  ;; set that to 350, while normal light is at 300 and regular is at
  ;; 400.  Source: <https://gitlab.com/protesilaos/iosevka-comfy>.
  (setq prot-fonts-typeface-sets-alist
        '((laptop 90 "Hack" normal "DejaVu Sans Condensed" normal)
          (desktop 130 "Iosevka Comfy" light "Roboto Condensed" normal)
          (reader 150 "Iosevka Comfy" light "FiraGO" normal)
          (presentation 180 "Iosevka Comfy" light "Source Sans Pro" normal)))
  (setq prot-fonts-monospaced-list
        '("Hack" "DejaVu Sans Mono" "Iosevka Comfy" "Source Code Pro"
          "Ubuntu Mono" "Fantasque Sans Mono" "Fira Code" "Monoid"))
  (setq prot-fonts-heights-list
        '(100 105 110 120 130 140 150 160 170 180 190))
  (setq prot-fonts-line-spacing-alist
        '(("Ubuntu Mono" . 2)))
  (setq prot-fonts-laptop-desktop-keys-list '(laptop desktop))
  (setq prot-fonts-max-small-resolution-width 1600)
  (setq prot-fonts-bold-weight-alist
        '(("Iosevka Comfy" . semibold)
          ("Fira Code" . semibold)
          ("Source Code Pro" . semibold)))
  ;; This is defined in Emacs' C code, though I feel this is a good
  ;; place to put it.
  (setq x-underline-at-descent-line t)
  ;; And this just sets the right font depending on whether my laptop is
  ;; connected to an external monitor or not.
  (prot-fonts-fonts-per-monitor)
  (add-hook 'prot-fonts-set-typeface-hook #'prot-fonts-line-spacing)
  (add-hook 'prot-fonts-set-typeface-hook #'prot-fonts-bold-face)
  ;; See theme section for this hook
  (add-hook 'modus-themes-after-load-theme-hook #'prot-fonts-bold-face)
  (define-key global-map (kbd "C-c f") #'prot-fonts-set-fonts-dwim))

(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

(require 'so-long)
(with-eval-after-load 'so-long
  (global-so-long-mode 1))

(require 'which-key)
(with-eval-after-load 'which-key
  (add-to-list 'prot-emacs-ensure-install 'which-key)
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

(require 'async)
(with-eval-after-load 'async
  (add-to-list 'prot-emacs-ensure-install 'async))

(require 'prot-orderless)
(with-eval-after-load 'prot-orderless
  (setq prot-orderless-default-styles
        '(orderless-prefixes
          orderless-literal
          orderless-strict-leading-initialism
          orderless-regexp
          orderless-flex))
  (setq prot-orderless-alternative-styles
        '(orderless-literal
          orderless-prefixes
          orderless-strict-leading-initialism
          orderless-regexp)))

(require 'orderless)
(with-eval-after-load 'orderless
  (add-to-list 'prot-emacs-ensure-install 'orderless)
  (setq orderless-component-separator " +")
  (setq orderless-matching-styles prot-orderless-default-styles)
  (setq orderless-style-dispatchers
        '(prot-orderless-literal-dispatcher
          prot-orderless-initialism-dispatcher))
  ;; SPC should never complete: use it for `orderless' groups.
  (let ((map minibuffer-local-completion-map))
    (define-key map (kbd "SPC") nil)
    (define-key map (kbd "?") nil)))

(require 'marginalia)
(with-eval-after-load 'marginalia
  (add-to-list 'prot-emacs-ensure-install 'marginalia)
  (setq marginalia-annotators
        '(marginalia-annotators-heavy
          marginalia-annotators-light))
  (marginalia-mode 1))

(require 'prot-minibuffer)
(with-eval-after-load 'prot-minibuffer
  (define-key global-map (kbd "s-v") #'prot-minibuffer-focus-mini-or-completions)
  (let ((map completion-list-mode-map))
    (define-key map (kbd "M-v") #'prot-minibuffer-focus-mini)
    (define-key map (kbd "h") #'prot-simple-describe-symbol) ; from `prot-simple.el'
    ;; Those are DE FACTO DEPRECATED generic actions for the
    ;; "*Completions*" buffer.  I normally use `embark' and its own
    ;; buffers.
    (define-key map (kbd "w") #'prot-minibuffer-completions-kill-symbol-at-point)
    (define-key map (kbd "i") #'prot-minibuffer-completions-insert-symbol-at-point)
    (define-key map (kbd "j") #'prot-minibuffer-completions-insert-symbol-at-point-exit))
  (add-hook 'minibuffer-setup-hook #'prot-minibuffer-mini-cursor))

(require 'minibuffer)
(with-eval-after-load 'minibuffer
  (setq completion-styles '(partial-completion substring flex orderless))
  (setq completion-category-defaults nil)
  (setq completion-cycle-threshold 3)
  (setq completion-flex-nospace nil)
  (setq completion-pcm-complete-word-inserts-delimiters t)
  (setq completion-pcm-word-delimiters "-_./:| ")
  (setq completion-show-help nil)
  (setq completion-auto-help nil)
  (setq completion-ignore-case t)
  (setq-default case-fold-search t)   ; For general regexp

  ;; The following two are updated in Emacs 28.  They concern the
  ;; *Completions* buffer.  Note that I actually do not use that buffer,
  ;; because I rely on Embark's version of it.
  (setq completions-format 'one-column)
  (setq completions-detailed t)

  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)

  (setq enable-recursive-minibuffers t)
  (setq read-answer-short t)
  (setq resize-mini-windows t)
  (setq minibuffer-eldef-shorten-default t)

  (setq echo-keystrokes 0.25)           ; from the C source code

  (file-name-shadow-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1)

  ;; Defines, among others, aliases for common minibuffer commands to
  ;; Super-KEY.  Normally these should go in individual package
  ;; declarations, but their grouping here makes things easier to
  ;; understand.  Besides, they are related to the minibuffer.
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
  ;; De facto deprecated as I use Embark and its own completions'
  ;; buffer.
  (let ((map completion-list-mode-map))
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "f") #'next-completion)
    (define-key map (kbd "b") #'previous-completion)))

(require 'consult)
(with-eval-after-load 'consult
  (add-to-list 'prot-emacs-ensure-install 'consult)
  (setq consult-line-numbers-widen t)
  (setq completion-in-region-function #'consult-completion-in-region)
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

  ;; Registers' setup -- From Consult's README
  ;;
  ;; This gives a consistent display for `consult-register',
  ;; `consult-register-load', `consult-register-store', and the Emacs
  ;; built-ins.
  (setq register-preview-delay 0.8
        register-preview-function #'consult-register-format)
  ;; Tweak the register preview window.
  ;; * Sort the registers
  ;; * Hide the mode line
  ;; * Resize the window, such that the contents fit exactly
  (advice-add #'register-preview :around
              (lambda (fun buffer &optional show-empty)
                (let ((register-alist (seq-sort #'car-less-than-car register-alist)))
                  (funcall fun buffer show-empty))
                (when-let (win (get-buffer-window buffer))
                  (with-selected-window win
                    (setq-local mode-line-format nil)
                    (setq-local window-min-height 1)
                    (fit-window-to-buffer)))))

  (let ((map global-map))
    (define-key map (kbd "C-x M-:") #'consult-complex-command)
    (define-key map (kbd "C-x M-m") #'consult-minor-mode-menu)
    (define-key map (kbd "C-x M-k") #'consult-kmacro)
    (define-key map (kbd "M-g g") #'consult-goto-line)
    (define-key map (kbd "M-g M-g") #'consult-goto-line)
    (define-key map (kbd "M-X") #'consult-mode-command)
    (define-key map (kbd "M-K") #'consult-keep-lines) ; M-S-k is similar to M-S-5 (M-%)
    (define-key map (kbd "M-F") #'consult-focus-lines) ; same principle
    (define-key map (kbd "M-s g") #'consult-grep)
    (define-key map (kbd "M-s m") #'consult-mark)
    (define-key map (kbd "C-x r r") #'consult-register) ; Use the register's prefix
    (define-key map (kbd "C-x r S") #'consult-register-store)
    (define-key map (kbd "C-x r L") #'consult-register-load)
    (define-key consult-narrow-map (kbd "?") #'consult-narrow-help)))

(require 'prot-consult)
(with-eval-after-load 'prot-consult
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
    (define-key map (kbd "M-s i") #'prot-consult-imenu)
    (define-key map (kbd "M-s f") #'prot-consult-fd)
    (define-key map (kbd "M-s s") #'prot-consult-outline)    ; M-s o is `occur'
    (define-key map (kbd "M-s y") #'prot-consult-yank)
    (define-key map (kbd "M-s l") #'prot-consult-line)))

(require 'embark)
(with-eval-after-load 'embark
  (add-to-list 'prot-emacs-ensure-install 'embark)
  ;; (with-eval-after-load 'prot-minibuffer)
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

  (setq embark-key-action-separator (propertize " · " 'face 'shadow))
  ;; Please don't read too much into the names of those faces.  Just
  ;; green and yellow.
  (setq embark-action-indicator
        (let ((act (propertize "Act" 'face 'success)))
          (cons act (concat act " on '%s'"))))
  (setq embark-become-indicator (propertize "Become" 'face 'warning))

  ;; ;; NOTE: I keep this around for when I do videos, otherwise I do not
  ;; ;; use it.  It requires `which-key' to display key hints.
  ;; (setq embark-action-indicator
  ;;       (lambda (map _target)
  ;;         (which-key--show-keymap "Embark" map nil nil 'no-paging)
  ;;         #'which-key--hide-popup-ignore-command)
  ;;       embark-become-indicator embark-action-indicator)
  (add-hook 'minibuffer-setup-hook #'embark-collect-completions-after-input)
  (add-hook 'embark-post-action-hook #'embark-collect--update-linked)
  (add-hook 'embark-collect-mode-hook #'prot-embark-completions-cursor)
  (define-key global-map (kbd "C-,") #'embark-act)
  (let ((map minibuffer-local-completion-map))
    (define-key map (kbd "C-,") #'embark-act)
    (define-key map (kbd "C->") #'embark-become)
    (define-key map (kbd "M-q") #'embark-collect-toggle-view)) ; parallel of `fill-paragraph'
  (let ((map embark-collect-mode-map))
    (define-key map (kbd "C-,") #'embark-act)
    (define-key map (kbd ",") #'embark-act)
    (define-key map (kbd "M-q") #'embark-collect-toggle-view))
  (let ((map embark-symbol-map))
    (define-key map (kbd ".") #'embark-find-definition)
    (define-key map (kbd "k") #'describe-keymap)))

(require 'embark-consult)
(with-eval-after-load 'embark-consult
  (add-to-list 'prot-emacs-ensure-install 'embark-consult)
  ;; ;; Use the hook, or check `prot-embark-consult-preview-toggle'.
  ;; :hook (embark-collect-mode-hook . embark-consult-preview-minor-mode)
  (define-key embark-collect-mode-map (kbd "C-j") #'embark-consult-preview-at-point))

(require 'prot-embark)
(with-eval-after-load 'prot-embark
  (add-hook 'minibuffer-exit-hook #'prot-embark-clear-live-buffers)
  (add-hook 'embark-collect-post-revert-hook #'prot-embark-collect-fit-window)
  (add-hook 'embark-collect-mode-hook #'prot-embark-hl-line)
  (add-hook 'embark-collect-mode-hook #'prot-embark-display-line-numbers)
  ;; NOTE: to switch to the live collection buffer, I also use
  ;; `prot-minibuffer-focus-mini-or-completions' which is bound to
  ;; "s-v".
  (let ((map embark-collect-mode-map))
    (define-key map (kbd "h") #'prot-simple-describe-symbol)  ; from `prot-simple.el'
    (define-key map (kbd "C-g") #'prot-embark-keyboard-quit)
    (define-key map (kbd "C-k") #'prot-embark-collection-kill-line)
    (define-key map (kbd "C-M-n") #'prot-embark-completions-act-next)
    (define-key map (kbd "C-M-p") #'prot-embark-completions-act-previous)
    (define-key map (kbd "C-M-j") #'prot-embark-completions-act-current)
    (define-key map (kbd "C-M-v") #'prot-embark-consult-preview-toggle) ; "view", "visualise" mnemonic
    (define-key map (kbd "C-n") #'prot-embark-next-line-or-mini)
    (define-key map (kbd "<down>") #'prot-embark-next-line-or-mini)
    (define-key map (kbd "C-p") #'prot-embark-previous-line-or-mini)
    (define-key map (kbd "<up>") #'prot-embark-previous-line-or-mini)
    (define-key map (kbd "M-F") #'prot-embark-collection-flush-lines) ; M-S-f like M-S-5 (M-%)
    (define-key map (kbd "M-K") #'prot-embark-collection-keep-lines)) ; same principle
  (let ((map minibuffer-local-completion-map))
    (define-key map (kbd "C-n") #'prot-embark-switch-to-completions-top)
    (define-key map (kbd "<down>") #'prot-embark-switch-to-completions-top)
    (define-key map (kbd "C-p") #'prot-embark-switch-to-completions-bottom)
    (define-key map (kbd "<up>") #'prot-embark-switch-to-completions-bottom)
    (define-key map (kbd "C-l") #'prot-embark-completions-toggle)))

(require 'project)
(with-eval-after-load 'project
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

(require 'prot-project)
(with-eval-after-load 'prot-project
  (setq prot-project-project-roots '("~/src/"))
  (setq prot-project-commit-log-limit 25)
  (setq prot-project-large-file-lines 1000)
  (let ((map global-map))
    (define-key map (kbd "C-x p <delete>") #'prot-project-remove-project)
    (define-key map (kbd "C-x p l") #'prot-project-commit-log)
    (define-key map (kbd "C-x p m") #'prot-project-magit-status)
    (define-key map (kbd "C-x p s") #'prot-project-find-subdir)
    (define-key map (kbd "C-x p t") #'prot-project-retrieve-tag)))

(require 'recentf)
(with-eval-after-load 'recentf
  (setq recentf-save-file (concat user-emacs-directory "recentf"))
  (setq recentf-max-saved-items 200)
  (setq recentf-exclude '(".gz" ".xz" ".zip" "/elpa/" "/ssh:" "/sudo:"))
  (add-hook 'after-init-hook #'recentf-mode))

(require 'prot-recentf)
(with-eval-after-load 'prot-recentf
  (add-to-list 'recentf-keep 'prot-recentf-keep-predicate)
  (let ((map global-map))
    (define-key map (kbd "s-r") #'prot-recentf-recent-files)
    (define-key map (kbd "C-x C-r") #'prot-recentf-recent-dirs)))

(require 'prot-embark-extras)
(with-eval-after-load 'prot-embark-extras
  (prot-embark-extras-keymaps 1)
  (prot-embark-extras-setup-packages 1))

(require 'dabbrev)
(with-eval-after-load 'dabbrev
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

(require 'hippie-exp)
(with-eval-after-load 'hippie-exp
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol
          try-complete-file-name-partially
          try-complete-file-name))
  (setq hippie-expand-verbose t)
  (setq hippie-expand-dabbrev-skip-space nil)
  (setq hippie-expand-dabbrev-as-symbol t)
  (setq hippie-expand-no-restriction t)
  (define-key global-map (kbd "C-M-/") #'hippie-expand))

(require 'isearch)
(with-eval-after-load 'isearch
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

(require 'replace)
(with-eval-after-load 'replace
  (setq list-matching-lines-jump-to-current-line t)
  (add-hook 'occur-mode-hook #'hl-line-mode)
  (add-hook 'occur-mode-hook (lambda ()
			       (toggle-truncate-lines t)))
  (define-key global-map (kbd "M-s M-o") #'multi-occur)
  (define-key occur-mode-map (kbd "t") #'toggle-truncate-lines))

(require 'prot-search)
(with-eval-after-load 'prot-search
  (let ((map global-map))
    (define-key map (kbd "M-s %") #'prot-search-isearch-replace-symbol)
    (define-key map (kbd "M-s M-<") #'prot-search-isearch-beginning-of-buffer)
    (define-key map (kbd "M-s M->") #'prot-search-isearch-end-of-buffer)
    (define-key map (kbd "M-s u") #'prot-search-occur-urls)
    (define-key map (kbd "M-s M-u") #'prot-search-occur-browse-url))
  (let ((map isearch-mode-map))
    (define-key map (kbd "<up>") #'prot-search-isearch-repeat-backward)
    (define-key map (kbd "<down>") #'prot-search-isearch-repeat-forward)
    (define-key map (kbd "<backspace>") #'prot-search-isearch-abort-dwim)
    (define-key map (kbd "<C-return>") #'prot-search-isearch-other-end)))

(require 're-builder)
(with-eval-after-load 're-builder
  (setq reb-re-syntax 'read))

(require 'visual-regexp)
(with-eval-after-load 'visual-regexp
  (add-to-list 'prot-emacs-ensure-install 'visual-regexp)
  (setq vr/default-replace-preview nil)
  (setq vr/match-separator-use-custom-face t))

(require 'wgrep)
(with-eval-after-load 'wgrep
  (add-to-list 'prot-emacs-ensure-install 'wgrep)
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t)
  (let ((map grep-mode-map))
    (define-key map (kbd "e") #'wgrep-change-to-wgrep-mode)
    (define-key map (kbd "C-x C-q") #'wgrep-change-to-wgrep-mode)
    (define-key map (kbd "C-c C-c") #'wgrep-finish-edit)))

(require 'xref)
(with-eval-after-load 'xref
  ;; ;; Use this for Emacs 27 (I am on 28)
  ;; (add-to-list 'prot-emacs-ensure-install 'xref)

  ;; All those have been changed for Emacs 28
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (setq xref-show-xrefs-function #'xref-show-definitions-completing-read)
  (setq xref-file-name-display 'project-relative)
  (setq xref-search-program 'ripgrep))

(require 'dired)
(with-eval-after-load 'dired
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-AGFhlv --group-directories-first --time-style=long-iso")
  (setq dired-dwim-target t)
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'hl-line-mode))

(require 'dired-aux)
(with-eval-after-load 'dired-aux
  (setq dired-isearch-filenames 'dwim)
  ;; The following variables were introduced in Emacs 27.1
  (setq dired-create-destination-dirs 'ask)
  (setq dired-vc-rename-file t)

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

(require 'dired-x)
(with-eval-after-load 'dired-x
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

(require 'dired-subtree)
(with-eval-after-load 'dired-subtree
  (add-to-list 'prot-emacs-ensure-install 'dired-subtree)
  (setq dired-subtree-use-backgrounds nil)
  (let ((map dired-mode-map))
    (define-key map (kbd "<tab>") #'dired-subtree-toggle)
    (define-key map (kbd "<C-tab>") #'dired-subtree-cycle)
    (define-key map (kbd "<S-iso-lefttab>") #'dired-subtree-remove)))

(require 'wdired)
(with-eval-after-load 'wdired
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))

(require 'image-dired)
(with-eval-after-load 'image-dired
  (setq image-dired-external-viewer "xdg-open")
  (setq image-dired-thumb-size 80)
  (setq image-dired-thumb-margin 2)
  (setq image-dired-thumb-relief 0)
  (setq image-dired-thumbs-per-row 4)
  (define-key image-dired-thumbnail-mode-map
    (kbd "<return>") #'image-dired-thumbnail-display-external))

;; part of `async' package
(require 'dired-async)
(with-eval-after-load 'dired-async
  (add-hook 'dired-mode-hook #'dired-async-mode))

(require 'uniquify)
(with-eval-after-load 'uniquify
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-strip-common-suffix t)
  (setq uniquify-after-kill-buffer-p t))

(require 'ibuffer)
(with-eval-after-load 'ibuffer
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
                (name 30 30 :left :elide)
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

(require 'prot-ibuffer)
(with-eval-after-load 'prot-ibuffer
  (let ((map global-map))
    (define-key map (kbd "M-s b") #'prot-ibuffer-buffers-major-mode)
    (define-key map (kbd "M-s v") #'prot-ibuffer-buffers-vc-root)))

(require 'scratch)
(with-eval-after-load 'scratch
  (add-to-list 'prot-emacs-ensure-install 'scratch)
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

(require 'window)
(with-eval-after-load 'window
  (setq display-buffer-alist
        '(;; top side window
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
          ("\\*.*\\([^E]eshell\\|shell\\|v?term\\).*"
           (display-buffer-reuse-mode-window display-buffer-at-bottom)
           (window-height . 0.2)
           ;; (mode . '(eshell-mode shell-mode))
           )))
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
    (define-key map (kbd "C-x _") #'balance-windows)
    (define-key map (kbd "C-x +") #'balance-windows-area)
    (define-key map (kbd "s-q") #'window-toggle-side-windows)))

(require 'winner)
(with-eval-after-load 'winner
  (add-hook 'after-init-hook #'winner-mode)
  (let ((map global-map))
    (define-key map (kbd "<s-right>") #'winner-redo)
    (define-key map (kbd "<s-left>") #'winner-undo)))

(require 'windmove)
(with-eval-after-load 'windmove
  (setq windmove-create-window nil)     ; Emacs 27.1
  (let ((map global-map))
    (define-key map (kbd "<C-M-up>") #'windmove-up)
    (define-key map (kbd "<C-M-right>") #'windmove-right)
    (define-key map (kbd "<C-M-down>") #'windmove-down)
    (define-key map (kbd "<C-M-left>") #'windmove-left)))

(require 'tab-bar)
(with-eval-after-load 'tab-bar
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
  (setq tab-bar-close-tab-select 'recent)
  (setq tab-bar-new-tab-choice t)
  (setq tab-bar-new-tab-to 'right)
  (setq tab-bar-position nil)
  (setq tab-bar-show nil)
  (setq tab-bar-tab-hints nil)
  (setq tab-bar-tab-name-function 'tab-bar-tab-name-all)
  (tab-bar-mode -1)
  (tab-bar-history-mode -1)
  (let ((map global-map))
    (define-key map (kbd "<s-tab>") #'tab-next)
    (define-key map (kbd "<S-s-iso-lefttab>") #'tab-previous)))

(require 'prot-tab)
(with-eval-after-load 'prot-tab
  (let ((map global-map))
    (define-key map (kbd "<f8>") #'prot-tab-tab-bar-toggle)
    (define-key map (kbd "C-x t t") #'prot-tab-select-tab-dwim)
    (define-key map (kbd "s-t") #'prot-tab-select-tab-dwim)))

(require 'transpose-frame)
(with-eval-after-load 'transpose-frame
  (add-to-list 'prot-emacs-ensure-install 'transpose-frame)
  (let ((map global-map))
    (define-key map (kbd "C-s-t") #'flop-frame) ; what I consider "transpose" in this context
    (define-key map (kbd "C-s-r") #'rotate-frame-clockwise)))

(require 'face-remap)

(require 'olivetti)
(with-eval-after-load 'olivetti
  (add-to-list 'prot-emacs-ensure-install 'olivetti)
  (setq olivetti-body-width 0.7)
  (setq olivetti-minimum-body-width 80)
  (setq olivetti-recall-visual-line-mode-entry-state t))

(require 'prot-logos)
(with-eval-after-load 'prot-logos
  (setq prot-logos-org-presentation nil)
  (setq prot-logos-variable-pitch nil)
  (setq prot-logos-scroll-lock nil)
  (setq prot-logos-hidden-modeline t)
  (define-key global-map (kbd "<f9>") #'prot-logos-focus-mode))

(require 'deft)
(with-eval-after-load 'deft
  (add-to-list 'prot-emacs-ensure-install 'deft)
  (setq deft-extensions '("md" "txt" "org"))
  (setq deft-default-extension "md")
  (setq deft-directory "~/Documents/notes")
  (setq deft-use-filename-as-title t))

(require 'zetteldeft)
(with-eval-after-load 'zetteldeft
  (add-to-list 'prot-emacs-ensure-install 'zetteldeft)
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
                             . font-lock-constant-face))))

(require 'tmr)
(with-eval-after-load 'tmr
  (setq tmr-sound-file
        "/usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga"))

(require 'diff-mode)
(with-eval-after-load 'diff-mode
  (setq diff-default-read-only t)
  (setq diff-advance-after-apply-hunk t)
  (setq diff-update-on-the-fly t)
  ;; The following are from Emacs 27.1
  (setq diff-refine nil)                ; I do it on demand
  (setq diff-font-lock-prettify nil)    ; better for patches
  ;; The following is further controlled by
  ;; `prot-diff-modus-themes-diffs'
  (setq diff-font-lock-syntax 'hunk-also))

(require 'prot-diff)
(with-eval-after-load 'prot-diff
  (prot-diff-modus-themes-diffs)
  (add-hook 'modus-themes-after-load-theme-hook #'prot-diff-modus-themes-diffs)

  (prot-diff-extra-keywords 1)

  ;; `prot-diff-buffer-dwim' replaces the default for `vc-diff' (which I
  ;; bind to another key---see VC section).
  (define-key global-map (kbd "C-x v =") #'prot-diff-buffer-dwim)
  (let ((map diff-mode-map))
    (define-key map (kbd "C-c C-b") #'prot-diff-refine-dwim) ; replace `diff-refine-hunk'
    (define-key map (kbd "C-c C-n") #'prot-diff-narrow-dwim)))

(with-eval-after-load 'vc

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
  (with-eval-after-load 'log-edit
    (setq log-edit-confirm 'changed)
    (setq log-edit-keep-buffer nil)
    (setq log-edit-require-final-newline t)
    (setq log-edit-setup-add-author nil))

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
        '("%d%h %ad %an: %s"
          ;; The first shy group matches the characters drawn by --graph.
          ;; We use numbered groups because `log-view-message-re' wants the
          ;; revision number to be group 1.
          "^\\(?:[*/\\| ]+ \\)?\
\\(?2: ([^)]+)\\)?\\(?1:[0-9a-z]+\\) \
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
    (define-key map (kbd "o") #'vc-log-outgoing)
    (define-key map (kbd "f") #'vc-log-incoming) ; replaces `vc-dir-find-file' (use RET)
    (define-key map (kbd "F") #'vc-update)       ; symmetric with P: `vc-push'
    (define-key map (kbd "d") #'vc-diff)         ; parallel to D: `vc-root-diff'
    (define-key map (kbd "k") #'vc-dir-clean-files))
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

(require 'prot-vc)
(with-eval-after-load 'prot-vc
  (setq prot-vc-log-limit 20)
  (setq prot-vc-shell-output "*prot-vc-output*")
  (setq prot-vc-patch-output-dirs (list "~/" "~/Desktop/"))

  ;; This refashions log view and log edit buffers
  (prot-vc-git-setup-mode 1)

  ;; NOTE: I override lots of the defaults
  (let ((map global-map))
    (define-key map (kbd "C-x v i") #'prot-vc-git-log-insert-commits)
    (define-key map (kbd "C-x v p") #'prot-vc-project-or-dir)
    (define-key map (kbd "C-x v SPC") #'prot-vc-custom-log)
    (define-key map (kbd "C-x v c") #'prot-vc-git-patch-dwim)
    (define-key map (kbd "C-x v s") #'prot-vc-git-show)
    (define-key map (kbd "C-x v r") #'prot-vc-git-find-revision)
    (define-key map (kbd "C-x v B") #'prot-vc-git-blame-region-or-file))
  (let ((map vc-git-log-edit-mode-map))
    (define-key map (kbd "C-C C-n") #'prot-vc-git-log-extract-file-name)
    (define-key map (kbd "C-C C-i") #'prot-vc-git-log-insert-commits)
    ;; Also done by `prot-vc-git-setup-mode', but I am putting it here
    ;; as well for visibility.
    (define-key map (kbd "C-c C-c") #'prot-vc-git-log-edit-done))
  (let ((map log-view-mode-map))
    (define-key map (kbd "<C-tab>") #'prot-vc-log-view-toggle-entry-all)
    (define-key map (kbd "c") #'prot-vc-git-patch-dwim)
    (define-key map (kbd "w") #'prot-vc-log-kill-hash)))

(require 'magit)
(with-eval-after-load 'magit
  (add-to-list 'prot-emacs-ensure-install 'magit)
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

(require 'smerge-mode)

(require 'ediff)
(with-eval-after-load 'ediff
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

(require 'eshell)
(with-eval-after-load 'eshell
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

  (require 'em-cmpl)

  (require 'em-dirs)
  (setq eshell-cd-on-directory t)

  (require 'em-tramp)
  (setq password-cache t)
  (setq password-cache-expiry 600)

  (require 'em-hist)
  (setq eshell-hist-ignoredups t)
  (setq eshell-save-history-on-exit t))

(require 'prot-eshell)
(with-eval-after-load 'prot-eshell
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
    (define-key map (kbd "C-c C-s") #'prot-eshell-find-subdirectory-recursive)))

(require 'shell)
(with-eval-after-load 'shell
  (setq ansi-color-for-comint-mode t)
  (setq shell-command-prompt-show-cwd t) ; Emacs 27.1
  (define-key global-map (kbd "<s-S-return>") #'shell))

(require 'calendar)
(with-eval-after-load 'calendar
  (setq calendar-mark-diary-entries-flag nil)
  (setq calendar-time-display-form
        '(24-hours ":" minutes
                   (when time-zone
                     (concat " (" time-zone ")"))))
  (setq calendar-week-start-day 1)      ; Monday
  (setq calendar-date-style 'iso)
  (setq calendar-mark-holidays-flag nil)
  (setq calendar-time-zone-style 'numeric) ; Emacs 28.1

  (add-hook 'calendar-today-visible-hook #'calendar-mark-today))

(require 'auth-source)
(with-eval-after-load 'auth-source
  (setq auth-sources '("~/.authinfo.gpg"))
  (setq user-full-name "David Porter")
  (setq user-mail-address "david@daporter.net"))

(require 'mm-encode)
(with-eval-after-load 'mm-encode
  (setq mm-encrypt-option 'guided)
  (setq mm-sign-option 'guided))

(require 'mml-sec)
(with-eval-after-load 'mml-sec
  (setq mml-secure-openpgp-encrypt-to-self t)
  (setq mml-secure-openpgp-sign-with-sender t)
  (setq mml-secure-smime-encrypt-to-self t)
  (setq mml-secure-smime-sign-with-sender t))

(require 'message)
(with-eval-after-load 'message
  (setq mail-user-agent 'message-user-agent)
  (setq compose-mail-user-agent-warnings nil)
  (setq message-mail-user-agent nil)    ; default is `gnus'
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

(require 'gnus)
(with-eval-after-load 'gnus
  (require 'gnus-sum)
  (require 'gnus-dired)
  (require 'gnus-topic)
  (require 'prot-gnus)
;;; accounts
  (setq gnus-select-method '(nnnil ""))
  (setq gnus-secondary-select-methods
        '((nntp "news.gwene.org")
          (nnmaildir "migadu" (directory "~/mail/migadu")
                     (gnus-search-engine gnus-search-notmuch ; this feature is from Emacs 28
 					                     (remove-prefix "~/mail/migadu")))))

  (setq gnus-search-use-parsed-queries t) ; Emacs 28

  (setq gnus-parameters
        '((".*"
           (posting-style
            (gcc "nnmaildir+migadu:sent")))))

  (setq gnus-gcc-mark-as-read t)
  (setq gnus-agent t)
  (setq gnus-novice-user nil)           ; careful with this
  ;; checking sources
  (setq gnus-check-new-newsgroups 'ask-server)
  (setq gnus-read-active-file 'some)
  ;; dribble
  (setq gnus-use-dribble-file t)
  (setq gnus-always-read-dribble-file t)
;;; agent
  (setq gnus-agent-article-alist-save-format 1)  ; uncompressed
  (setq gnus-agent-cache t)
  (setq gnus-agent-confirmation-function 'y-or-n-p)
  (setq gnus-agent-consider-all-articles nil)
  (setq gnus-agent-directory "~/News/agent/")
  (setq gnus-agent-enable-expiration 'ENABLE)
  (setq gnus-agent-expire-all nil)
  (setq gnus-agent-expire-days 30)
  (setq gnus-agent-mark-unread-after-downloaded t)
  (setq gnus-agent-queue-mail t)        ; queue if unplugged
  (setq gnus-agent-synchronize-flags nil)
;;; article
  (setq gnus-article-browse-delete-temp 'ask)
  (setq gnus-article-over-scroll nil)
  (setq gnus-article-show-cursor t)
  (setq gnus-article-sort-functions
        '(gnus-article-sort-by-number
          gnus-article-sort-by-date))
  (setq gnus-article-truncate-lines nil)
  (setq gnus-html-frame-width 80)
  (setq gnus-html-image-automatic-caching t)
  (setq gnus-inhibit-images t)
  (setq gnus-max-image-proportion 0.7)
  (setq gnus-treat-display-smileys nil)
  (setq gnus-article-mode-line-format "%G %S %m")
  (setq gnus-visible-headers
        '("^From:" "^To:" "^Cc:" "^Subject:" "^Newsgroups:" "^Date:"
          "Followup-To:" "Reply-To:" "^Organization:" "^X-Newsreader:"
          "^X-Mailer:"))
  (setq gnus-sorted-header-list gnus-visible-headers)
;;; async
  (setq gnus-asynchronous t)
  (setq gnus-use-article-prefetch 15)
;;; group
  (setq gnus-list-groups-with-ticked-articles t)
  (setq gnus-group-sort-function
        '((gnus-group-sort-by-unread)
          (gnus-group-sort-by-alphabet)
          (gnus-group-sort-by-rank)))
  (setq gnus-group-line-format "%M%p%P%5y:%B%(%g%)\n")
  (setq gnus-group-mode-line-format "%%b")
  (setq gnus-topic-display-empty-topics nil)
;;; summary
  (setq gnus-auto-select-first nil)
  (setq gnus-summary-ignore-duplicates t)
  (setq gnus-suppress-duplicates t)
  (setq gnus-save-duplicate-list t)
  (setq gnus-summary-goto-unread nil)
  (setq gnus-thread-sort-functions
        '(gnus-thread-sort-by-number
          gnus-thread-sort-by-date))
  (setq gnus-subthread-sort-functions
        'gnus-thread-sort-by-date)
  (setq gnus-thread-hide-subtree nil)
  (setq gnus-thread-ignore-subject nil)
  (setq gnus-user-date-format-alist
        '(((gnus-seconds-today) . "Today at %R")
          ((+ (* 60 60 24) (gnus-seconds-today)) . "Yesterday, %R")
          (t . "%Y-%m-%d %R")))

  ;; When the %f specifier in `gnus-summary-line-format' matches my
  ;; name, this will use the contents of the "To:" field, prefixed by
  ;; the string I specify.  Useful when checking your "Sent" summary or
  ;; a mailing list you participate in.
  (setq gnus-ignored-from-addresses "David Porter")
  (setq gnus-summary-to-prefix "To: ")

  (defun dp-get-address-from-header (field header)
    (cdr (assq field (elt header (1- (length header))))))

  (defun gnus-user-format-function-g (header)
    "Indicate whether HEADER contains my Gmail address."
    (let ((gmail-address "david\\.a\\.porter@gmail\\.com")
          (to-address (dp-get-address-from-header 'To header))
          (cc-address (dp-get-address-from-header 'Cc header)))
      (if (or (and to-address (string-match gmail-address to-address))
              (and cc-address (string-match gmail-address cc-address)))
          "G"
        " ")))

  (setq gnus-summary-make-false-root 'dummy)
  (setq gnus-summary-dummy-line-format
        (concat "   "
                "                      "
                "                               "
                "• %S\n"))
  (setq gnus-summary-line-format
        (concat "%0{%U%R%z%}"
                "%-16,16&user-date;  "
                "%0{%ug%}  "
                "%-30,30f  "
                "%B" "%s\n"))

  (setq gnus-summary-mode-line-format "[%U] %p")
  (setq gnus-sum-thread-tree-single-indent   "• ")
  (setq gnus-sum-thread-tree-false-root      "  ")
  (setq gnus-sum-thread-tree-root            "• ")
  (setq gnus-sum-thread-tree-vertical        "│ ")
  (setq gnus-sum-thread-tree-leaf-with-other "├─➤ ")
  (setq gnus-sum-thread-tree-single-leaf     "└─➤ ")
  (setq gnus-sum-thread-tree-indent          "  ")

  (add-hook 'dired-mode-hook #'gnus-dired-mode) ; dired integration
  (add-hook 'gnus-group-mode-hook #'hl-line-mode)
  (add-hook 'gnus-group-mode-hook #'gnus-topic-mode)
  (add-hook 'gnus-summary-mode-hook #'hl-line-mode)
  (add-hook 'gnus-browse-mode-hook #'hl-line-mode)
  (add-hook 'gnus-server-mode-hook #'hl-line-mode)
  (add-hook 'gnus-select-group-hook #'gnus-group-set-timestamp)
  ;; ;; TODO 2021-01-28: fill for `gnus-article-mode-hook' should be
  ;; ;; reviewed in light of prot-fill.el
  ;; (add-hook 'gnus-article-mode-hook (lambda () (setq-local fill-column 80)))
  (define-key global-map (kbd "C-c m") #'gnus)
  (let ((map gnus-article-mode-map))
    (define-key map (kbd "i") #'gnus-article-show-images)
    (define-key map (kbd "s") #'gnus-mime-save-part)
    (define-key map (kbd "o") #'gnus-mime-copy-part))
  (let ((map gnus-group-mode-map))       ; I always use `gnus-topic-mode'
    (define-key map (kbd "n") #'gnus-group-next-group)
    (define-key map (kbd "p") #'prot-gnus-group-previous-group) ; from `prot-gnus.el'
    (define-key map (kbd "M-n") #'gnus-topic-goto-next-topic)
    (define-key map (kbd "M-p") #'gnus-topic-goto-previous-topic))
  (let ((map gnus-summary-mode-map))
    (define-key map (kbd "<delete>") #'gnus-summary-delete-article)
    (define-key map (kbd "n") #'gnus-summary-next-article)
    (define-key map (kbd "p") #'gnus-summary-prev-article)
    (define-key map (kbd "N") #'gnus-summary-next-unread-article)
    (define-key map (kbd "P") #'gnus-summary-prev-unread-article)
    (define-key map (kbd "M-n") #'gnus-summary-next-thread)
    (define-key map (kbd "M-p") #'gnus-summary-prev-thread)
    (define-key map (kbd "C-M-n") #'gnus-summary-next-group)
    (define-key map (kbd "C-M-p") #'gnus-summary-prev-group)
    (define-key map (kbd "C-M-^") #'gnus-summary-refer-thread)))

(require 'nnmail)
(with-eval-after-load 'nnmail
  (setq nnmail-expiry-wait 30))         ; careful with this

(require 'smtpmail)
(with-eval-after-load 'smtpmail
  (setq smtpmail-default-smtp-server "smtp.migadu.com")
  (setq smtpmail-smtp-server "smtp.migadu.com")
  (setq smtpmail-stream-type 'starttls)
  (setq smtpmail-smtp-service 587)
  (setq smtpmail-queue-mail nil))

;; part of `async' package
(require 'smtpmail-async)
(with-eval-after-load 'smtpmail-async
  (setq send-mail-function 'async-smtpmail-send-it)
  (setq message-send-mail-function 'async-smtpmail-send-it))

(require 'mbsync)
(with-eval-after-load 'mbsync
  (add-to-list 'prot-emacs-ensure-install 'mbsync)
  (setq mbsync-verbose 'verbose)
  (add-hook 'mbsync-exit-hook (lambda ()
                                (gnus-group-get-new-news 1)))
  (define-key gnus-group-mode-map (kbd "f") #'mbsync))

(require 'elfeed)
(with-eval-after-load 'elfeed
  (add-to-list 'prot-emacs-ensure-install 'elfeed)
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
  (require 'prot-elfeed-bongo)
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

(require 'prot-elfeed)
(with-eval-after-load 'prot-elfeed
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
    (define-key map (kbd "+") #'prot-elfeed-toggle-tag)))

(require 'reftex)
(with-eval-after-load 'reftex
  (setq reftex-default-bibliography
        '("~/Documents/bibliography/references.bib"))

  (defun dp/reftex-citation ()
    (interactive)
    (let ((reftex-cite-format
           '((?\C-m . "%l")
             ;; MLA citation style
             (?f . "[#%l]: %a. *%t*. %d, %u, %y, %p %<."))))
      (reftex-citation))))

(require 'bibtex)
(with-eval-after-load 'bibtex
  (setq bibtex-dialect 'biblatex)
  (setq bibtex-align-at-equal-sign t)
  (setq bibtex-autokey-name-year-separator "")
  (setq bibtex-autokey-year-title-separator "")
  (setq bibtex-autokey-titleword-first-ignore '("the" "a" "if" "and" "an"))
  (setq bibtex-autokey-titleword-length 30)
  (setq bibtex-autokey-titlewords 1)
  (setq bibtex-autokey-titlewords-stretch 0))

(require 'ebib)
(with-eval-after-load 'ebib
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

(define-key global-map (kbd "C-c C-z b") #'dp-insert-zotero-bibliography)
(define-key global-map (kbd "C-c C-z c") #'dp-insert-zotero-citation)

(require 'proced)
(with-eval-after-load 'proced
  (setq proced-auto-update-flag t)
  (setq proced-auto-update-interval 1)
  (setq proced-descend t)
  (setq proced-filter 'user))

(require 'password-store)
(with-eval-after-load 'password-store
  (add-to-list 'prot-emacs-ensure-install 'password-store)
  (setq password-store-time-before-clipboard-restore 30))

(require 'pass)
(with-eval-after-load 'pass
  (add-to-list 'prot-emacs-ensure-install 'pass))

(require 'shr)
(with-eval-after-load 'shr
  (setq shr-use-fonts nil)
  (setq shr-use-colors nil)
  (setq shr-max-image-proportion 0.7)
  (setq shr-image-animate nil)
  (setq shr-width (current-fill-column)))

;; TODO 2021-01-19: Everything about eww is subject to review.  It is
;; not in a good state.
(require 'eww)
(with-eval-after-load 'eww
  (setq eww-restore-desktop nil)
  (setq eww-desktop-remove-duplicates t)
  (setq eww-header-line-format "%u")
  (setq eww-search-prefix "https://duckduckgo.com/html/?q=")
  (setq eww-download-directory "~/Downloads/")
  (setq eww-suggest-uris
        '(eww-links-at-point
          thing-at-point-url-at-point))
  (setq eww-bookmarks-directory (concat user-emacs-directory "eww-bookmarks/"))
  (setq eww-history-limit 150)
  (setq eww-use-external-browser-for-content-type
        "\\`\\(video/\\|audio/\\|application/pdf\\)")
  (setq eww-browse-url-new-window-is-tab nil)
  (setq eww-form-checkbox-selected-symbol "[X]")
  (setq eww-form-checkbox-symbol "[ ]")

  ;;   (defun prot/eww-visit-history (&optional arg)
  ;;     "Revisit a URL from `eww-prompt-history' using completion.
  ;; With \\[universal-argument] produce a new buffer."
  ;;     (interactive "P")
  ;;     (let ((history eww-prompt-history)  ; eww-bookmarks
  ;;           (new (if arg t nil)))
  ;;       (eww
  ;;        (completing-read "Visit website from history: " history nil t)
  ;;        new)))

  ;; eww-view-source

  ;;   (defvar prot/eww-mode-global-map
  ;;     (let ((map (make-sparse-keymap)))
  ;;       (define-key map "s" 'eww-search-words)
  ;;       (define-key map "o" 'eww-open-in-new-buffer)
  ;;       (define-key map "f" 'eww-open-file)
  ;;       (define-key map "w" 'prot/eww-visit-history)
  ;;       map)
  ;;     "Key map to scope `eww' bindings for global usage.
  ;; The idea is to bind this to a prefix sequence, so that its
  ;; defined keys follow the pattern of <PREFIX> <KEY>.")

  (let ((map eww-mode-map))
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "f") #'forward-char)
    (define-key map (kbd "b") #'backward-char)
    (define-key map (kbd "a") #'prot/eww-org-archive-current-url)
    (define-key map (kbd "B") #'eww-back-url)
    (define-key map (kbd "N") #'eww-next-url)
    (define-key map (kbd "P") #'eww-previous-url)))

(require 'browse-url)
(with-eval-after-load 'browse-url
  (setq browse-url-browser-function 'eww-browse-url))

(require 'debian-el)
(with-eval-after-load 'debian-el
  (add-to-list 'prot-emacs-ensure-install 'debian-el))

(require 'beginend)
(with-eval-after-load 'beginend
  (add-to-list 'prot-emacs-ensure-install 'beginend)
  (beginend-global-mode 1))

(require 'goto-last-change)
(with-eval-after-load 'goto-last-change
  (add-to-list 'prot-emacs-ensure-install 'goto-last-change)
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

(setq mode-line-compact t)            ; Emacs 28
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

(require 'moody)
(with-eval-after-load 'moody
  (add-to-list 'prot-emacs-ensure-install 'moody))

(require 'prot-moody)
(with-eval-after-load 'prot-moody
  (prot-moody-set-height -1))

(require 'minions)
(with-eval-after-load 'minions
  (add-to-list 'prot-emacs-ensure-install 'minions)
  (setq minions-mode-line-lighter ";")
  ;; NOTE: This will be expanded whenever I find a mode that should not be hidden
  (setq minions-direct (list 'defining-kbd-macro
                             'flymake-mode))
  (minions-mode 1))

(require 'recursion-indicator)
(with-eval-after-load 'recursion-indicator
  (add-to-list 'prot-emacs-ensure-install 'recursion-indicator)
  (setq recursion-indicator-general "&")
  (setq recursion-indicator-minibuffer "@")
  (recursion-indicator-mode 1))

(require 'battery)
(with-eval-after-load 'battery
  (setq battery-mode-line-format " [%b%p%%]")
  (setq battery-mode-line-limit 95)
  (setq battery-update-interval 180)
  (setq battery-load-low 20)
  (setq battery-load-critical 10)
  (add-hook 'after-init-hook #'display-battery-mode))

(require 'time)
(with-eval-after-load 'time
  (setq display-time-format "%H:%M  %Y-%m-%d")
  ;;;; Covered by `display-time-format'
  ;; (setq display-time-24hr-format t)
  ;; (setq display-time-day-and-date t)
  (setq display-time-interval 60)
  (setq display-time-mail-directory nil)
  (setq display-time-default-load-average nil)

;;; World clock
  (setq zoneinfo-style-world-list
        '(("America/Los_Angeles" "Los Angeles")
          ("America/New_York" "New York")
          ("Europe/Amsterdam" "Amsterdam")
          ("Australia/Brisbane" "Brisbane")
          ("Australia/Sydney" "Sydney")))

  ;; All of the following variables are for Emacs 28
  (setq world-clock-list t)
  (setq world-clock-time-format "%R %z  %A %d %B")
  (setq world-clock-buffer-name "*world-clock*") ; Placement handled by `display-buffer-alist'
  (setq world-clock-timer-enable t)
  (setq world-clock-timer-second 60)

  (add-hook 'after-init-hook #'display-time-mode))

(setq window-divider-default-right-width 1)
(setq window-divider-default-bottom-width 1)
(setq window-divider-default-places 'right-only)
(add-hook 'after-init-hook #'window-divider-mode)

(require 'fringe)
(with-eval-after-load 'fringe
  (fringe-mode nil)
  (setq-default fringes-outside-margins nil)
  (setq-default indicate-buffer-boundaries nil)
  (setq-default indicate-empty-lines nil)
  (setq-default overflow-newline-into-fringe t))

(require 'diff-hl)
(with-eval-after-load 'diff-hl
  (add-to-list 'prot-emacs-ensure-install 'diff-hl)
  (setq diff-hl-draw-borders nil)
  (setq diff-hl-side 'left)
  ;; TODO 2021-01-19: write toggle for diff-hl-mode
  (add-hook 'after-init-hook #'global-diff-hl-mode))

(require 'hl-line)
(with-eval-after-load 'hl-line
  (setq hl-line-sticky-flag nil))

(require 'display-line-numbers)
(with-eval-after-load 'display-line-numbers
  ;; Set absolute line numbers.  A value of "relative" is also useful.
  (setq display-line-numbers-type t)
  ;; Those two variables were introduced in Emacs 27.1
  (setq display-line-numbers-major-tick 0)
  (setq display-line-numbers-minor-tick 0)
  ;; Use absolute numbers in narrowed buffers
  (setq-default display-line-numbers-widen t)

  ;; TODO 2021-01-19: this is subject to review, as it is fragile in its
  ;; current state.
  (define-minor-mode prot/display-line-numbers-mode
    "Toggle `display-line-numbers-mode' and `hl-line-mode'."
    :init-value nil
    :global nil
    (if prot/display-line-numbers-mode
        (progn
          (display-line-numbers-mode 1)
          (hl-line-mode 1)
          (setq-local truncate-lines t))
      (display-line-numbers-mode -1)
      (hl-line-mode -1)
      (setq-local truncate-lines nil)))

  (define-key global-map (kbd "<f7>") #'prot/display-line-numbers-mode))

(require 'whitespace)
(with-eval-after-load 'whitespace
  (defun prot/toggle-invisibles ()
    "Toggles the display of indentation and space characters."
    (interactive)
    (if (bound-and-true-p whitespace-mode)
        (whitespace-mode -1)
      (whitespace-mode)))

  (let ((map global-map))
    (define-key map (kbd "<f6>") #'prot/toggle-invisibles)
    (define-key map (kbd "C-c z") #'delete-trailing-whitespace)))

(require 'prot-outline)
(with-eval-after-load 'prot-outline
  (define-key global-map (kbd "<f10>") #'prot-outline-minor-mode-safe))

(require 'outline-minor-faces)
(with-eval-after-load 'outline-minor-faces
  (add-to-list 'prot-emacs-ensure-install 'outline-minor-faces)
  (add-hook 'prot-outline-minor-mode-enter-hook
	    #'outline-minor-faces-add-font-lock-keywords))

(setq-default cursor-type '(hbar . 3))
(setq-default cursor-in-non-selected-windows 'hollow)
(setq-default blink-cursor-blinks 50)
(setq-default blink-cursor-interval 0.2)
(setq-default blink-cursor-delay 0.2)
(blink-cursor-mode 1)

(require 'mouse)
(with-eval-after-load 'mouse
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
  (add-hook 'after-init-hook #'mouse-wheel-mode))

(setq-default scroll-preserve-screen-position t)
(setq-default scroll-conservatively 1) ; affects `scroll-step'
(setq-default scroll-margin 0)

(require 'delsel)
(with-eval-after-load 'delsel
  (add-hook 'after-init-hook #'delete-selection-mode))

(require 'tooltip)
(with-eval-after-load 'tooltip
  (setq tooltip-delay 0.5)
  (setq tooltip-short-delay 0.5)
  (setq x-gtk-use-system-tooltips nil)
  (setq tooltip-frame-parameters
        '((name . "tooltip")
          (internal-border-width . 6)
          (border-width . 0)
          (no-special-glyphs . t)))
  (add-hook 'after-init-hook #'tooltip-mode))

(require 'autorevert)
(with-eval-after-load 'autorevert
  (setq auto-revert-verbose t)
  (add-hook 'after-init-hook #'global-auto-revert-mode))

(setq save-interprogram-paste-before-kill t)

(require 'goggles)
(with-eval-after-load 'goggles
  (add-to-list 'prot-emacs-ensure-install 'goggles)
  (setq-default goggles-pulse t)
  (goggles-mode 1))

(setq mode-require-final-newline 'visit-save)

(setq repeat-on-final-keystroke t)
(setq set-mark-command-repeat-pop t)
(define-key global-map (kbd "M-z") #'zap-up-to-char)
(define-key global-map (kbd "s-z") #'repeat)

(require 'package)
(with-eval-after-load 'package
  ;; All variables are for Emacs 28+
  (setq package-name-column-width 40)
  (setq package-version-column-width 14)
  (setq package-status-column-width 12)
  (setq package-archive-column-width 8)
  (add-hook 'package-menu-mode-hook #'hl-line-mode))

(require 'prot-text)
(with-eval-after-load 'prot-text
  (add-to-list 'auto-mode-alist '("\\(README\\|CHANGELOG\\|COPYING\\|LICENSE\\)$" . text-mode))
  (let ((map text-mode-map))
    (define-key map (kbd "<M-return>") #'prot-text-insert-heading)
    (define-key map (kbd "M-;") #'prot-text-cite-region))
  (define-key org-mode-map (kbd "M-;") nil))

(require 'markdown-mode)
(with-eval-after-load 'markdown-mode
  (add-to-list 'prot-emacs-ensure-install 'markdown-mode)
  ;; Allows for fenced block focus with C-c ' (same as Org blocks).
  (require 'edit-indirect)
  (add-to-list 'prot-emacs-ensure-install 'edit-indirect)
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-asymmetric-header t)
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode)))

(require 'pandoc-mode)
(with-eval-after-load 'pandoc-mode
  (add-to-list 'prot-emacs-ensure-install 'pandoc-mode)
  (add-hook 'markdown-mode-hook #'pandoc-mode)
  (add-hook 'pandoc-mode-hook #'pandoc-load-default-settings)
  (add-hook 'pandoc-async-success-hook #'pandoc-view-output))

(require 'yaml-mode)
(with-eval-after-load 'yaml-mode
  (add-to-list 'prot-emacs-ensure-install 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode)))

(require 'css-mode)
(with-eval-after-load 'css-mode
  (add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
  (add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
  (setq css-fontify-colors nil))

(require 'sh-script)
(with-eval-after-load 'sh-script
  (add-to-list 'auto-mode-alist '("PKGBUILD" . sh-mode)))

(require 'prot-fill)
(with-eval-after-load 'prot-fill
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

(require 'newcomment)
(with-eval-after-load 'newcomment
  (setq comment-empty-lines t)
  (setq comment-fill-column nil)
  (setq comment-multi-line t)
  (setq comment-style 'multi-line)
  (let ((map global-map))
    (define-key map (kbd "C-:") #'comment-kill)         ; C-S-;
    (define-key map (kbd "M-;") #'comment-indent)))

(require 'prot-comment)
(with-eval-after-load 'prot-comment
  (setq prot-comment-comment-keywords
        '("TODO" "NOTE" "XXX" "REVIEW" "FIXME"))
  (let ((map global-map))
    (define-key map (kbd "C-;") #'prot-comment-comment-dwim)
    (define-key map (kbd "C-x C-;") #'prot-comment-timestamp-keyword)))

(require 'electric)
(with-eval-after-load 'electric
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

(require 'paren)
(with-eval-after-load 'paren
  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-in-periphery nil)
  (setq show-paren-when-point-inside-paren nil)
  (add-hook 'after-init-hook #'show-paren-mode))

(setq-default tab-always-indent 'complete)
(setq-default tab-first-completion 'word-or-paren-or-punct) ; Emacs 27
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(require 'flyspell)
(with-eval-after-load 'flyspell
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  (setq ispell-program-name "aspell")
  (setq ispell-dictionary "en_GB")
  (define-key flyspell-mode-map (kbd "C-;") nil))

(require 'prot-spell)
(with-eval-after-load 'prot-spell
  (setq prot-spell-dictionaries
        '(("EN English" . "en")
          ("FR Français" . "fr")
          ("NL Nederlands" . "nl")
          ("ES Espanõl" . "es")))
  (let ((map global-map))
    (define-key map (kbd "M-$") #'prot-spell-spell-dwim)
    (define-key map (kbd "C-M-$") #'prot-spell-change-dictionary)))

(require 'flymake)
(with-eval-after-load 'flymake
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
    (define-key map (kbd "C-c ! p") #'flymake-goto-prev-error)))

(require 'flymake-diagnostic-at-point)
(with-eval-after-load 'flymake-diagnostic-at-point
  (add-to-list 'prot-emacs-ensure-install 'flymake-diagnostic-at-point)
  (setq flymake-diagnostic-at-point-display-diagnostic-function
        'flymake-diagnostic-at-point-display-minibuffer))

(require 'flymake-proselint)
(with-eval-after-load 'flymake-proselint
  (add-to-list 'prot-emacs-ensure-install 'flymake-proselint)
  (add-hook 'markdown-mode #'flymake-proselint-setup)
  (add-hook 'org-mode #'flymake-proselint-setup)
  (add-hook 'text-mode #'flymake-proselint-setup))

(require 'eldoc)
(with-eval-after-load 'eldoc
  (global-eldoc-mode 1))

(require 'man)
(with-eval-after-load 'man
  (let ((map Man-mode-map))
    (define-key map (kbd "i") #'Man-goto-section)
    (define-key map (kbd "g") #'Man-update-manpage)))

(require 'server)
(with-eval-after-load 'server
  (add-hook 'after-init-hook #'server-start))

(require 'desktop)
(with-eval-after-load 'desktop
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
  (desktop-save-mode 1))

(require 'savehist)
(with-eval-after-load 'savehist
  (setq savehist-file (concat user-emacs-directory "savehist"))
  (setq history-length 1000)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (add-hook 'after-init-hook #'savehist-mode))

(require 'saveplace)
(with-eval-after-load 'saveplace
  (setq save-place-file (concat user-emacs-directory "saveplace"))
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

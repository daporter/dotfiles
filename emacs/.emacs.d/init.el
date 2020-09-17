;; Emacs Configuration
;; =============================================================================

;;; Base settings

;;;; Always  refresh package cache

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; Initialise the packages, avoiding a re-initialisation.
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

;; Make sure `use-package' is available.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Configure `use-package' prior to loading it.
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t)
  (setq use-package-compute-statistics t)
  ;; The following is VERY IMPORTANT.  Write hooks using their real name
  ;; instead of a shorter version: after-init ==> `after-init-hook'.
  ;;
  ;; This is to empower help commands with their contextual awareness,
  ;; such as `describe-symbol'.
  (setq use-package-hook-name-suffix nil))

(eval-when-compile
  (require 'use-package))

;;;; Disable GUI components

(use-package emacs
  :init
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  :config
  (setq use-file-dialog nil)
  (setq use-dialog-box t)       ; only for mouse events
  (setq inhibit-splash-screen t)
  :bind (("C-z" . nil)
         ("C-x C-z" . nil)
         ("C-h h" . nil)))

;;;; Set selection timeout

;; On OpenBSD, Emacs sometimes freezes when interacting with the
;; secondary selection.  This happens often when invoking 'org-capture'.
;; The workaround is to set a short timeout for accessing the secondary
;; selection.  More info here:
;; https://omecha.info/blog/org-capture-freezes-emacs.html
(use-package emacs
  :config
  (setq x-selection-timeout 10))

;;;; Remove modeline "lighters"

(use-package diminish
  :ensure
  :after use-package)

;;;; Put customisation settings in a "custom.el"

(use-package cus-edit
  :config
  (defvar prot/custom-file "~/.emacs.d/custom.el")

  (setq custom-file prot/custom-file)

  (defun prot/cus-edit ()
    (let ((file prot/custom-file))
      (unless (file-exists-p file)
        (make-empty-file file))
      (load-file file)))
  :hook (after-init-hook . prot/cus-edit))

;;;; Modus themes

(use-package modus-operandi-theme
  :ensure
  :init
  (setq modus-operandi-theme-slanted-constructs t
        modus-operandi-theme-bold-constructs t
        modus-operandi-theme-visible-fringes t
        modus-operandi-theme-variable-pitch-headings t
        modus-operandi-theme-distinct-org-blocks t
        modus-operandi-theme-rainbow-headings t
        modus-operandi-theme-scale-headings t)
  :config
  (load-theme 'modus-operandi t))

(use-package modus-vivendi-theme
  :ensure
  :init
  (setq modus-vivendi-theme-slanted-constructs t
        modus-vivendi-theme-bold-constructs t
        modus-vivendi-theme-visible-fringes t
        modus-vivendi-theme-variable-pitch-headings t
        modus-vivendi-theme-distinct-org-blocks t
        modus-vivendi-theme-rainbow-headings t
        modus-vivendi-theme-scale-headings t))

;;;; Base typeface configurations

;;;;; Primary and complementary font settings

(use-package emacs
  :defines (prot/font-set-fonts-hook
            prot/font-monospaced-fonts-list
            prot/font-fonts-line-spacing-alist
            prot/font-fonts-bold-weight-alist)
  :functions (prot/font-adjustment
              prot/font-line-spacing
              prot/font-bold-face
              prot/font-fonts-per-monitor)
  :commands (prot/font-set-face-attribute
             prot/font-set-fonts
             prot/font-set-font-size-family
             prot/font-fonts-dwim)
  :config
  (setq x-underline-at-descent-line t)
  (setq underline-minimum-offset 1)

  (defvar prot/font-switch-fonts-hook nil
    "Hook that is called after setting fonts.
See, for example, `prot/font-set-fonts'.")

  (defconst prot/font-sizes-families-alist
    '(("laptop" . (105 "Hack" "Noto Sans"))
      ("desktop" . (105 "Hack" "Noto Sans")))
    "Alist of desired typefaces and their point sizes.

Each association consists of a display type mapped to a point
size, followed by monospaced and proportionately spaced font
names.  The monospaced typeface is meant to be applied to the
`default' and `fixed-pitch' faces.  The proportionately spaced
font is intended for the `variable-pitch' face.")

  (defun prot/font-set-face-attribute (face family)
    "Set FACE font to FAMILY."
    (set-face-attribute `,face nil :family (format "%s" family)))

  (defun prot/font-set-fonts (&optional points font-mono font-var)
    "Set default font size using presets.

POINTS is the font's point size, represented as either '10' or
'10.5'.  FONT-MONO should be a monospaced typeface, due to the
alignment requirements of the `fixed-pitch' face.  FONT-VAR could
be a proportionately spaced typeface or even a monospaced one,
since the `variable-pitch' it applies to is not supposed to be
spacing-sensitive.  Both families must be represented as a string
holding the family's name."
    (interactive)
    (if window-system
        (let* ((data prot/font-sizes-families-alist)
               (displays (mapcar #'car data))
               (choice (or points
                           (completing-read "Pick display size: " displays nil t)))
               (size (or points
                         (nth 1 (assoc `,choice data))))
               (mono (or font-mono
                         (if (member choice displays)
                             (nth 2 (assoc `,choice data))
                           nil)))
               (var (or font-var
                        (if (member choice displays)
                            (nth 3 (assoc `,choice data))
                          nil)))
               (adjust (nth 4 (assoc `,choice data))))
          (set-face-attribute 'default nil :family mono :height size)
          (prot/font-set-face-attribute 'fixed-pitch mono)
          (prot/font-set-face-attribute 'variable-pitch var)
          (run-hooks 'prot/font-set-fonts-hook))
      (user-error "Not running a graphical Emacs.  Cannot set fonts.")))

  (defvar prot/font-monospaced-fonts-list
    '("Hack" "Iosevka Slab" "Source Code Pro"
      "Ubuntu Mono" "Fantasque Sans Mono" "DejaVu Sans Mono"
      "Fira Code" "Roboto Mono")
    "List of typefaces for coding.
See `prot/font-set-font-size-family' for how this is used
code-wise.")

  (defun prot/font-set-font-size-family ()
    "Set point size and main typeface.
This command is intended for testing various font families at
some common point sizes.

See `prot/font-set-fonts' for the function I would normally use
or `prot/font-fonts-dwim' which just wraps this one with that."
    (interactive)
    (if window-system
        (let* ((fonts prot/font-monospaced-fonts-list)
               (font (completing-read "Select main font: " fonts nil t))
               (nums (list 100 110 120 130))
               (sizes (mapcar 'number-to-string nums))
               (size (completing-read "Select or insert number: " sizes nil))
               (var (face-attribute 'variable-pitch :family)))
          (set-face-attribute 'default nil :family font :height (string-to-number size))
          (prot/font-set-face-attribute 'variable-pitch var)
          (run-hooks 'prot/font-set-fonts-hook))
      (user-error "Not running a graphical Emacs.  Cannot set fonts.")))

  (defun prot/font-fonts-dwim (&optional arg)
    "Set fonts interactively.
This is just a wrapper around `prot/font-set-fonts' and
`prot/font-set-font-size-family', whose sole purpose is to
economise on dedicated key bindings."
    (interactive "P")
    (if arg
        (prot/font-set-font-size-family)
      (prot/font-set-fonts)))

  (defvar prot/font-fonts-line-spacing-alist
    '(("Hack" . 1)
      ("Source Code Pro" . 1)
      ("Ubuntu Mono" . 2))
    "Font families in need of extra `line-spacing'.
See `prot/font-line-spacing' for how this is used.")

  (defvar prot/font-fonts-bold-weight-alist
    '(("Hack" . semibold)
      ("Iosevka Comfy" . semibold)
      ("Source Code Pro" . semibold))
    "Font families in need of a variegated weight for `bold'.
See `prot/font-bold-face' for how this is used.")

  (defmacro prot/font-adjustment (fn doc alist cond1 cond2)
    "Macro for functions that employ `prot/font-switch-fonts-hook'.
NAME is the name of the resulting function.  DOC is its
docstring.  ALIST is an assosiation list of cons cells.  COND1
and COND2 is the body of an `if' statement's 'if' and 'then' part
respectively."
    `(defun ,fn ()
       ,doc
       (let* ((data ,alist)
              (fonts (mapcar #'car data))
              (font (face-attribute 'default :family))
              (x (cdr (assoc font data))))
         (if (member font fonts)
             ,cond1
           ,cond2))))

  (prot/font-adjustment
   prot/font-line-spacing
   "Determine desirable `line-spacing', based on font family."
   prot/font-fonts-line-spacing-alist
   (setq-default line-spacing `,x)
   (setq-default line-spacing nil))

  ;; XXX This will not work with every theme, but only those that
  ;; inherit the `bold' face instead of specifying a weight property.
  ;; The intent is to configure this once and have it propagate wherever
  ;; a heavier weight is displayed.  My Modus themes handle this
  ;; properly.
  (prot/font-adjustment
   prot/font-bold-face
   "Determine weight for the `bold' face, based on font family."
   prot/font-fonts-bold-weight-alist
   (set-face-attribute 'bold nil :weight `,x)
   (set-face-attribute 'bold nil :weight 'bold))

  (defun prot/font-fonts-per-monitor ()
    "Use font settings based on screen size.
Meant to be used at some early initialisation stage, such as with
`after-init-hook'."
    (when window-system
      (let* ((display (if (<= (display-pixel-width) 1600)
                          "laptop"
                        "desktop"))
             (data prot/font-sizes-families-alist)
             (size (cadr (assoc `,display data)))
             (mono (nth 2 (assoc `,display data)))
             (var (nth 3 (assoc `,display data))))
        (set-face-attribute 'default nil :family mono :height size)
        (prot/font-set-face-attribute 'fixed-pitch mono)
        (prot/font-set-face-attribute 'variable-pitch var))
      (run-hooks 'prot/font-switch-fonts-hook)))

  :hook ((after-init-hook . prot/font-fonts-per-monitor)
         (prot/font-set-fonts-hook . prot/font-line-spacing)
         (prot/font-set-fonts-hook . prot/font-bold-face)
         ;; See theme section for this hook
         (prot/modus-theme-after-load-hook . prot/font-bold-face))
  ;; Awkward key because I do not need it very often.  Maybe once a day.
  ;; The "C-c f" is used elsewhere.
  :bind ("C-c F" . prot/font-fonts-dwim))

;;;;; Mode for proportional fonts

(use-package face-remap
  :diminish buffer-face-mode            ; the actual mode
  :commands prot/variable-pitch-mode
  :config
  (define-minor-mode prot/variable-pitch-mode
    "Toggle `variable-pitch-mode', except for `prog-mode'."
    :init-value nil
    :global nil
    (if prot/variable-pitch-mode
        (unless (derived-mode-p 'prog-mode)
          (variable-pitch-mode 1))
      (variable-pitch-mode -1))))

;;; Selection candidates and search methods

;;;; Completion framework and extras

;;;;; Tools for completion candidate listing

;;;;;; Minibuffer configurations and extras

(use-package minibuffer
  :config
  ;;; General minibuffer configurations
  ;; Super-powerful completion style for out-of-order groups of matches
  ;; using a comprehensive set of matching styles.
  (use-package orderless
    :ensure
    :config
    (setq orderless-regexp-separator "[/\s_-]+")
    (setq orderless-matching-styles
          '(orderless-flex
            orderless-strict-leading-initialism
            orderless-regexp
            orderless-prefixes
            orderless-literal))

    (defun prot/orderless-literal-dispatcher (pattern _index _total)
      (when (string-suffix-p "=" pattern)
        `(orderless-literal . ,(substring pattern 0 -1))))

    (defun prot/orderless-initialism-dispatcher (pattern _index _total)
      (when (string-suffix-p "," pattern)
        `(orderless-strict-leading-initialism . ,(substring pattern 0 -1))))

    (setq orderless-style-dispatchers
          '(prot/orderless-literal-dispatcher
            prot/orderless-initialism-dispatcher))
    :bind (:map minibuffer-local-completion-map
                ("SPC" . nil)))       ; Space should never complete: use
                                        ; it for `orderless' groups.

  (setq completion-styles
        '(orderless partial-completion))
  (setq completion-category-defaults nil)
  (setq completion-cycle-threshold 3)
  (setq completion-flex-nospace nil)
  (setq completion-pcm-complete-word-inserts-delimiters t)
  (setq completion-pcm-word-delimiters "-_./:| ")
  (setq completion-show-help nil)
  (setq completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq completions-format 'vertical)   ; *Completions* buffer
  (setq enable-recursive-minibuffers t)
  (setq read-answer-short t)
  (setq resize-mini-windows t)

  (file-name-shadow-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1)

  ;;; General minibuffer functions
  (defun prot/focus-minibuffer ()
    "Focus the active minibuffer.

Bind this to `completion-list-mode-map' to M-v to easily jump
between the list of candidates present in the \\*Completions\\*
buffer and the minibuffer (because by default M-v switches to the
completions if invoked from inside the minibuffer."
    (interactive)
    (let ((mini (active-minibuffer-window)))
      (when mini
        (select-window mini))))

  (defun prot/focus-minibuffer-or-completions ()
    "Focus the active minibuffer or the \\*Completions\\*.

If both the minibuffer and the Completions are present, this
command will first move per invocation to the former, then the
latter, and then continue to switch between the two.

The continuous switch is essentially the same as running
`prot/focus-minibuffer' and `switch-to-completions' in
succession."
    (interactive)
    (let* ((mini (active-minibuffer-window))
           (completions (get-buffer-window "*Completions*")))
      (cond ((and mini
                  (not (minibufferp)))
             (select-window mini nil))
            ((and completions
                  (not (eq (selected-window)
                           completions)))
             (select-window completions nil)))))

  ;;; Completions' buffer actions
  ;; NOTE In practice I only use those while inspecting a long list
  ;; produced by C-h {f,o,v}.  To pop the Completions buffer, use
  ;; `minibuffer-completion-help', by default bound to ? from inside the
  ;; minibuffer.

  (defun prot/completions-kill-save-symbol ()
    "Add symbol-at-point to the kill ring.

Intended for use in the \\*Completions\\* buffer.  Bind this to a
key in `completion-list-mode-map'."
    (interactive)
    (kill-new (thing-at-point 'symbol)))

  (defmacro prot/completions-buffer-act (name doc &rest body)
    `(defun ,name ()
       ,doc
       (interactive)
       (let ((completions-window (get-buffer-window "*Completions*"))
             (completions-buffer (get-buffer "*Completions*"))
             (symbol (thing-at-point 'symbol)))
         (if (window-live-p completions-window)
             (with-current-buffer completions-buffer
               ,@body)
           (user-error "No live window with Completions")))))

  (prot/completions-buffer-act
   prot/completions-kill-symbol-at-point
   "Add \"Completions\" buffer symbol-at-point to the kill ring."
   (kill-new `,symbol)
   (message "Copied %s to kill-ring"
            (propertize `,symbol 'face 'success)))

  (prot/completions-buffer-act
   prot/completions-insert-symbol-at-point
   "Add \"Completions\" buffer symbol-at-point to active window."
   (let ((window (window-buffer (get-mru-window))))
     (with-current-buffer window
       (insert `,symbol)
       (message "Inserted %s"
                (propertize `,symbol 'face 'success)))))

  (prot/completions-buffer-act
   prot/completions-insert-symbol-at-point-exit
   "Like `prot/completions-insert-symbol-at-point' plus exit."
   (prot/completions-insert-symbol-at-point)
   (top-level))

  ;;; Miscellaneous functions and key bindings

  ;; Technically, this is not specific to the minibuffer, but I define
  ;; it here so that you can see how it is also used from inside the
  ;; "Completions" buffer
  (defun prot/describe-symbol-at-point (&optional arg)
    "Get help (documentation) for the symbol at point.

With a prefix argument, switch to the *Help* window.  If that is
already focused, switch to the most recently used window
instead."
    (interactive "P")
    (let ((symbol (symbol-at-point)))
      (when symbol
        (describe-symbol symbol)))
    (when arg
      (let ((help (get-buffer-window "*Help*")))
        (when help
          (if (not (eq (selected-window) help))
              (select-window help)
            (select-window (get-mru-window)))))))

  ;; Defines, among others, aliases for common minibuffer commands to
  ;; Super-KEY.  Normally these should go in individual package
  ;; declarations, but their grouping here makes things easier to
  ;; understand.  Besides, they are related to the minibuffer.
  :bind (("s-f" . find-file)
         ("s-F" . find-file-other-window)
         ("s-d" . dired)
         ("s-D" . dired-other-window)
         ("s-b" . switch-to-buffer)
         ("s-B" . switch-to-buffer-other-window)
         ("s-v" . prot/focus-minibuffer-or-completions)
         ("s-h" . prot/describe-symbol-at-point)
         ("s-H" . (lambda ()
                    (interactive)
                    (prot/describe-symbol-at-point '(4))))
         :map minibuffer-local-completion-map
         ("<return>" . minibuffer-force-complete-and-exit) ; exit with completion
         ("C-j" . exit-minibuffer)      ; force input unconditionally
         :map completion-list-mode-map
         ("h" . prot/describe-symbol-at-point)
         ("w" . prot/completions-kill-symbol-at-point)
         ("i" . prot/completions-insert-symbol-at-point)
         ("j" . prot/completions-insert-symbol-at-point-exit)
         ("n" . next-line)
         ("p" . previous-line)
         ("f" . next-completion)
         ("b" . previous-completion)
         ("M-v" . prot/focus-minibuffer)))

;;;;;; Icomplete configurations and extras

(use-package icomplete
  :demand
  :after minibuffer                     ; Read that section as well
  :config
  (setq icomplete-delay-completions-threshold 100)
  (setq icomplete-max-delay-chars 2)
  (setq icomplete-compute-delay 0.2)
  (setq icomplete-show-matches-on-no-input t)
  (setq icomplete-hide-common-prefix nil)
  (setq icomplete-prospects-height 1)
  (setq icomplete-separator (propertize " · " 'face 'shadow))
  ;; (setq icomplete-separator " │ ")
  ;; (setq icomplete-separator " ┆ ")
  ;; (setq icomplete-separator " ¦ ")
  ;; (setq icomplete-separator " ┆ ")
  (setq icomplete-with-completion-tables t)
  (setq icomplete-tidy-shadowed-file-names t)

  (when (boundp 'fido-mode)
    (fido-mode -1))                        ; Emacs 27.1
  (icomplete-mode 1)

  (defun prot/icomplete-minibuffer-truncate ()
    "Truncate minibuffer lines in `icomplete-mode'.
This should only affect the horizontal layout and is meant to
enforce `icomplete-prospects-height' being set to 1, which is
what I always want.

Hook it to `icomplete-minibuffer-setup-hook'."
    (when (and (minibufferp)
               (bound-and-true-p icomplete-mode))
      (setq truncate-lines t)))

  ;;; Minibuffer actions
  ;; For a fully fledged package that covers this niche and offers lots
  ;; of added functionality, check Omar Antolín Camarena's "embark"
  ;; library: https://github.com/oantolin/embark
  ;;
  ;; My idea here is to implement the three actions I had always relied
  ;; on, because they are the only ones I ever use.
  (defmacro prot/minibuffer-completion-act (name doc &rest body)
    `(defun ,name ()
       (interactive)
       (let ((candidate (car completion-all-sorted-completions)))
         (when (and (minibufferp)
                    (bound-and-true-p icomplete-mode))
           ,@body))))

  (prot/minibuffer-completion-act
   prot/minibuffer-kill-completion
   "Place minibuffer candidate to the top of the `kill-ring'."
   (kill-new `,candidate)
   (message "Copied %s to kill-ring" (propertize `,candidate 'face 'success)))

  (prot/minibuffer-completion-act
   prot/minibuffer-insert-completion
   "Insert minibuffer candidate in last active window."
   (with-minibuffer-selected-window (insert `,candidate)))

  (prot/minibuffer-completion-act
   prot/minibuffer-insert-completion-exit
   "Like `prot/minibuffer-insert-completion' but exit minibuffer."
   (prot/minibuffer-insert-completion)
   (top-level))

  (define-prefix-command 'prot/minibuffer-completion-map)

  (define-key prot/minibuffer-completion-map
    (kbd "w") 'prot/minibuffer-kill-completion)
  (define-key prot/minibuffer-completion-map
    (kbd "i") 'prot/minibuffer-insert-completion)
  (define-key prot/minibuffer-completion-map
    (kbd "j") 'prot/minibuffer-insert-completion-exit)

  (define-key minibuffer-local-completion-map
    (kbd "M-o") prot/minibuffer-completion-map)

  ;; Note that the the syntax for `use-package' hooks is controlled by
  ;; the `use-package-hook-name-suffix' variable.  The "-hook" suffix is
  ;; not an error of mine.
  :hook (icomplete-minibuffer-setup-hook . prot/icomplete-minibuffer-truncate)
  :bind (:map icomplete-minibuffer-map
              ("<tab>" . icomplete-force-complete)
              ("<return>" . icomplete-force-complete-and-exit) ; exit with completion
              ("C-j" . exit-minibuffer) ; force input unconditionally
              ("C-n" . icomplete-forward-completions)
              ("<right>" . icomplete-forward-completions)
              ("<down>" . icomplete-forward-completions)
              ("C-p" . icomplete-backward-completions)
              ("<left>" . icomplete-backward-completions)
              ("<up>" . icomplete-backward-completions)
              ;; The following command is from Emacs 27.1
              ("<C-backspace>" . icomplete-fido-backward-updir)))

;;;;;;; Icomplete vertical mode

(use-package icomplete-vertical
  :ensure
  :demand
  :after (minibuffer icomplete)
  :config
  (setq icomplete-vertical-prospects-height (/ (frame-height) 6))
  (icomplete-vertical-mode -1)

  (defun prot/kill-ring-yank-complete ()
    "Insert the selected `kill-ring' item directly at point.
When region is active, `delete-region'.

Sorting of the `kill-ring' is disabled.  Items appear as they
normally would when calling `yank' followed by `yank-pop'."
    (interactive)
    (let ((kills                    ; do not sort items
           (lambda (string pred action)
             (if (eq action 'metadata)
                 '(metadata (display-sort-function . identity)
                            (cycle-sort-function . identity))
               (complete-with-action
                action kill-ring string pred)))))
      (icomplete-vertical-do
          (:separator 'dotted-line :height (/ (frame-height) 4))
        (when (use-region-p)
          (delete-region (region-beginning) (region-end)))
        (insert
         (completing-read "Yank from kill ring: " kills nil t)))))

  :bind (("s-y" . prot/kill-ring-yank-complete)
         :map icomplete-minibuffer-map
         ("C-v" . icomplete-vertical-toggle)))

;;;;; Imenu (dynamic completion-based buffer navigation)

(use-package imenu
  :config
  (setq imenu-use-markers t)
  (setq imenu-auto-rescan t)
  (setq imenu-auto-rescan-maxout 600000)
  (setq imenu-max-item-length 100)
  (setq imenu-use-popup-menu nil)
  (setq imenu-eager-completion-buffer t)
  (setq imenu-space-replacement " ")
  (setq imenu-level-separator "/")

  (defun prot/imenu-vertical ()
    "Use a vertical Icomplete layout for `imenu'.
Also configure the value of `orderless-matching-styles' to avoid
aggressive fuzzy-style matching for this particular command."
    (interactive)
    (let ((orderless-matching-styles    ; make sure to check `orderless'
           '(orderless-literal
             orderless-regexp
             orderless-prefixes)))
      (icomplete-vertical-do (:height (/ (frame-height) 4))
        (call-interactively 'imenu))))

  (defun prot/imenu-recenter-pulse ()
    "Recent `imenu' position at the top with subtle feedback.
Add this to `imenu-after-jump-hook'."
    (let ((pulse-delay .05))
      (recenter 0)
      (prot/pulse-line)))

  (defun prot/imenu-show-entry ()
    "Reveal index at point after successful `imenu' execution.
To be used with `imenu-after-jump-hook'."
    (cond
     ((and (eq major-mode 'org-mode)
           (org-at-heading-p))
      (org-show-entry)
      (org-reveal t))
     ((when prot/outline-minor-mode
        (outline-show-entry)))))

  :hook ((imenu-after-jump-hook . prot/imenu-recenter-pulse)
         (imenu-after-jump-hook . prot/imenu-show-entry))
  :bind ("C-." . prot/imenu-vertical))


;;;;;; Imenu-list (dedicated sidebar)

(use-package imenu-list
  :ensure
  :after imenu
  :config
  (defun prot/imenu-list-dwim (&optional arg)
    "Convenience wrapper for `imenu-list'.
Move between the current buffer and a dedicated window with the
contents of `imenu'.

The dedicated window is created if it does not exist, while it is
updated once it is focused again through this command.

With \\[universal-argument] toggle the display of the window."
    (interactive "P")
    (if arg
        (imenu-list-smart-toggle)
      (with-current-buffer
          (if (eq major-mode 'imenu-list-major-mode)
              (pop-to-buffer (other-buffer (current-buffer) t))
            (imenu-list)))))

  :bind ("C-," . prot/imenu-list-dwim))

;;;;;; Flimenu (flat imenu index)

(use-package flimenu
  :ensure
  :after imenu
  :config
  (flimenu-global-mode 1))

;;;;; Generic completion for projects and directory trees

(use-package project
  :config
  (defun prot/find-file-vc-or-dir (&optional arg)
    "Find file by name that belongs to the current project or dir.
With \\[universal-argument] match files by contents.  This
requires the command-line executable called 'rg' or 'ripgrep'."
    (interactive "P")
    (let* ((default-directory (file-name-directory
                               (or (locate-dominating-file "." ".git" )
                                   default-directory))))
      (if arg
          (let* ((regexp (read-regexp
                          (concat "File contents matching REGEXP in "
                                  (propertize default-directory 'face 'bold)
                                  ": ")))
                 (results (process-lines "rg" "-l" "--hidden" "-m" "1" "-M" "120" regexp)))
            (find-file
             (icomplete-vertical-do ()
               (completing-read (concat
                                 "Files with contents matching "
                                 (propertize regexp 'face 'success)
                                 (format " (%s)" (length results))
                                 ": ")
                                results nil t))))
        (let* ((filenames-all (directory-files-recursively default-directory ".*" nil t))
               (filenames (cl-remove-if (lambda (x)
                                          (string-match-p "\\.git" x))
                                        filenames-all)))
          (icomplete-vertical-do ()
            (find-file
             (completing-read "Find file recursively: " filenames nil t)))))))

  (defun prot/find-project (&optional arg)
    "Switch to sub-directory at the specified locations.
With \\[universal-argument] produce a `dired' buffer instead with
all the possible candidates."
    (interactive "P")
    (let* ((dirs (list "~/Git/Projects/" "~/.emacs.d/prot-dev/"))
           (dotless directory-files-no-dot-files-regexp)
           (cands (mapcan (lambda (d)
                            (directory-files d t dotless))
                          dirs))
           (projects (mapcar 'abbreviate-file-name cands))
           (buf "*Projects Dired*"))
      (if arg
          (dired (cons (generate-new-buffer-name buf) projects))
        (icomplete-vertical-do ()
          (dired
           (completing-read "Find project: " projects nil t))))))

  :bind (("M-s p" . prot/find-project)
         ("M-s f" . prot/find-file-vc-or-dir)
         ("M-s l" . find-library)))

;;;;; In-buffer completions

(use-package emacs
  :config
  (defun contrib/completing-read-in-region (start end collection &optional predicate)
    "Prompt for completion of region in the minibuffer if non-unique.
Use as a value for `completion-in-region-function'."
    (if (and (minibufferp) (not (string= (minibuffer-prompt) "Eval: ")))
        (completion--in-region start end collection predicate)
      (let* ((initial (buffer-substring-no-properties start end))
             (limit (car (completion-boundaries initial collection predicate "")))
             (all (completion-all-completions initial collection predicate
                                              (length initial)))
             (completion (cond
                          ((atom all) nil)
                          ((and (consp all) (atom (cdr all)))
                           (concat (substring initial 0 limit) (car all)))
                          (t (completing-read
                              "Completion: " collection predicate t initial)))))
        (if (null completion)
            (progn (message "No completion") nil)
          (delete-region start end)
          (insert completion)
          t))))

  (setq completion-in-region-function #'contrib/completing-read-in-region)
  :bind (:map minibuffer-local-completion-map
              ("<tab>" . minibuffer-force-complete)))

;;;;;; Dabbrev (dynamic word completion)

(use-package dabbrev
  :after (minibuffer icomplete icomplete-vertical) ; read those as well
  :config
  (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
  (setq dabbrev-abbrev-skip-leading-regexp "[$*/=']")
  (setq dabbrev-backward-only nil)
  (setq dabbrev-case-distinction 'case-replace)
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-case-replace 'case-replace)
  (setq dabbrev-check-other-buffers t)
  (setq dabbrev-eliminate-newlines nil)
  (setq dabbrev-upcase-means-case-search t)
  :bind (("M-/" . dabbrev-expand)
         ("C-M-/" . dabbrev-completion)
         ("s-/" . dabbrev-completion)))

;;;; Configurations for—or extensions to—built-in search commands

;;;;; Isearch configurations and enhancements

(use-package isearch
  :diminish
  :config
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

  (defun prot/isearch-mark-and-exit ()
    "Mark the current search string and exit the search."
    (interactive)
    (push-mark isearch-other-end t 'activate)
    (setq deactivate-mark nil)
    (isearch-done))

  (defun prot/isearch-other-end ()
    "End current search in the opposite side of the match.
Particularly useful when the match does not fall within the
confines of word boundaries (e.g. multiple words)."
    (interactive)
    (isearch-done)
    (when isearch-other-end
      (goto-char isearch-other-end)))

  (defun prot/isearch-abort-dwim ()
    "Delete failed `isearch' input, single char, or cancel search.

This is a modified variant of `isearch-abort' that allows us to
perform the following, based on the specifics of the case: (i)
delete the entirety of a non-matching part, when present; (ii)
delete a single character, when possible; (iii) exit current
search if no character is present and go back to point where the
search started."
    (interactive)
    (if (eq (length isearch-string) 0)
        (isearch-cancel)
      (isearch-del-char)
      (while (or (not isearch-success) isearch-error)
        (isearch-pop-state)))
    (isearch-update))

  (defun prot/isearch-query-replace-symbol-at-point ()
    "Run `query-replace-regexp' for the symbol at point."
    (interactive)
    (isearch-forward-symbol-at-point)
    (isearch-query-replace-regexp))

  (defmacro prot/isearch-occurrence (name edge &optional doc)
    "Construct function for moving to `isearch' occurrence.
NAME is the name of the function.  EDGE is either the beginning
or the end of the buffer.  Optional DOC is the resulting
function's docstring."
    `(defun ,name (&optional arg)
       ,doc
       (interactive "p")
       (let ((x (if arg arg 1))
             (command (intern (format "isearch-%s-of-buffer" ,edge))))
         (isearch-forward-symbol-at-point)
         (funcall command x))))

  (prot/isearch-occurrence
   prot/isearch-beginning-of-buffer
   "beginning"
   "Run `isearch-beginning-of-buffer' for the symbol at point.
With numeric ARG, move to ARGth occurrence counting from the
beginning of the buffer.")

  (prot/isearch-occurrence
   prot/isearch-end-of-buffer
   "end"
   "Run `isearch-end-of-buffer' for the symbol at point.
With numeric ARG, move to ARGth occurrence counting from the
end of the buffer.")

  :bind (("M-s %" . prot/isearch-query-replace-symbol-at-point)
         ("M-s M-<" . prot/isearch-beginning-of-buffer)
         ("M-s M->" . prot/isearch-end-of-buffer)
         :map minibuffer-local-isearch-map
         ("M-/" . isearch-complete-edit)
         :map isearch-mode-map
         ("C-g" . isearch-cancel)       ; instead of `isearch-abort'
         ("M-/" . isearch-complete)
         ("C-SPC" . prot/isearch-mark-and-exit)
         ("<backspace>" . prot/isearch-abort-dwim)
         ("<C-return>" . prot/isearch-other-end)))

;;;;; Occur (replace.el)

(use-package replace
  :config
  (setq list-matching-lines-jump-to-current-line t)
  ;; See my "Modus themes" for these inherited faces
  (setq list-matching-lines-buffer-name-face
        '(:inherit modus-theme-intense-neutral :weight bold))
  (setq list-matching-lines-current-line-face
        '(:inherit modus-theme-special-mild))

  (defun prot/occur-url ()
    "Produce list with all URLs in the current buffer."
    (interactive)
    (let ((urls browse-url-button-regexp))
      (occur urls "\\&")))

  (defun prot/occur-browse-url-in-buffer ()
    "Run `eww' on a URL from the buffer using completion.
Also see `prot/occur-url'."
    (interactive)
    (let ((matches nil))
      (save-excursion
        (goto-char (point-min))
        (while (search-forward-regexp browse-url-button-regexp nil t)
          (push (match-string-no-properties 0) matches)))
      (icomplete-vertical-do
          (:height (/ (frame-height) 4) :separator 'dotted-line)
        (eww
         (completing-read "Browse URL: " matches nil t)))))

  (defun prot/occur-visit-or-list-urls (&optional arg)
    "Wrap `prot/occur-visit-or-list-urls' and `prot/occur-url'.
Meant to economise on key bindings."
    (interactive "P")
    (if arg
        (prot/occur-url)
      (prot/occur-browse-url-in-buffer)))

  :hook ((occur-mode-hook . hl-line-mode)
         (occur-mode-hook . (lambda ()
                              (toggle-truncate-lines t))))
  :bind (("M-s u" . prot/occur-visit-or-list-urls)
         ("M-s M-o" . multi-occur)
         :map occur-mode-map
         ("t" . toggle-truncate-lines)))

;;;;; Regular expressions: re-builder and visual-regexp

(use-package re-builder
  :config
  (setq reb-re-syntax 'read))

(use-package visual-regexp
  :ensure
  :config
  (setq vr/default-replace-preview nil)
  (setq vr/match-separator-use-custom-face t))

;;;;; wgrep (writable grep)

(use-package wgrep
  :ensure
  :commands wgrep
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t))

;;;;; ripgrep (rg.el)

(use-package rg
  :ensure
  :config
  (setq rg-group-result t)
  (setq rg-hide-command t)
  (setq rg-show-columns nil)
  (setq rg-show-header t)
  (setq rg-custom-type-aliases nil)
  (setq rg-default-alias-fallback "all")

  (rg-define-search prot/rg-vc-or-dir
    "RipGrep in project root or present directory."
    :query ask
    :format regexp
    :files "everything"
    :dir (let ((vc (vc-root-dir)))
           (if vc
               vc                         ; search root project dir
             default-directory))          ; or from the current dir
    :confirm prefix
    :flags ("--hidden -g !.git"))

  (rg-define-search prot/rg-ref-in-dir
    "RipGrep for thing at point in present directory."
    :query point
    :format regexp
    :files "everything"
    :dir default-directory
    :confirm prefix
    :flags ("--hidden -g !.git"))

  (defun prot/rg-save-search-as-name ()
    "Save `rg' buffer, naming it after the current search query.

This function is meant to be mapped to a key in `rg-mode-map'."
    (interactive)
    (let ((pattern (car rg-pattern-history)))
      (rg-save-search-as-name (concat "«" pattern "»"))))

  :bind (("M-s g" . prot/rg-vc-or-dir)
         ("M-s r" . prot/rg-ref-in-dir)
         :map rg-mode-map
         ("s" . prot/rg-save-search-as-name)
         ("C-n" . next-line)
         ("C-p" . previous-line)
         ("M-n" . rg-next-file)
         ("M-p" . rg-prev-file)))

;;; General movements and motions

;;;; Custom commands in line with simple.el and lisp.el

;;;;; Custom commands for lines

(use-package emacs
  :config
  (defun prot/new-line-below (&optional arg)
    "Create an empty line below the current one.
Move the point to the absolute beginning.  Adapt indentation by
passing \\[universal-argument].  Also see `prot/new-line-above'."
    (interactive "P")
    (end-of-line)
    (if arg
        (newline-and-indent)
      (newline)))

  (defun prot/new-line-above (&optional arg)
    "Create an empty line above the current one.
Move the point to the absolute beginning.  Adapt indentation by
passing \\[universal-argument]."
    (interactive "P")
    (let ((indent (if arg arg nil)))
      (if (or (bobp)
              (line-number-at-pos 1))
          (progn
            (beginning-of-line)
            (newline)
            (forward-line -1))
        (forward-line -1)
        (prot/new-line-below indent))))

  (defun prot/copy-line-or-region (&optional arg)
    "Kill-save the current line or active region.
With \\[universal-argument] duplicate the target instead.  When
region is active, also apply context-aware indentation while
duplicating."
    (interactive "P")
    (let* ((rbeg (region-beginning))
           (rend (region-end))
           (pbol (point-at-bol))
           (peol (point-at-eol))
           (indent (if (eq (or rbeg rend) pbol) nil arg)))
      (if arg
          (progn
            (if (use-region-p)
                (progn
                  (copy-region-as-kill rbeg rend)
                  (when (eq (point) rbeg)
                    (exchange-point-and-mark))
                  (prot/new-line-below indent))
              (copy-region-as-kill pbol peol)
              (prot/new-line-below))
            (yank))
        (copy-region-as-kill pbol peol)
        (message "Current line copied"))))

  (defun prot/yank-replace-line-or-region ()
    "Replace line or region with latest kill.
This command can then be followed by the standard
`yank-pop' (default is bound to \\[yank-pop])."
    (interactive)
    (if (use-region-p)
        (delete-region (region-beginning) (region-end))
      (delete-region (point-at-bol) (point-at-eol)))
    (yank))

  (defun prot/multi-line-next ()
    "Move point 15 lines down."
    (interactive)
    (forward-line 15))

  (defun prot/multi-line-prev ()
    "Move point 15 lines up."
    (interactive)
    (forward-line -15))

  (defun prot/kill-line-backward ()
    "Kill from point to the beginning of the line."
    (interactive)
    (kill-line 0))

  ;; Based on `org--line-empty-p'.
  (defmacro prot/line-p (name regexp)
    "Make NAME function to match REGEXP on line n from point."
    `(defun ,name (n)
       (save-excursion
         (and (not (bobp))
	          (or (beginning-of-line n) t)
	          (save-match-data
	            (looking-at ,regexp))))))

  (prot/line-p
   prot/empty-line-p
   "[\s\t]*$")

  (prot/line-p
   prot/indent-line-p
   "^[\s\t]+")

  (prot/line-p
   prot/non-empty-line-p
   "^.*$")

  (prot/line-p
   prot/text-list-line-p
   "^\\([\s\t#*+]+\\|[0-9]+[).]+\\)")

  (prot/line-p
   prot/text-heading-line-p
   "^[=-]+")

  :bind (("C-S-w" . prot/copy-line-or-region)
         ("C-S-y" . prot/yank-replace-line-or-region)
         ("M-SPC" . cycle-spacing)
         ("M-o" . delete-blank-lines)   ; alias for C-x C-o
         ("M-k" . prot/kill-line-backward)
         ("C-S-n" . prot/multi-line-next)
         ("C-S-p" . prot/multi-line-prev)
         ("<C-return>" . prot/new-line-below)
         ("<C-S-return>" . prot/new-line-above)))

;;;;; Custom commands for text insertion or manipulation

(use-package emacs
  :config
  ;; Got those numbers from `string-to-char'
  (defconst prot/insert-pair-alist
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
      ("_ underscores" . (95 95)))           ; _ _
    "Alist of pairs for use with `prot/insert-pair-completion'.")

  (defun prot/insert-pair-completion (&optional arg)
    "Insert pair from `prot/insert-pair-alist'."
    (interactive "P")
    (let* ((data prot/insert-pair-alist)
           (chars (mapcar #'car data))
           (choice (completing-read "Select character: " chars nil t))
           (left (cadr (assoc choice data)))
           (right (caddr (assoc choice data))))
      (insert-pair arg left right)))

  (defun prot/text-mode-heading (&optional arg)
    "Insert equal length heading delimiter below current line.

A heading delimiter is drawn as a series of dashes (-).  With
optional ARG, i.e. by prefixing \\[universal-argument], draw the
heading delimiter with equals signs (=).  The latter is
considered a heading level 1, while the former is level 2.

A heading delimiter is inserted only when that would not mess up
with existing headings or lists.  In such cases, point will move
to the next line.  For the purposes of this command, text that
starts with a number and no further delimiter is not consider a
list element.

This command is meant to be used in `text-mode' buffers and
derivatives, such as `markdown-mode', though not in `org-mode'."
    (interactive "P")
    (cond
     ((eq major-mode 'org-mode)
      (user-error "Do not use `prot/text-mode-heading' in `org-mode'!"))
     ((derived-mode-p 'text-mode)
      (let* ((count (- (point-at-eol) (point-at-bol)))
             (char (string-to-char (if arg "=" "-"))))
        (cond
         ((and (eobp)
               (or (prot/text-list-line-p 1)
                   (prot/text-heading-line-p 1)
                   (prot/empty-line-p 1)
                   (prot/indent-line-p 1)))
          (newline 1))
         ((or (prot/empty-line-p 1)
              (prot/indent-line-p 1))
          (prot/new-line-below))
         ((or (prot/text-list-line-p 1)
              (prot/text-heading-line-p 2))
          (if (prot/empty-line-p 3)
              (beginning-of-line 3)
            (prot/new-line-below)))
         (t
          (prot/new-line-below)
          (insert-char char count nil)
          (newline 2)))))))

  :bind (("C-'" . prot/insert-pair-completion)
         ("M-'" . prot/insert-pair-completion)
         ("<C-M-backspace>" . backward-kill-sexp)
         ("M-c" . capitalize-dwim)
         ("M-l" . downcase-dwim)        ; "lower" case
         ("M-u" . upcase-dwim)
         :map text-mode-map
         ("<M-return>" . prot/text-mode-heading)))

;;;;; Custom commands for object transposition

(use-package emacs
  :commands (prot/transpose-chars
             prot/transpose-lines
             prot/transpose-paragraphs
             prot/transpose-sentences
             prot/transpose-sexps
             prot/transpose-words)
  :config
  (defmacro prot/transpose (name scope &optional doc)
    "Macro to produce transposition functions.
NAME is the function's symbol.  SCOPE is the text object to
operate on.  Optional DOC is the function's docstring.

Transposition over an active region will swap the object at
mark (region beginning) with the one at point (region end)"
    `(defun ,name (arg)
       ,doc
       (interactive "p")
       (let ((x (format "%s-%s" "transpose" ,scope)))
         (if (use-region-p)
             (funcall (intern x) 0)
           (funcall (intern x) arg)))))

  (prot/transpose
   prot/transpose-lines
   "lines"
   "Transpose lines or swap over active region.")

  (prot/transpose
   prot/transpose-paragraphs
   "paragraphs"
   "Transpose paragraphs or swap over active region.")

  (prot/transpose
   prot/transpose-sentences
   "sentences"
   "Transpose sentences or swap over active region.")

  (prot/transpose
   prot/transpose-sexps
   "sexps"
   "Transpose balanced expressions or swap over active region.")

  (defun prot/transpose-chars ()
    "Always transposes the two characters before point.
There is no 'dragging' the character forward.  This is the
behaviour of `transpose-chars' when point is at end-of-line."
    (interactive)
    (transpose-chars -1)
    (forward-char))

  (defun prot/transpose-words (arg)
    "Transpose words.

If region is active, swap the word at mark (region beginning)
with the one at point (region end).

Otherwise, and while inside a sentence, this behaves as the
built-in `transpose-words', dragging forward the word behind the
point.  The difference lies in its behaviour at the end or
beginnning of a line, where it will always transpose the word at
point with the one behind or ahead of it (effectively the
last/first two words)."
    (interactive "p")
    (cond
     ((use-region-p)
      (transpose-words 0))
     ((eq (point) (point-at-eol))
      (transpose-words -1))
     ((eq (point) (point-at-bol))
      (forward-word 1)
      (transpose-words 1))
     (t
      (transpose-words arg))))

  :bind (("C-t" . prot/transpose-chars)
         ("C-x C-t" . prot/transpose-lines)
         ("C-S-t" . prot/transpose-paragraphs)
         ("C-x M-t" . prot/transpose-sentences)
         ("C-M-t" . prot/transpose-sexps)
         ("M-t" . prot/transpose-words)))

;;;;; Custom commands for marking syntactic constructs

(use-package emacs
  :commands (prot/mark-symbol
             prot/mark-sexp-backward)
  :config
  (defmacro prot/mark (name object &optional docstring)
    "Produce function for marking small syntactic constructs.
NAME is how the function should be called.  OBJECT is its scope.
Optional DOCSTRING describes the resulting function.

This is a slightly modified version of the built-in `mark-word'."
    `(defun ,name (&optional arg allow-extend)
       ,docstring
       (interactive "P\np")
       (let ((x (format "%s-%s" "forward" ,object)))
         (cond ((and allow-extend
                     (or (and (eq last-command this-command) (mark t))
                         (region-active-p)))
                (setq arg (if arg (prefix-numeric-value arg)
                            (if (< (mark) (point)) -1 1)))
                (set-mark
                 (save-excursion
                   (goto-char (mark))
                   (funcall (intern x) arg)
                   (point))))
               (t
                (let ((bounds (bounds-of-thing-at-point (intern ,object))))
                  (unless (consp bounds)
                    (user-error "No %s at point" ,object))
                  (if (>= (prefix-numeric-value arg) 0)
                      (goto-char (car bounds))
                    (goto-char (cdr bounds)))
                  (push-mark
                   (save-excursion
                     (funcall (intern x) (prefix-numeric-value arg))
                     (point)))
                  (activate-mark)))))))

  (prot/mark
   prot/mark-word
   "word"
   "Mark the whole word at point.
This function is a slightly modified version of the built-in
`mark-word', that I intend to use only in special circumstances,
such as when recording a keyboard macro where precision is
required.  For a general purpose utility, use `prot/mark-symbol'
instead.")

  (prot/mark
   prot/mark-symbol
   "symbol"
   "Mark the whole symbol at point.
With optional ARG, mark the current symbol and any remaining
ARGth symbols away from point.  A negative argument moves
backward. Repeated invocations of this command mark the next
symbol in the direction originally specified.

In the absence of a symbol and if a word is present at point,
this command will operate on it as described above.")

  (defun prot/mark-sexp-backward (&optional arg)
    "Mark previous or ARGth balanced expression[s].
Just a convenient backward-looking `mark-sexp'."
    (interactive "P")
    (if arg
        (mark-sexp (- arg) t)
      (mark-sexp (- 1) t)))

  (defun prot/mark-construct-dwim (&optional arg)
    "Mark symbol or balanced expression at point.
A do-what-I-mean wrapper for `prot/mark-sexp-backward',
`mark-sexp', and `prot/mark-symbol'.

When point is over a symbol, mark the entirety of it.  Regular
words are interpreted as symbols when an actual symbol is not
present.

For balanced expressions, a backward match will happen when point
is to the right of the closing delimiter.  A forward match is the
fallback condition and should work when point is before a
balanced expression, with or without whitespace in between it an
the opening delimiter.

Optional ARG will mark a total of ARGth objects while counting
the current one (so 3 would be 1+2 more).  A negative count moves
the mark backward (though that would invert the backward-moving
sexp matching of `prot/mark-sexp-backward', so be mindful of
where the point is).  Repeated invocations of this command
incrementally mark objects in the direction originally
specified."
    (interactive "P")
    (cond
     ((symbol-at-point)
      (prot/mark-symbol arg t))
     ((eq (point) (cdr (bounds-of-thing-at-point 'sexp)))
      (prot/mark-sexp-backward arg))
     (t
      (mark-sexp arg t))))

  :bind (("M-@" . prot/mark-word)       ; replaces `mark-word'
         ("C-M-SPC" . prot/mark-construct-dwim)))

;;;; Go to actionable beginning or end of buffer (beginend.el)

(use-package beginend
  :ensure
  :demand
  :diminish beginend-global-mode
  :config
  (dolist (mode beginend-modes) (diminish (cdr mode)))
  (beginend-global-mode 1))

;;;; Go to last change

(use-package goto-last-change
  :ensure
  :bind ("C-z" . goto-last-change))

;;; Directory, buffer, window management

;;;; Dired (directory editor, file manager)

;;;;; Base settings for Dired

(use-package dired
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-AGFhlv --group-directories-first --time-style=long-iso")
  (setq dired-dwim-target t)
  ;; Note that the the syntax for `use-package' hooks is controlled by
  ;; the `use-package-hook-name-suffix' variable.  The "-hook" suffix is
  ;; not an error of mine.
  :hook ((dired-mode-hook . dired-hide-details-mode)
         (dired-mode-hook . hl-line-mode)))

(use-package dired-aux
  :config
  (setq dired-isearch-filenames 'dwim)
  ;; The following variables were introduced in Emacs 27.1
  (setq dired-create-destination-dirs 'ask)
  (setq dired-vc-rename-file t)

  (defmacro prot/dired-fd (name doc prompt &rest flags)
    "Make commands for selecting 'fd' results with completion.
NAME is how the function should be named.  DOC is the function's
documentation string.  PROMPT describes the scope of the query.
FLAGS are the command-line arguments passed to the 'fd'
executable, each of which is a string."
    `(defun ,name (&optional arg)
       ,doc
       (interactive "P")
       (let* ((vc (vc-root-dir))
              (dir (expand-file-name (if vc vc default-directory)))
              (regexp (read-regexp
                       (format "%s matching REGEXP in %s: " ,prompt
                               (propertize dir 'face 'bold))))
              (names (process-lines "fd" ,@flags regexp dir))
              (buf "*FD Dired*"))
         (if names
             (if arg
                 (dired (cons (generate-new-buffer-name buf) names))
               (icomplete-vertical-do ()
                 (find-file
                  (completing-read (format "Items matching %s (%s): "
                                           (propertize regexp 'face 'success)
                                           (length names))
                                   names nil t)))))
         (user-error (format "No matches for « %s » in %s" regexp dir)))))

  (prot/dired-fd
   prot/dired-fd-dirs
   "Search for directories in VC root or PWD.
With \\[universal-argument] put the results in a `dired' buffer.
This relies on the external 'fd' executable."
   "Subdirectories"
   "-i" "-H" "-a" "-t" "d" "-c" "never")

  (prot/dired-fd
   prot/dired-fd-files-and-dirs
   "Search for files and directories in VC root or PWD.
With \\[universal-argument] put the results in a `dired' buffer.
This relies on the external 'fd' executable."
   "Files and dirs"
   "-i" "-H" "-a" "-t" "d" "-t" "f" "-c" "never")

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

  :bind (("M-s d" .  prot/dired-fd-dirs)
         ("M-s z" . prot/dired-fd-files-and-dirs)
         :map dired-mode-map
         ("C-+" . dired-create-empty-file)
         ("M-s f" . nil)
         :map minibuffer-local-filename-completion-map
         ("C-c d" . contrib/cd-bookmark)))

(use-package async :ensure)

(use-package dired-async
  :after (dired async)
  :hook (dired-mode-hook . dired-async-mode))

;;;;; Narrowed dired

(use-package dired-narrow
  :ensure
  :after dired
  :config
  (setq dired-narrow-exit-when-one-left t)
  (setq dired-narrow-enable-blinking t)
  (setq dired-narrow-blink-time 0.3)
  :bind (:map dired-mode-map
              ("/" . dired-narrow-regexp)))

;;;;; wdired (writable dired)

(use-package wdired
  :after dired
  :commands wdired-change-to-wdired-mode
  :config
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))

;;;;; peep-dired (file previews including images)

(use-package peep-dired
  :ensure
  :after dired
  :config
  (setq peep-dired-cleanup-on-disable t)
  (setq peep-dired-cleanup-eagerly t)
  (setq peep-dired-enable-on-directories nil)
  (setq peep-dired-ignored-extensions
        '("mkv" "webm" "mp4" "mp3" "ogg" "iso"))
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

;;;;; image-dired (image thumbnails and previews)

(use-package image-dired
  :config
  (setq image-dired-external-viewer "xdg-open")
  (setq image-dired-thumb-size 80)
  (setq image-dired-thumb-margin 2)
  (setq image-dired-thumb-relief 0)
  (setq image-dired-thumbs-per-row 4)
  :bind (:map image-dired-thumbnail-mode-map
              ("<return>" . image-dired-thumbnail-display-external)))

;;;;; dired-subtree (tree-style view/navigation)

(use-package dired-subtree
  :ensure
  :after dired
  :config
  (setq dired-subtree-use-backgrounds nil)
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<C-tab>" . dired-subtree-cycle)
              ("<S-iso-lefttab>" . dired-subtree-remove)))

;;;;; dired-x (extra Dired functions)

(use-package dired-x
  :after dired
  :config
  (setq dired-clean-up-buffers-too t)
  (setq dired-clean-confirm-killing-deleted-buffers t)
  (setq dired-x-hands-off-my-keys t)
  (setq dired-bind-man nil)
  (setq dired-bind-info nil)

  (defun prot/dired-jump-extra (&optional arg)
    "Switch directories comprising context and bookmarks.
NEEDS REVIEW."
    (interactive "P")
    (let* ((vc (vc-root-dir))
           (buf-name (buffer-file-name))
           (path (if buf-name
                     buf-name
                   default-directory))
           (file (abbreviate-file-name path))
           (bookmarks (mapcar (lambda (b)
                                (cdr b))
                              (contrib/cdb--bookmarked-directories)))
           (collection (append bookmarks
                               (list (file-name-directory file)
                                     (when vc vc))))
           (files (cl-remove-if (lambda (f)
                                  (eq f nil))
                                collection)))
      (icomplete-vertical-do ()
        (dired
         (completing-read "Jump to context or bookmark: " files nil t)))))

  :bind (("C-c j" . prot/dired-jump-extra)
         ("C-x C-j" . dired-jump)
         ("s-j" . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window)
         ("s-J" . dired-jump-other-window)))

;;;;; diredfl (more dired colours)

(use-package diredfl
  :ensure
  :config
  (setq diredfl-ignore-compressed-flag nil)
  :hook (dired-mode-hook . diredfl-mode))

;;;;; Dired disk usage (du)

(use-package dired-du
  :ensure
  :commands dired-du-mode)

;;;; Working with buffers

;;;;; General buffer commands

(use-package emacs
  :config
  (defun prot/kill-buffer-current (&optional arg)
    "Kill current buffer or abort recursion when in minibuffer."
    (interactive "P")
    (if (minibufferp)
        (abort-recursive-edit)
      (kill-buffer (current-buffer)))
    (when (and arg
               (not (one-window-p)))
      (delete-window)))

  (defun contrib/rename-file-and-buffer ()
    "Rename current buffer and, if available, its file."
    (interactive)
    (let ((filename (buffer-file-name)))
      (if (not (and filename (file-exists-p filename)))
          (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
        (let* ((new-name (read-from-minibuffer "New name: " filename))
               (containing-dir (file-name-directory new-name)))
          (make-directory containing-dir t)
          (cond
           ((vc-backend filename) (vc-rename-file filename new-name))
           (t
            (rename-file filename new-name t)
            (set-visited-file-name new-name t t)))))))
  :bind (("M-=" . count-words)
         ("<C-f2>" . contrib/rename-file-and-buffer)
         ("s-k" . prot/kill-buffer-current)))

;;;;; Unique names for buffers

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-strip-common-suffix t)
  (setq uniquify-after-kill-buffer-p t))

;;;;; Ibuffer and extras (dired-like buffer list manager)

(use-package ibuffer
  :config
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

  (defun prot/buffers-major-mode (&optional arg)
    "Select buffers that match the current buffer's major mode.
With \\[universal-argument] produce an `ibuffer' filtered
accordingly.  Else use standard completion."
    (interactive "P")
    (let* ((major major-mode)
           (prompt "Buffers for ")
           (mode-string (format "%s" major))
           (mode-string-pretty (propertize mode-string 'face 'success)))
      (if arg
          (ibuffer t (concat "*" prompt mode-string "*")
                   (list (cons 'used-mode major)))
        (switch-to-buffer
         (read-buffer
          (concat prompt mode-string-pretty ": ") nil t
          (lambda (pair) ; pair is (name-string . buffer-object)
            (with-current-buffer (cdr pair) (derived-mode-p major))))))))

  (defun prot/buffers-vc-root (&optional arg)
    "Select buffers that match the present `vc-root-dir'.
With \\[universal-argument] produce an `ibuffer' filtered
accordingly.  Else use standard completion.

When no VC root is available, use standard `switch-to-buffer'."
    (interactive "P")
    (let* ((root (vc-root-dir))
           (prompt "Buffers for VC ")
           (vc-string (format "%s" root))
           (vc-string-pretty (propertize vc-string 'face 'success)))
      (if root
          (if arg
              (ibuffer t (concat "*" prompt vc-string "*")
                       (list (cons 'filename (expand-file-name root))))
            (switch-to-buffer
             (read-buffer
              (concat prompt vc-string-pretty ": ") nil t
              (lambda (pair) ; pair is (name-string . buffer-object)
                (with-current-buffer (cdr pair) (string= (vc-root-dir) root))))))
        (call-interactively 'switch-to-buffer))))

  :hook (ibuffer-mode-hook . hl-line-mode)
  :bind (("M-s b" . prot/buffers-major-mode)
         ("M-s v" . prot/buffers-vc-root)
         ("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         ("* f" . ibuffer-mark-by-file-name-regexp)
         ("* g" . ibuffer-mark-by-content-regexp) ; "g" is for "grep"
         ("* n" . ibuffer-mark-by-name-regexp)
         ("s n" . ibuffer-do-sort-by-alphabetic)  ; "sort name" mnemonic
         ("/ g" . ibuffer-filter-by-content)))

;;;;;; Ibuffer integration with VC (version control framework)

(use-package ibuffer-vc
  :ensure
  :after (ibuffer vc)
  :bind (:map ibuffer-mode-map
              ("/ V" . ibuffer-vc-set-filter-groups-by-vc-root)
              ("/ <deletechar>" . ibuffer-clear-filter-groups)))

;;;; Window configuration

(use-package emacs
  :config
  (defvar prot/window-configuration nil
    "Current window configuration.
Intended for use by `prot/window-monocle'.")

  (define-minor-mode prot/window-single-toggle
    "Toggle between multiple windows and single window.
This is the equivalent of maximising a window.  Tiling window
managers such as DWM, BSPWM refer to this state as 'monocle'."
    :lighter " [M]"
    :global nil
    (if (one-window-p)
        (when prot/window-configuration
          (set-window-configuration prot/window-configuration))
      (setq prot/window-configuration (current-window-configuration))
      (delete-other-windows)))
  :bind ("s-m" . prot/window-single-toggle))

;;;;; Window rules and basic tweaks

(use-package window
  :init
  (setq display-buffer-alist
        '(;; top side window
          ("\\*Bongo-Elfeed Queue.*"
           (display-buffer-reuse-window display-buffer-in-side-window)
           (window-height . 0.16)
           (side . top)
           (slot . -2))
          ("\\*\\(elfeed-mpv-output\\|world-clock\\).*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . top)
           (slot . -1))
          ("\\*\\(Flycheck\\|Flymake\\|Package-Lint\\|vc-git :\\).*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . top)
           (slot . 0)
           (window-parameters . ((no-other-window . t))))
          ("\\*Messages.*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . top)
           (slot . 1)
           (window-parameters . ((no-other-window . t))))
          ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\)\\*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . top)
           (slot . 2)
           (window-parameters . ((no-other-window . t))))
          ;; bottom side window
          ("\\*\\(Output\\|Register Preview\\).*"
           (display-buffer-in-side-window)
           (window-width . 0.16)       ; See the :hook
           (side . bottom)
           (slot . -1)
           (window-parameters . ((no-other-window . t))))
          (".*\\*Completions.*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . bottom)
           (slot . 0)
           (window-parameters . ((no-other-window . t))))
          ("^\\(\\*e?shell\\|vterm\\).*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . bottom)
           (slot . 1))
          ;; left side window
          ("\\*Help.*"
           (display-buffer-in-side-window)
           (window-width . 0.20)       ; See the :hook
           (side . left)
           (slot . 0)
           (window-parameters . ((no-other-window . t))))
          ;; right side window
          ("\\*Faces\\*"
           (display-buffer-in-side-window)
           (window-width . 0.25)
           (side . right)
           (slot . 0)
           (window-parameters
            . ((no-other-window . t)
               (mode-line-format
                . (" "
                   mode-line-buffer-identification)))))
          ("\\*Custom.*"
           (display-buffer-in-side-window)
           (window-width . 0.25)
           (side . right)
           (slot . 1))
          ;; bottom buffer (NOT side window)
          ("\\*\\vc-\\(incoming\\|outgoing\\).*"
           (display-buffer-at-bottom))))
  (setq window-combination-resize t)
  (setq even-window-sizes 'height-only)
  (setq window-sides-vertical nil)
  (setq switch-to-buffer-in-dedicated-window 'pop)
  ;; Note that the the syntax for `use-package' hooks is controlled by
  ;; the `use-package-hook-name-suffix' variable.  The "-hook" suffix is
  ;; not an error of mine.
  :hook ((help-mode-hook . visual-line-mode)
         (custom-mode-hook . visual-line-mode))
  :bind (("s-n" . next-buffer)
         ("s-p" . previous-buffer)
         ("s-o" . other-window)
         ("s-2" . split-window-below)
         ("s-3" . split-window-right)
         ("s-0" . delete-window)
         ("s-1" . delete-other-windows)
         ("s-5" . delete-frame)
         ("C-x _" . balance-windows)
         ("C-x +" . balance-windows-area)
         ("s-q" . window-toggle-side-windows)))

;;;;; Window history (winner-mode)

(use-package winner
  :hook (after-init-hook . winner-mode)
  :bind ("<s-right>" . winner-redo)
  ("<s-left>" . winner-undo))

;;;;; Directional window motions (windmove)

(use-package windmove
  :config
  (setq windmove-create-window nil)     ; Emacs 27.1
  :bind (("C-s-k" . windmove-up)
         ("C-s-l" . windmove-right)
         ("C-s-j" . windmove-down)
         ("C-s-h" . windmove-left)
         ;; numpad keys clockwise: 8 6 2 4
         ("<kp-up>" . windmove-up)
         ("<kp-right>" . windmove-right)
         ("<kp-down>" . windmove-down)
         ("<kp-left>" . windmove-left)))

;;;;; Transposition and rotation of windows

(use-package transpose-frame
  :ensure
  :commands (transpose-frame
             flip-frame
             flop-frame
             rotate-frame
             rotate-frame-clockwise
             rotate-frame-anticlockwise)
  :bind (("C-s-t" . flop-frame) ; what I consider "transpose" in this context
         ("C-s-r" . rotate-frame-clockwise)))

;;; Applications and utilities

;;;; Calendar

(use-package calendar
  :config
  (setq calendar-mark-diary-entries-flag t)
  (setq calendar-time-display-form
        '(24-hours ":" minutes
                   (when time-zone
                     (concat " (" time-zone ")"))))
  (setq calendar-week-start-day 1)      ; Monday
  (setq calendar-date-style 'iso)
  (setq calendar-christian-all-holidays-flag nil)
  (setq calendar-holidays
        (append holiday-local-holidays  ; TODO set local holidays
                holiday-solar-holidays))

  (use-package solar
    :config
    (setq calendar-latitude 35.17
          calendar-longitude 33.36))

  (use-package lunar
    :config
    (setq lunar-phase-names
          '("New Moon"
            "First Quarter Moon"
            "Full Moon"
            "Last Quarter Moon")))

  :hook (calendar-today-visible-hook . calendar-mark-today))

;;;; Diary

(use-package diary-lib
  :config
  (setq diary-file "~/.emacs.d/diary")
  (setq diary-entry-marker "diary")
  (setq diary-show-holidays-flag t)
  (setq diary-header-line-flag nil)
  (setq diary-mail-addr "david@daporter.net")
  (setq diary-mail-days 3)
  (setq diary-number-of-entries 3)
  (setq diary-comment-start ";")
  (setq diary-comment-end "")
  (setq diary-date-forms
        '((day "/" month "[^/0-9]")
          (day "/" month "/" year "[^0-9]")
          (day " *" monthname " *" year "[^0-9]")
          (monthname " *" day "[^,0-9]")
          (monthname " *" day ", *" year "[^0-9]")
          (year "[-/]" month "[-/]" day "[^0-9]")
          (dayname "\\W"))))


;;;; Org-mode (personal information manager)

;;;;; Org basic configurations

(use-package org
  :ensure
  :config
  ;; agenda and basic directory structure
  (setq org-directory "~/org/")
  (setq org-default-notes-file "~/org/notes.org")
  (setq org-agenda-files
        '("~/gtd/inbox.org"
          "~/gtd/projects.org"))
  (setq org-archive-location "archive/%s_archive::")
  (setq org-archive-file-header-format
        "#+FILETAGS: ARCHIVE\nArchived entries from file %s\n")
  ;; refile, todo
  (setq org-refile-targets
        '(("~/gtd/projects.org" :maxlevel . 3)
          ("~/gtd/maybe.org" :level . 1)))
  (setq org-refile-use-outline-path t)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-cache t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w@/!)" "|" "DONE(d)" "CANCELLED(c@/!)")))
  (setq org-fast-tag-selection-include-todo t)
  (setq org-use-fast-todo-selection t)
  (setq org-outline-path-complete-in-steps nil)  
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-fontify-whole-heading-line nil)
  (setq org-fontify-whole-block-delimiter-line t)
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-track-ordered-property-with-tag t)
  ;; code blocks
  (setq org-confirm-babel-evaluate nil)
  ;; log
  (setq org-log-done 'time)
  (setq org-log-note-clock-out nil)
  (setq org-log-redeadline nil)
  (setq org-log-reschedule nil)
  (setq org-read-date-prefer-future 'time)
  ;; general
  (setq org-adapt-indentation nil)      ; No, thanks!
  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-hide-emphasis-markers t)
  (setq org-hide-leading-stars nil)
  (setq org-catch-invisible-edits 'show)
  (setq org-return-follows-link t)
  (setq org-loop-over-headlines-in-active-region 'start-level)
  (setq org-imenu-depth 7)
  (setq org-modules '(habit ol-gnus ol-info ol-eww))

  :bind (:map org-mode-map
              ("<C-return>" . nil)
              ("<C-S-return>" . nil)))

(use-package org-list
  :config
  (setq org-list-allow-alphabetical t))

;;;;; Org links

(use-package ol
  :config
  (setq org-link-keep-stored-after-insertion t)
  :bind (("C-c l" . org-store-link)
         :map org-mode-map
         ("C-c S-l" . org-toggle-link-display)
         ("C-c C-S-l" . org-insert-last-stored-link)))

;;;;; Org-capture templates

(use-package org-capture
  :after org
  :config
  (setq org-capture-templates
        '(("t" "Todo" entry (file "~/gtd/inbox.org")
           "* TODO %?\n%U\n%a\n")
          ("f" "Fleeting note" entry  (file "~/gtd/inbox.org")
           "* TODO Process fleeting note\n%?\n%U\n%a\n")
          ("r" "Reply to an email" entry (file+headline "~/gtd/inbox.org")
           "* TODO Reply to %:from on %:subject\n SCHEDULED: %t\n%U\n%a\n"
           :immediate-finish t)
          ("l" "link" entry (file "~/gtd/inbox.org")
           "* TODO %(org-cliplink-capture)" :immediate-finish t)
          ("c" "org-protocol-capture" entry (file "~/gtd/inbox.org")
           "* TODO [[%:link][%:description]]\n%:initial" :immediate-finish t)))

  (setq org-capture-templates-contexts
        '(("r" ((in-mode . "gnus-article-mode")
                (in-mode . "gnus-summary-mode")))))
  
  :bind ("C-c c" . org-capture))

;;;;; Org agenda

(use-package org-agenda
  :after org
  :config
  ;; Basic setup
  (setq org-agenda-span 'day)
  (setq org-agenda-confirm-kill t)
  (setq org-agenda-show-outline-path nil)
  (setq org-agenda-window-setup 'current-window)
  
  ;; General view options
  (defun prot/org-agenda-format-date-aligned (date)
    "Format a DATE string for display in the daily/weekly agenda.
This function makes sure that dates are aligned for easy reading.

Slightly tweaked version of `org-agenda-format-date-aligned' that
produces dates with a fixed length."
    (require 'cal-iso)
    (let* ((dayname (calendar-day-name date t))
           (day (cadr date))
           (day-of-week (calendar-day-of-week date))
           (month (car date))
           (monthname (calendar-month-name month t))
           (year (nth 2 date))
           (iso-week (org-days-to-iso-week
                      (calendar-absolute-from-gregorian date)))
           (weekyear (cond ((and (= month 1) (>= iso-week 52))
                            (1- year))
                           ((and (= month 12) (<= iso-week 1))
                            (1+ year))
                           (t year)))
           (weekstring (if (= day-of-week 1)
                           (format " (W%02d)" iso-week)
                         "")))
      (format "%s %2d %s %4d%s"
              dayname day monthname year weekstring)))

  (setq org-agenda-format-date #'prot/org-agenda-format-date-aligned)
  (setq org-habit-graph-column 45)

  ;; Items with deadline and scheduled timestamps
  (setq org-deadline-warning-days 5)
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  (setq org-agenda-skip-timestamp-if-deadline-is-shown t)
  (setq org-agenda-skip-deadline-prewarning-if-scheduled 1)
  (setq org-scheduled-past-days 365)
  (setq org-deadline-past-days 365)
  (setq org-agenda-timerange-leaders
        '("" "(%d/%d): "))
  (setq org-agenda-scheduled-leaders
        '("Scheduled: " "Sched.%2dx: "))
  (setq org-agenda-inactive-leader "[")
  (setq org-agenda-deadline-leaders
        '("Deadline:  " "In %3d d.: " "%2d d. ago: "))
  ;; Time grid
  (setq org-agenda-time-leading-zero t)
  (setq org-agenda-current-time-string
        "—·—·—·—·—·—·—·—·—")
  (setq org-agenda-time-grid
        '((daily today require-timed)
          (0700 0800 0900 1000 1100
                1200 1300 1400 1500 1600
                1700 1800 1900 2000 2100)
          " -----" "—————————————————"))

  ;; Global to-do list
  (setq org-agenda-todo-ignore-with-date t)
  (setq org-agenda-todo-ignore-timestamp t)
  (setq org-agenda-todo-ignore-scheduled t)
  (setq org-agenda-todo-ignore-deadlines t)
  (setq org-agenda-todo-ignore-time-comparison-use-seconds t)

  (setq org-agenda-custom-commands
        '(("A" "Agenda"
           ((agenda "")
            (todo "TODO"
                  ((org-agenda-overriding-header "To Refile:")
                   (org-agenda-files '("~/gtd/inbox.org"))))))
          ("n" "Next tasks"
           ((todo "TODO"
                  ((org-agenda-overriding-header "Next tasks:")))))))

  :bind (("C-c a" . org-agenda)
         :map org-mode-map
         ("C-'" . nil)
         ("C-," . nil)))

;;;;; Org source code blocks

(use-package org-src
  :after org
  :config
  (setq org-src-window-setup 'current-window)
  (setq org-edit-src-persistent-message nil)
  (setq org-src-fontify-natively t)
  (setq org-src-preserve-indentation t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 0))

;;;;; Deft

(use-package deft
  :ensure
  :config
  (setq deft-directory "~/Dropbox/wiki")
  (setq deft-extensions '("md" "org" "txt"))
  (setq deft-default-extension "md")

  (defun dp/deft-dir (dir)
    "Run deft in directory DIR"
    (when (get-buffer "*Deft*")
      (kill-buffer "*Deft*"))
    (setq deft-directory dir)
    (deft))

  (defun dp/deft-wiki ()
    "Run deft in wiki directory"
    (interactive)
    (dp/deft-dir "~/Dropbox/wiki"))

  (defun dp/deft-zettelkasten ()
    "Run deft in zettelkasten directory"
    (interactive)
    (let ((deft-extensions '("md"))
          (deft-use-filename-as-title t))
      (dp/deft-dir "~/Dropbox/zettelkasten")))

  :bind
  (("C-c n w" . dp/deft-wiki)
   ("C-c n z" . dp/deft-zettelkasten)))

(use-package zetteldeft
  :ensure t
  :after deft
  :config
  (setq zetteldeft-title-prefix "# ")
  (setq zetteldeft-link-indicator "[[")
  (setq zetteldeft-link-suffix "]]")
  (zetteldeft-set-classic-keybindings)
  (font-lock-add-keywords
   'markdown-mode `((,zetteldeft-id-regex . font-lock-warning-face))))

;;;;; Org-journal

(use-package org-journal
  :ensure
  :config
  (setq org-journal-dir "~/Dropbox/journal/")
  (setq org-journal-file-format "%Y-%m-%d.org")
  (setq org-journal-date-prefix "#+title: ")
  (setq org-journal-date-format "%A, %d %B %Y")
  :bind
  ("C-c n j" . org-journal-new-entry))

;;;;; Org-ref

(use-package org-ref
  :ensure
  :config
  (setq reftex-default-bibliography '("~/Dropbox/bibliography/references.bib"))
  (setq org-ref-default-bibliography reftex-default-bibliography)
  (setq org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org")
  (setq org-ref-pdf-directory "~/Dropbox/bibliography/bibtex-pdfs/"))

;;;;; Org export

(use-package ox
  :after org
  :commands (prot/ox-html prot/ox-texinfo)
  :config
  (setq org-export-with-toc t)
  (setq org-export-headline-levels 8)
  (setq org-export-dispatch-use-expert-ui nil)
  (setq org-html-htmlize-output-type nil)
  (setq org-html-head-include-default-style nil)
  (setq org-html-head-include-scripts nil)
  (use-package ox-texinfo)
  ;; FIXME: how to remove everything else?
  (setq org-export-backends '(html texinfo))

  (defun prot/ox-html ()
    (interactive)
    (org-html-export-as-html nil nil nil t nil))

  (defun prot/ox-texinfo ()
    (interactive)
    (org-texinfo-export-to-info)))

;;;; Email settings

;;;;; Client-agnostic email settings (credentials, message composition, encryption)

(use-package auth-source
  :config
  (setq auth-sources '("~/.authinfo.gpg"))
  (setq user-full-name "David Porter")
  (setq user-mail-address "david@daporter.net"))

(use-package mm-encode
  :config
  (setq mm-encrypt-option 'guided)
  (setq mm-sign-option 'guided))

(use-package mml-sec
  :config
  (setq mml-secure-openpgp-encrypt-to-self t)
  (setq mml-secure-openpgp-sign-with-sender t)
  (setq mml-secure-smime-encrypt-to-self t)
  (setq mml-secure-smime-sign-with-sender t))

(use-package epa-file
  :config
  (setq epa-file-cache-passphrase-for-symmetric-encryption nil)
  (setq epa-pinentry-mode 'loopback))

(use-package message
  :config
  (setq message-directory "~/mail/")
  (setq mail-user-agent 'message-user-agent)
  (setq compose-mail-user-agent-warnings nil)
  (setq message-mail-user-agent nil)    ; default is `gnus'
  (setq message-citation-line-format "%f [%Y-%m-%d, %R %z]:\n")
  (setq message-citation-line-function
        'message-insert-formatted-citation-line)
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq message-confirm-send nil)
  (setq message-kill-buffer-on-exit t)
  (setq message-wide-reply-confirm-recipients t)
  (setq message-default-charset 'utf-8)
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
  
  :hook ((message-header-setup-hook . prot/message-header-add-gcc)
         (message-setup-hook . message-sort-headers)))

;;;;; Gnus for reading email, mailing lists, and more

;;;;;; Gnus account settings and essential configurations

(use-package gnus
  :config
  (setq gnus-directory "~/news")
  (setq gnus-select-method '(nnnil ""))
  (setq gnus-secondary-select-methods
        '((nnmaildir "migadu" (directory "~/mail/migadu"))))

  (setq nnir-method-default-engines
        '((nnmaildir . notmuch)))

  (setq gnus-parameters
        '(("migadu"
           (posting-style
            (gcc "nnmaildir+migadu:sent")))))
  (setq gnus-gcc-mark-as-read t)
  (setq gnus-agent t)
  (setq gnus-novice-user nil)
  (setq gnus-check-new-newsgroups 'ask-server)
  (setq gnus-read-active-file 'some)
  (setq gnus-use-dribble-file t)
  (setq gnus-always-read-dribble-file t)

  (defun dp/archive-message ()
    "Move the current message to the mailbox `archive'."
    (interactive)
    (gnus-summary-move-article nil "nnmaildir+migadu:archive"))

  :bind ("C-c m" . gnus))

(use-package nnmail
  :config
  (setq nnmail-expiry-wait 30))

;;;;;; Gnus agent

(use-package gnus-agent
  :after gnus
  :config
  (setq gnus-agent-article-alist-save-format 1)  ; uncompressed
  (setq gnus-agent-cache t)
  (setq gnus-agent-confirmation-function 'y-or-n-p)
  (setq gnus-agent-consider-all-articles nil)
  (setq gnus-agent-directory "~/news/agent/")
  (setq gnus-agent-enable-expiration 'ENABLE)
  (setq gnus-agent-expire-all nil)
  (setq gnus-agent-expire-days 30)
  (setq gnus-agent-mark-unread-after-downloaded t)
  (setq gnus-agent-queue-mail t)        ; queue if unplugged
  (setq gnus-agent-synchronize-flags nil))

;;;;;; Gnus article (message view)

(use-package gnus-art
  :after gnus
  :demand
  :config
  (setq gnus-article-browse-delete-temp 'ask)
  (setq gnus-article-over-scroll nil)
  (setq gnus-article-show-cursor t)
  (setq gnus-article-sort-functions
        '((not gnus-article-sort-by-number)
          (not gnus-article-sort-by-date)))
  (setq gnus-article-truncate-lines nil)
  (setq gnus-html-frame-width 80)
  (setq gnus-html-image-automatic-caching t)
  (setq gnus-inhibit-images t)
  (setq gnus-max-image-proportion 0.7)
  (setq gnus-treat-display-smileys nil)
  (setq gnus-article-mode-line-format "%G %S %m")
  (setq gnus-visible-headers
        '("^From:" "^Subject:" "^To:" "^Cc:" "^Newsgroups:" "^Date:"
          "Followup-To:" "Reply-To:" "^Organization:" "^X-Newsreader:"
          "^X-Mailer:"))
  (setq gnus-sorted-header-list gnus-visible-headers)
  :hook (gnus-article-mode-hook . (lambda ()
                                    (setq-local fill-column 80)))
  :bind (:map gnus-article-mode-map
              ("i" . gnus-article-show-images)
              ("s" . gnus-mime-save-part)
              ("o" . gnus-mime-copy-part)))

;;;;;; Gnus asynchronous operations

(use-package gnus-async
  :after gnus
  :config
  (setq gnus-asynchronous t)
  (setq gnus-use-article-prefetch 15))

;;;;;; Gnus group (main interface)

(use-package gnus-group
  :after gnus
  :demand
  :config
  (setq gnus-list-groups-with-ticked-articles nil)
  (setq gnus-group-sort-function
        '((gnus-group-sort-by-unread)
          (gnus-group-sort-by-alphabet)
          (gnus-group-sort-by-rank)))
  (setq gnus-group-line-format "%M%p%P%5y:%B%(%g%)\n")
  (setq gnus-group-mode-line-format "%%b")
  :hook ((gnus-group-mode-hook . hl-line-mode)
         (gnus-select-group-hook . gnus-group-set-timestamp))
  :bind (:map gnus-group-mode-map
              ("M-n" . gnus-topic-goto-next-topic)
              ("M-p" . gnus-topic-goto-previous-topic)))

(use-package gnus-topic
  :after (gnus gnus-group)
  :config
  (setq gnus-topic-display-empty-topics nil)
  :hook (gnus-group-mode-hook . gnus-topic-mode))

;;;;;; Gnus summary

(use-package gnus-sum
  :after (gnus gnus-group)
  :demand
  :config
  (setq gnus-auto-select-first nil)
  (setq gnus-summary-ignore-duplicates t)
  (setq gnus-suppress-duplicates t)
  (setq gnus-save-duplicate-list t)
  (setq gnus-summary-goto-unread nil)
  (setq gnus-thread-sort-functions
        '(gnus-thread-sort-by-number
          gnus-thread-sort-by-date))
  (setq gnus-subthread-sort-functions 'gnus-thread-sort-by-date)
  (setq gnus-user-date-format-alist
        '(((gnus-seconds-today) . "Today at %R")
          ((+ 86400 (gnus-seconds-today)) . "Yesterday, %R")
          (t . "%Y-%m-%d %R")))

  ;; When the %f specifier in `gnus-summary-line-format' matches my
  ;; name, this will use the contents of the "To:" field, prefixed by
  ;; the string I specify.  Useful when checking your "Sent" summary.
  (setq gnus-ignored-from-addresses "David Porter")
  (setq gnus-summary-to-prefix "To: ")


  (setq gnus-summary-make-false-root 'dummy)
  (setq gnus-summary-dummy-line-format
        (concat "   "
                "                   "
                "                               "
                "• %S\n"))
  (setq gnus-summary-line-format
        (concat "%0{%U%R%z%}"
                "%-16,16&user-date;  "
                "%-30,30f  "
                "%B" "%s\n"))

  (setq gnus-sum-thread-tree-single-indent   "• ")
  (setq gnus-sum-thread-tree-false-root      "  ")
  (setq gnus-sum-thread-tree-root            "• ")
  (setq gnus-sum-thread-tree-vertical        "│ ")
  (setq gnus-sum-thread-tree-leaf-with-other "├─➤ ")
  (setq gnus-sum-thread-tree-single-leaf     "└─➤ ")
  ;;(setq gnus-sum-thread-tree-leaf-with-other "├─> ")
  ;;(setq gnus-sum-thread-tree-single-leaf     "└─> ")
  (setq gnus-sum-thread-tree-indent          "  ")

  (setq gnus-summary-mode-line-format "%p")

  (setq gnus-parameters
        '(("nnmaildir\\+migadu:.*"
           (gnus-summary-dummy-line-format
            (concat "   "
                    "                      "
                    "                               "
                    "• %S\n"))
           (gnus-summary-line-format
            (concat "%0{%U%R%z%}"
                    "%-16,16&user-date;  "
                    "%0{%ug%}  "
                    "%-30,30f  "
                    "%B" "%s\n")))))

  (defun dp/get-address-from-header (field header)
    (cdr (assq field (elt header (1- (length header))))))
  
  (defun gnus-user-format-function-g (header)
    "Indicate whether HEADER contains my Gmail address."
    (let ((gmail-address "david\\.a\\.porter@gmail\\.com")
          (to-address (dp/get-address-from-header 'To header))
          (cc-address (dp/get-address-from-header 'Cc header)))
      (if (or (and to-address (string-match gmail-address to-address))
              (and cc-address (string-match gmail-address cc-address)))
          "G"
        " ")))

  :hook (gnus-summary-mode-hook . hl-line-mode)
  :bind (:map gnus-summary-mode-map
              ("<delete>" . gnus-summary-delete-article)
              ("n" . gnus-summary-next-article)
              ("p" . gnus-summary-prev-article)
              ("N" . gnus-summary-next-unread-article)
              ("P" . gnus-summary-prev-unread-article)
              ("M-n" . gnus-summary-next-thread)
              ("M-p" . gnus-summary-prev-thread)
              ("C-M-n" . gnus-summary-next-group)
              ("C-M-p" . gnus-summary-prev-group)
              ("C-M-^" . gnus-summary-refer-thread)))

;;;;;; Gnus server

(use-package gnus-srvr
  :after gnus
  :hook
  ((gnus-browse-mode-hook gnus-server-mode-hook) . hl-line-mode))

;;;;;; Gnus intersection with Dired

(use-package gnus-dired
  :after (gnus dired)
  :hook (dired-mode-hook . gnus-dired-mode))

;;;; Version control tools

;;;;; Diff-mode

(use-package diff
  :commands prot/diff-refine-buffer
  :config
  (setq diff-default-read-only t)
  (setq diff-advance-after-apply-hunk t)
  (setq diff-update-on-the-fly t)
  ;; The following are from Emacs 27.1
  (setq diff-refine nil)                ; I do it on demand
  (setq diff-font-lock-prettify nil)    ; better for patches
  (setq diff-font-lock-syntax nil)      ; good for accessibility

  (defun prot/diff-buffer-with-file (&optional arg)
    "Compare buffer to its file, else run `vc-diff'.
With \\[universal-argument] also enable highlighting of word-wise
changes, local to the current buffer."
    (interactive "P")
    (let ((buf nil))     ; this method will "fail" if multi diff buffers
      (if (buffer-modified-p)
          (progn
            (diff-buffer-with-file (current-buffer))
            (setq buf "*Diff*"))
        (vc-diff)
        (setq buf "*vc-diff*"))
      (when arg
        (with-current-buffer (get-buffer buf)
          (setq-local diff-refine 'font-lock)))))

  (defun prot/diff-refine-buffer ()
    "Produce word-wise, 'refined' diffs in `diff-mode' buffer.
Also see `prot/diff-refine-hunk-or-buf' that is a wrapper for the
current command."
    (interactive)
    (let ((position (point)))
      (when (derived-mode-p 'diff-mode)
        (setq-local diff-refine 'font-lock)
        (font-lock-flush (point-min) (point-max))
        (goto-char position))))

  (defun prot/diff-refine-hunk-or-buf (&optional arg)
    "Apply word-wise, 'refined' diffs to hunk or buffer.
With prefix ARG (\\[universal-argument]), refine the entire
buffer, else the diff hunk at point.

This is a wrapper around `prot/diff-refine-buffer' and
`diff-refine-hunk', meant to economise on key bindings."
    (interactive "P")
    (if arg
        (prot/diff-refine-buffer)
      (diff-refine-hunk)))

  (defun prot/diff-restrict-view-dwim (&optional arg)
    "Use `diff-restrict-view', or widen when already narrowed.
By default the narrowing effect applies to the focused diff hunk.
With \\[universal-argument] do it for the current file instead."
    (interactive "P")
    (when (derived-mode-p 'diff-mode)
      (if (buffer-narrowed-p)
          (progn
            (widen)
            (message "Widened the view"))
        (if arg
            (progn
              (diff-restrict-view arg)
              (message "Narrowed to file"))
          (diff-restrict-view)
          (message "Narrowed to diff hunk")))))

  ;; `prot/diff-buffer-with-file' replaces the default for `vc-diff'
  ;; (which I bind to another key---see VC section).
  :bind (("C-x v =" . prot/diff-buffer-with-file)
         :map diff-mode-map
         ("C-c C-b" . prot/diff-refine-hunk-or-buf) ; replace `diff-refine-hunk'
         ("C-c C-n" . prot/diff-restrict-view-dwim)))

;;;;; Generic version control (VC) framework

(use-package vc
  :config
  (setq vc-find-revision-no-save t)

  (use-package add-log
    :config
    ;; I use a different account for git commits
    (setq add-log-mailing-address "info@protesilaos.com")
    (setq add-log-keep-changes-together t))

  (use-package log-view
    :config
    (defun prot/vc-print-log (&optional arg)
      "Like `vc-print-log' but for a custom fileset.

With optional prefix ARG (\\[universal-argument]), query for a
number to limit the log to.  Then prompt the user for matching
files in the `default-directory'.  A literal space delimits
multiple files (inserting a space will renew the prompt, asking
for another file match).

In a `dired-mode' buffer, print log for the file at point, or any
marked files, except for when a double prefix argument is passed.
A single prefix arg still provides for a limit to the log.

If a double prefix ARG is passed, prompt for a limit and produce
a log that covers all files in the present directory."
      (interactive "P")
      (let* ((lim (if arg
                      (read-number "Limit log to N entries: " 5)
                    20))
             (dir default-directory)
             (dotless directory-files-no-dot-files-regexp)
             (files (directory-files dir nil dotless t))
             (crm-separator " ")
             (set (cond
                   ((equal arg '(16))
                    files)
                   ((eq major-mode 'dired-mode)
                    (dired-get-marked-files t nil))
                   (t
                    (icomplete-vertical-do ()
                      (completing-read-multiple
                       "Select files in current dir: " files nil t)))))
             (backend (vc-backend set)))
        (vc-print-log-internal backend set nil nil lim 'with-diff)))

    (defun prot/log-view-extract-commit ()
      "Kill commit from around point in `vc-print-log'."
      (interactive)
      (let ((commit (cadr (log-view-current-entry (point) t))))
        (kill-new (format "%s" commit))
        (message "Copied: %s" commit)))

    :bind (("C-x v SPC" . prot/vc-print-log)
           :map log-view-mode-map
           ("<tab>" . log-view-toggle-entry-display)
           ("<return>" . log-view-find-revision)
           ("w" . prot/log-view-extract-commit)
           ("s" . vc-log-search)
           ("o" . vc-log-outgoing)
           ("f" . vc-log-incoming)
           ("F" . vc-update)
           ("P" . vc-push)))

  :bind (("C-x v b" . vc-retrieve-tag)  ; "branch" switch
         ("C-x v t" . vc-create-tag)
         ("C-x v f" . vc-log-incoming)  ; the actual git fetch
         ("C-x v F" . vc-update)        ; "F" because "P" is push
         ("C-x v d" . vc-diff)))

(use-package vc-dir
  :config
  (defun prot/vc-dir (&optional arg)
    "Run `vc-dir' for the current project or directory.
With optional ARG (\\[universal-argument]), use the present
working directory, else default to the root of the current
project, as defined by `vc-root-dir'."
    (interactive "P")
    (let ((dir (if arg default-directory (vc-root-dir))))
      (vc-dir dir)))
  :bind (("C-x v p" . prot/vc-dir)
         :map vc-dir-mode-map
         ("b" . vc-retrieve-tag)
         ("t" . vc-create-tag)
         ("o" . vc-log-outgoing)
         ("f" . vc-log-incoming) ; replaces `vc-dir-find-file' (use RET)
         ("F" . vc-update)       ; symmetric with P: `vc-push'
         ("d" . vc-diff)         ; align with D: `vc-root-diff'
         ("k" . vc-dir-clean-files)))

(use-package vc-git
  :config
  (setq vc-git-diff-switches '("--patch-with-stat" "--histogram"))
  (setq vc-git-print-log-follow t)
  (setq vc-git-revision-complete-only-branches t) ; Emacs 28
  (setq vc-git-root-log-format
        '("%d%h  %ad  %an: %s"
          "^\\(?:[*/\\| ]+ \\)?\
\\(?2: ([^)]+)\\)?\
\\(?1:[0-9a-z]+ \\) \
\\(?3:[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)\
\\(?4:.*?:\\)"
          ((1 'log-view-message)
           (2 'change-log-list nil lax)
           (3 'change-log-date)
           (4 'change-log-name)))))

(use-package vc-annotate
  :config
  (setq vc-annotate-display-mode 'scale) ; scale to oldest
  :bind (("C-x v a" . vc-annotate)       ; `vc-update-change-log' is not in git
         :map vc-annotate-mode-map
         ("<tab>" . vc-annotate-toggle-annotation-visibility)))

;;;;;; Commit log editing

(use-package log-edit
  :config
  (setq log-edit-confirm 'changed)
  (setq log-edit-keep-buffer nil)
  (setq log-edit-require-final-newline t)
  (setq log-edit-setup-add-author nil))

;;;;; Interactive git front-end (Magit)

;;;;;; Base Magit settings

(use-package magit
  :ensure
  :bind ("C-c g" . magit-status))

;;;;;; Magit commits

(use-package git-commit
  :after magit
  :config
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
          overlong-summary-line)))

;;;;;; Magit diffs

(use-package magit-diff
  :after magit
  :config
  (setq magit-diff-refine-hunk t))

;;;;;; Magit repo list

(use-package magit-repos
  :after magit
  :commands magit-list-repositories
  :config
  (setq magit-repository-directories
        '(("~/projects" . 1))))

;;;;; Smerge and Ediff

(use-package smerge-mode
  :config
  (defhydra prot/hydra-smerge-mode
    (:color pink :hint nil :post (smerge-auto-leave))
    "
   ^Motions^      ^Actions^
---^^-------------^^-------
_n_: Next      _b_: Base
_p_: Prev      _u_: Upper
^^             _l_: Lower
^^             _a_: All
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("q" nil "cancel" :color blue))

  (defun prot/smerge-mode-hydra ()
    "Load `prot/hydra-smerge-mode' in `smerge-mode'."
    (when smerge-mode
      (prot/hydra-smerge-mode/body)))
  :hook ((vc-find-file-hook . prot/smerge-mode-hydra)
         (magit-diff-visit-file-hook . prot/smerge-mode-hydra)))

(use-package ediff
  :config
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

;;;; Shells and terminal emulators

;;;;; Command-line shells

;;;;;; Shell (M-x shell)

(use-package shell
  :commands shell-command
  :config
  (setq ansi-color-for-comint-mode t)
  (setq shell-command-prompt-show-cwd t) ; Emacs 27.1
  :bind (("<s-S-return>" . shell)))

;;;;;; Eshell (like M-x shell but in elisp)

(use-package eshell
  :bind ("<s-return>" . eshell))

(use-package esh-mode
  :commands prot/eshell-minor-mode
  :config
  ;; used by other functions below
  (declare-function ffap-file-at-point "ffap.el")

  (defmacro prot/eshell-ffap (name doc &rest body)
    "Make commands for `eshell' find-file-at-point.
NAME is how the function is called.  DOC is the function's
documentation string.  BODY is the set of arguments passed to the
`if' statement to be evaluated when a file at point is present."
    `(defun ,name ()
       ,doc
       (interactive)
       (let ((file (ffap-file-at-point)))
         (if file
             ,@body
           (user-error "No file at point")))))

  (prot/eshell-ffap
   prot/eshell-insert-file-at-point
   "Insert (cat) contents of file at point."
   (progn
     (goto-char (point-max))
     (insert (concat "cat " file))
     (eshell-send-input)))

  (prot/eshell-ffap
   prot/eshell-kill-save-file-at-point
   "Add to kill-ring the absolute path of file at point."
   (progn
     (kill-new (concat (eshell/pwd) "/" file))
     (message "Copied full path of %s" file)))

  (prot/eshell-ffap
   prot/eshell-find-file-at-point
   "Run `find-file' for file at point (ordinary file or dir).
Recall that this will produce a `dired' buffer if the file is a
directory."
   (find-file file))

  (prot/eshell-ffap
   prot/eshell-file-parent-dir
   "Open `dired' with the parent directory of file at point."
   (dired (file-name-directory file)))

  (defun prot/eshell-put-last-output-to-buffer ()
    "Produce a buffer with output of last `eshell' command."
    (interactive)
    (let ((eshell-output (buffer-substring-no-properties
                          (eshell-beginning-of-output)
                          (eshell-end-of-output))))
      (with-current-buffer (get-buffer-create "*last-eshell-output*")
        (erase-buffer)
        (insert eshell-output)
        (switch-to-buffer-other-window (current-buffer)))))

  (defun prot/eshell-complete-redirect-to-buffer ()
    "Complete the syntax for appending to a buffer via `eshell'."
    (interactive)
    (insert
     (format " >>> #<%s>"
             (read-buffer-to-switch "Switch to buffer: "))))

  (defun prot/eshell-narrow-output-highlight-regexp ()
    (interactive)
    (let ((regexp (read-regexp "Regexp to highlight")))
      (narrow-to-region (eshell-beginning-of-output)
                        (eshell-end-of-output))
      (goto-char (point-min))
      (highlight-regexp regexp 'hi-yellow)))

  ;;;; NOTE by Prot 2020-06-16: the following two advice-add snippets
  ;;;; will need to be reviewed to make sure they do not produce
  ;;;; undesirable side effects.

  ;; syntax highlighting implementation modified from
  ;; https://emacs.stackexchange.com/questions/50385/use-emacs-syntax-coloring-when-not-in-emacs
  ;;
  ;; This command also makes it possible to, e.g., cat an encrypted and/or
  ;; compressed file.
  (defun contrib/eshell-cat-with-syntax-highlight (&rest args)
    "Like `eshell/cat' but with syntax highlighting.
To be used as `:override' advice to `eshell/cat'."
    (setq args (eshell-stringify-list (flatten-tree args)))
    (dolist (filename args)
      (let ((existing-buffer (get-file-buffer filename))
            (buffer (find-file-noselect filename)))
        (eshell-print
         (with-current-buffer buffer
           (if (fboundp 'font-lock-ensure)
               (font-lock-ensure)
             (with-no-warnings
               (font-lock-fontify-buffer)))
           (let ((contents (buffer-string)))
             (remove-text-properties 0 (length contents) '(read-only nil) contents)
             contents)))
        (unless existing-buffer
          (kill-buffer buffer)))))

  (advice-add 'eshell/cat :override #'contrib/eshell-cat-with-syntax-highlight)

  ;; Turn ls results into clickable links.  Especially useful when
  ;; combined with link-hint.  Modified from
  ;; https://www.emacswiki.org/emacs/EshellEnhancedLS
  (define-button-type 'eshell-ls
    'supertype 'button
    'help-echo "RET, mouse-2: visit this file"
    'follow-link t)

  (defun contrib/electrify-ls (name)
    "Buttonise `eshell' ls file names.
Visit them with RET or mouse click.  This function is meant to be
used as `:filter-return' advice to `eshell-ls-decorated-name'."
    (add-text-properties 0 (length name)
                         (list 'button t
                               'keymap button-map
                               'mouse-face 'highlight
                               'evaporate t
                               'action #'find-file
                               'button-data (expand-file-name name)
                               'category 'eshell-ls)
                         name)
    name)

  (advice-add 'eshell-ls-decorated-name :filter-return #'contrib/electrify-ls)

  ;; XXX HACK: This and the following minor mode are only introduced to
  ;; circumvent an issue I have encountered with `eshell-mode-map' being
  ;; set to nil and breaking my key bindings by forcing them into the
  ;; global map.
  ;;
  ;; What I do is to define a minor mode that runs from
  ;; `eshell-mode-hook' and pass all my custom key bindings there.
  (defvar prot/eshell-minor-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "M-n") 'outline-next-visible-heading)
      (define-key map (kbd "M-k") 'eshell-kill-input)
      (define-key map (kbd "C-c w") 'prot/eshell-kill-save-file-at-point)
      (define-key map (kbd "C-c i") 'prot/eshell-insert-file-at-point)
      (define-key map (kbd "C-c f") 'prot/eshell-find-file-at-point)
      (define-key map (kbd "C-c C-f") 'prot/eshell-find-file-at-point)
      (define-key map (kbd "C-c o") 'prot/eshell-put-last-output-to-buffer)
      (define-key map (kbd "C-c >") 'prot/eshell-complete-redirect-to-buffer)
      (define-key map (kbd "C-c C-j") 'prot/eshell-file-parent-dir)
      (define-key map (kbd "C-c h") 'prot/eshell-narrow-output-highlight-regexp)
      map)
    "Key map with custom commands for `eshell'.")

  ;; HACK: See comment above.
  (define-minor-mode prot/eshell-minor-mode
    "Special minor mode to enable custom keys in `eshell'.

\\{prot/eshell-minor-mode-map}"
    :init-value nil
    :keymap prot/eshell-minor-mode-map)

  :hook (eshell-mode-hook . prot/eshell-minor-mode))

(use-package esh-module
  :config
  (setq eshell-modules-list             ; Needs review
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
          eshell-unix)))

(use-package em-dirs
  :after esh-mode
  :config
  (setq eshell-cd-on-directory t))

(use-package em-tramp
  :after esh-mode
  :config
  (setq password-cache t)
  (setq password-cache-expiry 600))

(use-package em-hist
  :after esh-mode
  :config
  (setq eshell-hist-ignoredups t)
  (setq eshell-save-history-on-exit t)

  (defun prot/eshell-complete-history ()
    "Insert element from `eshell' history using completion."
    (interactive)
    (let ((hist (ring-elements eshell-history-ring)))
      (insert
       (completing-read "Input history: " hist nil t))))

  (defun prot/eshell-complete-recent-dir (&optional arg)
    "Switch to a recent `eshell' directory using completion.
With \\[universal-argument] also open the directory in a `dired'
buffer."
    (interactive "P")
    (let* ((dirs (ring-elements eshell-last-dir-ring))
           (dir (icomplete-vertical-do ()
                  (completing-read "Switch to recent dir: " dirs nil t))))
      (insert dir)                      ; Not good enough
      (eshell-send-input)               ; Should cd directly…
      (when arg
        (dired dir))))

  ;; `cl-remove-if' is used right below
  (declare-function cl-remove-if "cl-seq")

  (defun prot/eshell-find-subdirectory-recursive ()
    "Recursive `eshell/cd' to subdirectory.
This command has the potential for infinite recursion: use it
wisely or prepare to use `eshell-interrupt-process'."
    (interactive)
    (let* ((dir (abbreviate-file-name (eshell/pwd)))
           (contents (directory-files-recursively dir ".*" t nil nil))
           (dirs (cl-remove-if (lambda (x)
                                 (or (not (file-directory-p x))
                                     (string-match-p "\\.git" x)))
                               contents))
           (selection (icomplete-vertical-do ()
                        (completing-read
                         (format "Find sub-dir from %s (%s): "
                                 (propertize dir 'face 'success)
                                 (length dirs))
                         dirs nil t))))
      (insert selection)
      (eshell-send-input)))

  :bind (:map prot/eshell-minor-mode-map
              ("M-s" . nil) ; I use this for lots of more useful commands
              ("M-r" . prot/eshell-complete-history) ; use this to find input history
              ("C-c d" . prot/eshell-find-subdirectory-recursive)
              ("C-c =" . prot/eshell-complete-recent-dir)))

;;;;; Terminals inside of Emacs

;;;;;; Vterm terminal emulator

(use-package vterm
  :ensure
  :commands vterm
  :config
  (setq vterm-disable-bold-font nil)
  (setq vterm-disable-inverse-video nil)
  (setq vterm-disable-underline nil)
  (setq vterm-kill-buffer-on-exit nil)
  (setq vterm-max-scrollback 9999)
  (setq vterm-shell "/bin/bash")
  (setq vterm-term-environment-variable "xterm-256color"))

;;;;;; Built-in terminals (fallback option)

(use-package term
  :commands (term ansi-term)
  :config
  (setq term-buffer-maximum-size 9999)
  (setq term-completion-autolist t)
  (setq term-completion-recexact t)
  (setq term-scroll-to-bottom-on-output nil))

;;;; Tools for manual pages (manpages)

(use-package man
  :bind (:map Man-mode-map
              ("i" . Man-goto-section)
              ("g" . Man-update-manpage)))

;;;; PDF Tools

(use-package pdf-tools
  :ensure
  :config
  (pdf-tools-install))

;;;; Proced (process monitor, similar to `top')

(use-package proced
  :commands proced
  :config
  (setq proced-auto-update-flag t)
  (setq proced-auto-update-interval 1)
  (setq proced-descend t)
  (setq proced-filter 'user))

(use-package proced-narrow
  :ensure
  :after proced
  :diminish
  :bind (:map proced-mode-map
              ("/" . proced-narrow)))

;;;; BibTex reference manager

(use-package ebib
  :ensure
  :commands ebib
  :config
  (setq ebib-preload-bib-files
        '("~/Dropbox/bibliography/references.bib")))

;;;; Elfeed (RSS/Atom feed reader)

(use-package elfeed
  :ensure
  :config
  (setq elfeed-use-curl t)
  (setq elfeed-curl-max-connections 10)
  (setq elfeed-db-directory "~/.emacs.d/elfeed/")
  (setq elfeed-enclosure-default-dir "~/Downloads/")
  (setq elfeed-search-filter "@4-months-ago +unread")
  (setq elfeed-sort-order 'ascending)
  (setq elfeed-search-clipboard-type 'CLIPBOARD)
  (setq elfeed-search-title-max-width 100)
  (setq elfeed-search-title-min-width 30)
  (setq elfeed-search-trailing-width 25)
  (setq elfeed-show-truncate-long-urls t)
  (setq elfeed-show-unique-buffers t)

  (defun prot/elfeed-feeds ()
    "Load file containing the `elfeed-feeds' list.
Add this to `elfeed-search-mode-hook'."
    (let ((feeds "~/.emacs.d/feeds.el.gpg"))
      (if (file-exists-p feeds)
          (load-file feeds)
        (user-error "Missing feeds' file"))))

  (defun prot/elfeed-show-eww (&optional link)
    "Browse current `elfeed' entry link in `eww'.
Only show the readable part once the website loads.  This can
fail on poorly-designed websites."
    (interactive)
    (let* ((entry (if (eq major-mode 'elfeed-show-mode)
                      elfeed-show-entry
                    (elfeed-search-selected :ignore-region)))
           (link (if link link (elfeed-entry-link entry))))
      (eww link)
      (add-hook 'eww-after-render-hook 'eww-readable nil t)))

  (defun prot/elfeed-search-other-window (&optional arg)
    "Browse `elfeed' entry in the other window.
With \\[universal-argument] browse the entry in `eww' using the
`prot/elfeed-show-eww' wrapper."
    (interactive "P")
    (let* ((entry (if (eq major-mode 'elfeed-show-mode)
                      elfeed-show-entry
                    (elfeed-search-selected :ignore-region)))
           (link (elfeed-entry-link entry))
           (win (selected-window)))
      (with-current-buffer (get-buffer "*elfeed-search*")
        (unless (one-window-p)              ; experimental
          (delete-other-windows win))
        (split-window win (/ (frame-height) 5) 'below)
        (other-window 1)
        (if arg
            (progn
              (when (eq major-mode 'elfeed-search-mode)
                (elfeed-search-untag-all-unread))
              (prot/elfeed-show-eww link))
          (elfeed-search-show-entry entry)))))

  (defun prot/elfeed-kill-buffer-close-window-dwim ()
    "Do-what-I-mean way to handle `elfeed' windows and buffers.

When in an entry buffer, kill the buffer and return to the Elfeed
Search view.  If the entry is in its own window, delete it as
well.

When in the search view, close all other windows.  Else just kill
the buffer."
    (interactive)
    (let ((win (selected-window)))
      (cond ((eq major-mode 'elfeed-show-mode)
             (elfeed-kill-buffer)
             (unless (one-window-p) (delete-window win))
             (switch-to-buffer "*elfeed-search*"))
            ((eq major-mode 'elfeed-search-mode)
             (if (one-window-p)
                 (elfeed-search-quit-window)
               (delete-other-windows win))))))

  (defvar prot/elfeed-mpv-hook nil
    "Hook called before `prot/elfeed-mpv-dwim'.")

  (defun prot/elfeed-mpv-buffer ()
    "Prepare \"*elfeed-mpv-output*\" buffer.
For use by `prot/elfeed-mpv-dwim'.  To be called from
`prot/elfeed-mpv-hook'."
    (let ((buf (get-buffer "*elfeed-mpv-output*"))
          (inhibit-read-only t))
      (with-current-buffer buf
        (erase-buffer))))

  (defun prot/elfeed-mpv-dwim ()
    "Play entry link with external `mpv' program.
When there is an audio enclosure (podcast), play just the audio.
Else spawn a video player at a resolution that accounts for the
current monitor's width."
    (interactive)
    (let* ((entry (if (eq major-mode 'elfeed-show-mode)
                      elfeed-show-entry
                    (elfeed-search-selected :ignore-region)))
           (link (elfeed-entry-link entry))
           (enclosure (elt (car (elfeed-entry-enclosures entry)) 0)) ; fragile?
           (audio "--no-video")
           ;; Here the display width checks if I am on the laptop
           (height (if (<= (display-pixel-width ) 1366) 720 1080))
           (video                       ; this assumes mpv+youtube-dl
            (format "%s --ytdl-format=bestvideo[height\\<=?%s]+bestaudio/best"
                    "--hwdec=auto-safe" height))
           (buf (pop-to-buffer "*elfeed-mpv-output*")))
      (run-hooks 'prot/elfeed-mpv-hook)
      (if enclosure
          (progn
            (async-shell-command (format "mpv %s %s" audio enclosure) buf)
            (message
             (concat "Launching MPV for "
                     (propertize enclosure 'face 'success))))
        (async-shell-command (format "mpv %s %s" video link) buf)
        (message
         (concat "Launching MPV for "
                 (propertize link 'face 'success))))))

  (defvar prot/elfeed-bongo-playlist "*Bongo-Elfeed Queue*"
    "Name of the Elfeed+Bongo multimedia playlist.")

  (defun prot/elfeed-bongo-insert-item ()
    "Insert `elfeed' multimedia links in `bongo' playlist buffer.

The playlist buffer has a unique name so that it will never
interfere with the default `bongo-playlist-buffer'."
    (interactive)
    (with-eval-after-load 'bongo
      (let* ((entry (if (eq major-mode 'elfeed-show-mode)
                        elfeed-show-entry
                      (elfeed-search-selected :ignore-region)))
             (link (elfeed-entry-link entry))
             (enclosure (elt (car (elfeed-entry-enclosures entry)) 0))
             (url (or enclosure link))
             (title (elfeed-entry-title entry))
             (height (if (<= (display-pixel-width) 1366) 720 1080))
             (bongo-pl prot/elfeed-bongo-playlist)
             (buffer (get-buffer-create bongo-pl)))
        (unless (bongo-playlist-buffer)
          (bongo-playlist-buffer))
        (display-buffer buffer)
        (with-current-buffer buffer
 	      (when (not (bongo-playlist-buffer-p))
 	        (bongo-playlist-mode)
            (setq-local bongo-library-buffer (get-buffer "*elfeed-search*"))
            (setq-local bongo-enabled-backends '(mpv))
            (setq-local bongo-mpv-extra-arguments
                        (format           ; this assumes mpv+youtube-dl
                         "%s --ytdl-format=bestvideo[height\\<=?%s]+bestaudio/best"
                         "--hwdec=auto-safe" height))
            (bongo-progressive-playback-mode))
 	      (goto-char (point-max))
          (bongo-insert-uri url title)
          (bongo-insert-comment-text (format "     ==> %s\n" url))
          (let ((inhibit-read-only t))
            (delete-duplicate-lines (point-min) (point-max)))
          (bongo-recenter))
        (message "Enqueued %s “%s” in %s"
                 (if enclosure "podcast" "video")
                 (propertize title 'face 'italic)
                 (propertize bongo-pl 'face 'bold)))))

  (defun prot/elfeed-bongo-switch-to-playlist ()
    (interactive)
    (let* ((bongo-pl prot/elfeed-bongo-playlist)
           (buffer (get-buffer bongo-pl)))
      (if buffer
          (switch-to-buffer buffer)
        (message "No `bongo' playlist is associated with `elfeed'."))))

  (defun prot/elfeed-show-search-update (direction)
    "Update `elfeed-search-buffer' to match entry in DIRECTION.

This is useful when Elfeed is split in two windows, with the
search buffer on one side and an entry buffer on the other.  The
user is changing entries while in the latter, while the former
gets updated to put point on the current item.

EXPERIMENTAL."
    (interactive "s")
    (let* ((entry (if (eq major-mode 'elfeed-show-mode)
                      elfeed-show-entry
                    (elfeed-search-selected :ignore-region)))
           (title (elfeed-entry-title entry))
           (es "*elfeed-search*")
           (buf (get-buffer es))
           (win (get-buffer-window buf)))
      (funcall (intern (concat "elfeed-show-"
                               (substring `,direction 0 4))))
      (when (window-live-p win)
        (with-current-buffer buf
          (goto-char (point-min)) ; Elfeed way to find entry window?
          (search-forward (format "%s" title))
          (funcall (intern (concat `,direction "-line")))
          (set-window-point win (point-at-bol))
          (prot/pulse-line)))))
  
  (defun prot/elfeed-search-tag-filter ()
    "Filter `elfeed' by tags using completion.

Arbitrary input is also possible, but you may need to exit the
minibuffer with `exit-minibuffer' (I bind it to C-j in
`minibuffer-local-completion-map')."
    (interactive)
    (unwind-protect
        (elfeed-search-clear-filter)
      ;; NOTE for the `crm-separator' to work with just a space, you
      ;; need to make SPC self-insert in the minibuffer (the default is
      ;; to behave like tab-completion).
      (let* ((crm-separator " ")
             (elfeed-search-filter-active :live)
             (db-tags (elfeed-db-get-all-tags))
             (plus-tags (delete-dups
                         (mapcar (lambda (x)
                                   (concat "+" (format "%s" x)))
                                 db-tags)))
             (minus-tags (delete-dups
                          (mapcar (lambda (x)
                                    (concat "-" (format "%s" x)))
                                  db-tags)))
             (all-tags (append plus-tags minus-tags))
             (tags (completing-read-multiple
                    "Apply tag: "
                    all-tags nil t))
             (input (string-join `(,elfeed-search-filter ,@tags) " ")))
        (setq elfeed-search-filter input))
      (elfeed-search-update :force)))

  (defun prot/elfeed-show-archive ()
    "Create an archive copy of the current `elfeed' entry."
    (interactive)
    (let* ((entry (if (eq major-mode 'elfeed-show-mode)
                      elfeed-show-entry
                    (elfeed-search-selected :ignore-region)))
           ;; TODO how to cleanly add hyphens instead of spaces?
           (title (elfeed-entry-title entry))
           (elfeed-show-truncate-long-urls nil)
           (archives "~/Documents/feeds/")
           (file (format "%s%s.txt" archives title)))
      (unless (file-exists-p archives)
        (make-directory archives t))
      (when (derived-mode-p 'elfeed-show-mode)
        ;; Refresh to expand truncated URLs
        (elfeed-show-refresh)
        (write-file file t)
        (message "Saved buffer at %s" file))))

  (defun prot/elfeed-toggle-tag (tag)
    "Toggle TAG on current item."
    (interactive)
    (elfeed-search-toggle-all tag))

  (defface prot/important-elfeed-entry
    '((t :inherit error))
    "Marks an important Elfeed entry.")

  (push '(important prot/important-elfeed-entry)
        elfeed-search-face-alist)
  
  :hook ((elfeed-search-mode-hook . prot/elfeed-feeds)
         (prot/elfeed-mpv-hook . prot/elfeed-mpv-buffer))
  :bind (("C-c f" . elfeed)
         :map elfeed-search-mode-map
         ("s" . prot/elfeed-search-tag-filter)
         ("w" . elfeed-search-yank)
         ("g" . elfeed-update)
         ("G" . elfeed-search-update--force)
         ("o" . prot/elfeed-search-other-window)
         ("v" . prot/elfeed-mpv-dwim)
         ("q" . prot/elfeed-kill-buffer-close-window-dwim)
         ("!" . (lambda ()
                  (interactive)
                  (prot/elfeed-toggle-tag 'important)))
         ("b" . prot/elfeed-bongo-insert-item)
         ("h" . prot/elfeed-bongo-switch-to-playlist) ; "hop" mnemonic
         :map elfeed-show-mode-map
         ("a" . prot/elfeed-show-archive)
         ("b" . prot/elfeed-bongo-insert-item)
         ("n" . (lambda ()
                  (interactive)
                  (prot/elfeed-show-search-update "next")))
         ("p" . (lambda ()
                  (interactive)
                  (prot/elfeed-show-search-update "previous")))
         ("e" . prot/elfeed-show-eww)
         ("q" . prot/elfeed-kill-buffer-close-window-dwim)
         ("v" . prot/elfeed-mpv-dwim)
         ("w" . elfeed-show-yank)))

;;;; Emacs Simple HTML Renderer (shr)

(use-package shr
  :config
  (setq shr-use-fonts nil)
  (setq shr-use-colors nil)
  (setq shr-max-image-proportion 0.7)
  (setq shr-image-animate nil)
  (setq shr-width (current-fill-column)))

(use-package shr-tag-pre-highlight
  :ensure
  :after shr
  :config
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight)))

;;;; Emacs Web Wowser (EWW)

(use-package eww
  :config
  (setq eww-restore-desktop nil)
  (setq eww-desktop-remove-duplicates t)
  (setq eww-header-line-format "%u")
  (setq eww-search-prefix "https://duckduckgo.com/html/?q=")
  (setq eww-download-directory "~/Downloads/")
  (setq eww-suggest-uris
        '(eww-links-at-point
          thing-at-point-url-at-point))
  (setq eww-bookmarks-directory "~/.emacs.d/eww-bookmarks/")
  (setq eww-history-limit 150)
  (setq eww-use-external-browser-for-content-type
        "\\`\\(video/\\|audio/\\|application/pdf\\)")
  (setq eww-browse-url-new-window-is-tab nil)
  (setq eww-form-checkbox-selected-symbol "[X]")
  (setq eww-form-checkbox-symbol "[ ]")

  (defun prot/eww-visit-history (&optional arg)
    "Revisit a URL from `eww-prompt-history' using completion.
With \\[universal-argument] produce a new buffer."
    (interactive "P")
    (let ((history eww-prompt-history)  ; eww-bookmarks
          (new (if arg t nil)))
      (icomplete-vertical-do ()
        (eww
         (completing-read "Visit website from history: " history nil t)
         new))))

  ;; eww-view-source

  (defvar prot/eww-mode-global-map
    (let ((map (make-sparse-keymap)))
      (define-key map "s" 'eww-search-words)
      (define-key map "o" 'eww-open-in-new-buffer)
      (define-key map "f" 'eww-open-file)
      (define-key map "w" 'prot/eww-visit-history)
      map)
    "Key map to scope `eww' bindings for global usage.
The idea is to bind this to a prefix sequence, so that its
defined keys follow the pattern of <PREFIX> <KEY>.")
  :bind-keymap ("C-c w" . prot/eww-mode-global-map)
  :bind (:map eww-mode-map
              ("n" . next-line)
              ("p" . previous-line)
              ("f" . forward-char)
              ("b" . backward-char)
              ("a" . prot/eww-org-archive-current-url)
              ("B" . eww-back-url)
              ("N" . eww-next-url)
              ("P" . eww-previous-url)))

(use-package browse-url
  :after eww
  :config
  (setq browse-url-browser-function 'eww-browse-url))

;;; General interface and interactions

;;;; Mode line

(use-package emacs
  :commands prot/mode-line-hidden-mode
  :config
  (setq mode-line-percent-position '(-3 "%p"))
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
                  (vc-mode vc-mode)
                  " "
                  mode-line-modes
                  " "
                  mode-line-misc-info
                  mode-line-end-spaces))

  (define-minor-mode prot/mode-line-hidden-mode
    "Toggle modeline visibility in the current buffer."
    :init-value nil
    :global nil
    (if prot/mode-line-hidden-mode
        (setq-local mode-line-format nil)
      (kill-local-variable 'mode-line-format)
      (force-mode-line-update))))

;;;;; Battery status

(use-package battery
  :config
  (setq battery-mode-line-format " [%b%p%%]")
  (setq battery-mode-line-limit 97)
  (setq battery-update-interval 180)
  (setq battery-load-low 20)
  (setq battery-load-critical 10)
  :hook (after-init-hook . display-battery-mode))

;;;;; Display current time

(use-package time
  :commands world-clock
  :config
  (setq display-time-format "%H:%M  %Y-%m-%d")
  ;;;; Covered by `display-time-format'
  ;; (setq display-time-24hr-format t)
  ;; (setq display-time-day-and-date t)
  (setq display-time-interval 60)
  (setq display-time-mail-directory nil)
  (setq display-time-default-load-average nil)

  ;; World clock
  (setq zoneinfo-style-world-list
        '(("America/Los_Angeles" "Los Angeles")
          ("America/New_York" "New York")
          ("Europe/Amsterdam" "Amsterdam")
          ("Oceania/Sydney" "Sydney")))

  ;; All of the following variables are for Emacs 28
  (setq world-clock-list t)
  (setq world-clock-time-format "%R %z  %A %d %B")
  (setq world-clock-buffer-name "*world-clock*") ; Placement handled by
                                        ; `display-buffer-alist'
  (setq world-clock-timer-enable t)
  (setq world-clock-timer-second 60)

  :hook (after-init-hook . display-time-mode))

;;;; Window divider mode

(use-package emacs
  :config
  (setq window-divider-default-right-width 1)
  (setq window-divider-default-bottom-width 1)
  (setq window-divider-default-places 'right-only)
  :hook (after-init-hook . window-divider-mode))

;;;; Fringe mode

(use-package fringe
  :config
  (fringe-mode nil)
  (setq-default fringes-outside-margins nil)
  (setq-default indicate-buffer-boundaries nil)
  (setq-default indicate-empty-lines nil)
  (setq-default overflow-newline-into-fringe t))

;;;;; Diff highlights in the fringe (diff-hl)

(use-package diff-hl
  :ensure
  :after vc
  :config
  (setq diff-hl-draw-borders nil)
  (setq diff-hl-side 'left)
  :hook ((after-init-hook . global-diff-hl-mode)))

;;;; Highlight comment keywords (hl-todo)

(use-package hl-todo
  :ensure
  :hook (prog-mode-hook . hl-todo-mode))

;;;; Tone down parentheses and autoload cookies in Elisp

(use-package paren-face
  :ensure
  :disabled
  :hook (after-init-hook . global-paren-face-mode))

(use-package dim-autoload
  :ensure
  :hook (emacs-lisp-mode-hook . dim-autoload-cookies-mode))

;;;; Optional visual indicators or layout elements

;;;;; Current line highlight (hl-line-mode)

(use-package hl-line
  :config
  (setq hl-line-sticky-flag nil))

;;;;; Toggles for line numbers and whitespace indicators

(use-package display-line-numbers
  :config
  ;; Set absolute line numbers.  A value of "relative" is also useful.
  (setq display-line-numbers-type t)
  ;; Those two variables were introduced in Emacs 27.1
  (setq display-line-numbers-major-tick 20)
  (setq display-line-numbers-minor-tick 5)
  ;; Use absolute numbers in narrowed buffers
  (setq display-line-numbers-widen t)

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
  :bind ("<f7>" . prot/display-line-numbers-mode))

(use-package whitespace
  :config
  (defun prot/toggle-invisibles ()
    "Toggles the display of indentation and space characters."
    (interactive)
    (if (bound-and-true-p whitespace-mode)
        (whitespace-mode -1)
      (whitespace-mode)))
  :bind (("<f6>" . prot/toggle-invisibles)
         ("C-c z" . delete-trailing-whitespace)))

;;;;; Dim context and focus-to-thing (focus-mode)

(use-package focus
  :ensure
  :commands (prot/focus-mode-to-thing
             prot/focus-mode)
  :config
  (setq-default focus-mode-to-thing
                '((prog-mode . defun)
                  (text-mode . paragraph)))

  ;; TODO review in accordance with `prot/focus-mode'
  (defun prot/focus-mode-to-thing (&optional narrow)
    "Set local values for `focus-mode-to-thing'.
With optional NARROW limit the scope of `focus-mode' to smaller
syntactic units."
    (interactive "P")
    (let* ((units (if narrow
                      (list 'sexp 'sentence)
                    (list 'defun 'paragraph)))
           (prog (car units))
           (text (cadr units)))
      (setq-local focus-mode-to-thing
                  `((prog-mode . ,prog)
                    (text-mode . ,text)))))

  ;; TODO use `define-minor-mode' instead of this
  (defun prot/focus-mode (&optional arg)
    "Toggle the use of `focus-mode'.
With optional \\[universal-argument] prefix, also invoke
`prot/focus-mode-to-thing' with its optional argument for a
narrower field of view."
    (interactive "P")
    (if (bound-and-true-p focus-mode)
        (focus-mode -1)
      (if arg
          (prot/focus-mode-to-thing t)
        (kill-local-variable 'focus-mode-to-thing))
      (focus-mode 1)))

  ;; C-c o is used by `prot/olivetti-mode' and this is a good mnemonic
  ;; for a concept of "Only the thing".
  :bind (("C-c O" . prot/focus-mode)))

;;;;; Centred, focused text mode (olivetti-mode)

(use-package olivetti
  :ensure
  :diminish
  :config
  (setq olivetti-body-width 0.7)
  (setq olivetti-minimum-body-width 80)
  (setq olivetti-recall-visual-line-mode-entry-state t)

  (define-minor-mode prot/olivetti-mode
    "Toggle buffer-local `olivetti-mode' with additional parameters.

Fringes are disabled.  The modeline is hidden, except for
`prog-mode' buffers (see `prot/hidden-mode-line-mode').  The
default typeface is set to a proportionately spaced family,
except for programming modes (see `prot/variable-pitch-mode').
The cursor becomes a blinking bar, per `prot/cursor-type-mode'."
    :init-value nil
    :global nil
    (if prot/olivetti-mode
        (progn
          (olivetti-mode 1)
          (set-window-fringes (selected-window) 0 0)
          (prot/variable-pitch-mode 1)
          (prot/cursor-type-mode 1)
          (unless (derived-mode-p 'prog-mode)
            (prot/hidden-mode-line-mode 1)))
      (olivetti-mode -1)
      (set-window-fringes (selected-window) nil) ; Use default width
      (prot/variable-pitch-mode -1)
      (prot/cursor-type-mode -1)
      (unless (derived-mode-p 'prog-mode)
        (prot/hidden-mode-line-mode -1))))
  :bind ("C-c o" . prot/olivetti-mode))

;;;;; Auto-dim unfocused buffers

(use-package auto-dim-other-buffers
  :ensure
  :commands auto-dim-other-buffers-mode
  :config
  (setq auto-dim-other-buffers-dim-on-switch-to-minibuffer nil)
  (setq auto-dim-other-buffers-dim-on-focus-out t))

;;;;; Outline mode, outline minor mode, and extras

(use-package outline
  :diminish outline-minor-mode
  :config
  ;;; NOTE change this to provide support for your major mode, in case
  ;;; it is missing.
  ;; (setq outline-regexp "[*\^L]+")

  (use-package outline-minor-faces
    :ensure
    :hook
    (outline-minor-mode-hook . outline-minor-faces-add-font-lock-keywords))

  (use-package foldout)
  (use-package bicycle :ensure)

  ;;; NOTE this could also be used to auto-reveal a fold, though I find
  ;;; it a bit annoying when things like that happen automatically.
  ;; (use-package reveal :diminish)

  ;;; NOTE DEPRECATED I am keeping this older piece for reference, but I
  ;;; am discontinuing it: I prefer the approach further below.
  ;;   (defhydra prot/hydra-outline (:idle nil :color pink
  ;;                                       :hint nil :post (deactivate-mark))
  ;;     "
  ;;    ^Motions^                            ^Show^
  ;; ---^------------------------------------^----------------
  ;; _n_: Next heading                    _a_: All
  ;; _p_: Prev heading                    _e_: Entry
  ;; _f_: Fore same level               _TAB_: Cycle
  ;; _b_: Back same level   _<S-iso-lefttab>_: Cycle global
  ;; "
  ;;     ("n" outline-next-visible-heading)
  ;;     ("p" outline-previous-visible-heading)
  ;;     ("f" outline-forward-same-level)
  ;;     ("b" outline-backward-same-level)
  ;;     ("a" outline-show-all)
  ;;     ("e" outline-show-entry)
  ;;     ("TAB" bicycle-cycle)
  ;;     ("<S-iso-lefttab>" bicycle-cycle-global)
  ;;     ("q" nil "cancel" :color blue))

  (defun prot/outline-hide-all ()
    "Hide all `outline-mode' subtrees."
    (interactive)
    (outline-map-region 'outline-hide-subtree (point-min) (point-max)))

  (defun prot/outline-down-heading ()
    "Move to the next `outline-mode' subtree."
    (interactive)
    ;; Hacky, but it kinda works.
    (outline-up-heading 1 t)
    (outline-forward-same-level 1))

  (defun prot/bicycle-cycle-tab-dwim ()
    "Convenience wrapper for TAB key in `outline-mode'."
    (interactive)
    (if (outline-on-heading-p)
        (bicycle-cycle)
      (indent-for-tab-command)))

  (defvar prot/outline-minor-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "M-n") 'outline-next-visible-heading)
      (define-key map (kbd "M-p") 'outline-previous-visible-heading)
      (define-key map (kbd "C-c C-n") 'outline-next-visible-heading)
      (define-key map (kbd "C-c C-p") 'outline-previous-visible-heading)
      (define-key map (kbd "C-c C-f") 'outline-forward-same-level)
      (define-key map (kbd "C-c C-b") 'outline-backward-same-level)
      (define-key map (kbd "C-c C-a") 'outline-show-all)
      (define-key map (kbd "C-c C-q") 'prot/outline-hide-all)
      (define-key map (kbd "C-c C-u") 'outline-up-heading)
      (define-key map (kbd "C-c C-d") 'prot/outline-down-heading)
      (define-key map (kbd "C-c C-z") 'foldout-zoom-subtree)
      (define-key map (kbd "C-c C-x") 'foldout-exit-fold)
      (define-key map (kbd "<tab>") 'prot/bicycle-cycle-tab-dwim)
      (define-key map (kbd "<C-tab>") 'bicycle-cycle)
      (define-key map (kbd "<S-iso-lefttab>") 'bicycle-cycle-global)
      map)
    "Key map for `prot/outline-minor-mode'.
The idea is to make `outline-minor-mode' keys a bit easier to
work with.")

  (defvar prot/outline-minor-mode-enter-hook nil
    "Hook that runs when `prot/outline-minor-mode' is enabled.")

  (defvar prot/outline-minor-mode-exit-hook nil
    "Hook that runs when `prot/outline-minor-mode' is disabled.")

  ;; `imenu' integration with `outline-minor-mode'
  (defun prot/outline-imenu-heading ()
    "Move to the previous `outline-mode' heading.
This is because `imenu' produces its index by moving backward
from the bottom."
    (outline-previous-heading))

  (defun prot/outline-imenu-title ()
    "Return the text of the `outline-mode' heading."
    (interactive)
    ;; NOTE This may be too simplistic and error-prone, though I have
    ;; not ran into any problems.
    (buffer-substring (point-at-bol)
                      (point-at-eol)))

  (defun prot/outline-imenu-setup ()
    "`imenu' bindings for the local `outline-mode' buffer.
To be used in tandem with `prot/outline-minor-mode-enter-hook'."
    (setq-local imenu-prev-index-position-function
                'prot/outline-imenu-heading)
    (setq-local imenu-extract-index-name-function
                'prot/outline-imenu-title))

  (defun prot/outline-imenu-restore ()
    "Restore `imenu' list when `prot/outline-minor-mode' is off.
The new index should be the same as the one you would get in a
standard invocation of `imenu'.

To be used in `prot/outline-minor-mode-exit-hook'."
    (dolist (var '(imenu-prev-index-position-function
                   imenu-extract-index-name-function))
      (kill-local-variable var))
    (save-excursion
      (imenu-default-create-index-function)
      (message "Refreshed `imenu' index")))

  ;; XXX This could prove problematic when more than one major modes are
  ;; active, such as when you use C-c ' in an Org block.
  (defun prot/outline-minor-refontify ()
    "Re-enable the current buffer's major mode.
Add this to `prot/outline-minor-mode-exit-hook'."
    (let ((mode major-mode))
      (when (or (derived-mode-p 'text-mode)
                (derived-mode-p 'prog-mode))
        (funcall mode)
        (message "Re-enabled %s" mode))))

  (define-minor-mode prot/outline-minor-mode
    "Toggles `outline-minor-mode' and extras.

\\{prot/outline-minor-mode-map}"
    :init-value nil
    :lighter " =┆"
    :global nil
    :keymap prot/outline-minor-mode-map
    (if prot/outline-minor-mode
        (progn
          (when (eq major-mode 'org-mode)
            (user-error "Don't use `outline-minor-mode' with Org"))
          (outline-minor-mode 1)
          (run-hooks 'prot/outline-minor-mode-enter-hook))
      (outline-minor-mode -1)
      (run-hooks 'prot/outline-minor-mode-exit-hook)))

  :hook ((prot/outline-minor-mode-enter-hook . prot/outline-imenu-setup)
         (prot/outline-minor-mode-exit-hook . prot/outline-imenu-restore)
         (prot/outline-minor-mode-exit-hook . prot/outline-minor-refontify))
  ;; key replaces `menu-bar-open', which I never use
  :bind ("<f10>" . prot/outline-minor-mode))

;;;; Registers

(use-package register
  :commands (prot/clear-registers
             prot/append-register-line-separator
             prot/append-register-space-separator
             prot/prepend-register-line-separator
             prot/prepend-register-space-separator)
  :config
  ;;;;;; Use this if you need it
  ;;;; Define f5 as an alias for C-x r
  ;; (global-set-key (kbd "<f5>") (lookup-key global-map (kbd "C-x r")))

  (defun prot/clear-registers ()
    "Remove all saved registers."
    (interactive)
    (setq register-alist nil))

  (defun prot/append-register-space-separator (start end)
    "Append region to register with space in between entries.

A prompt will ask for the register to operate on."
    (interactive "r")
    (let* ((prompt (register-read-with-preview
                    "Append to register with space separator: ")))
      (setq register-separator ?+)
      (set-register register-separator " ")
      (append-to-register prompt start end nil)))

  (defun prot/append-register-line-separator (start end)
    "Append region to register with an empty line in between entries.

A prompt will ask for the register to operate on."
    (interactive "r")
    (let* ((prompt (register-read-with-preview
                    "Append to register with line separator: ")))
      (setq register-separator ?+)
      (set-register register-separator "\n\n")
      (append-to-register prompt start end nil)))

  (defun prot/prepend-register-space-separator (start end)
    "Prepend region to register with space in between entries.

A prompt will ask for the register to operate on."
    (interactive "r")
    (let* ((prompt (register-read-with-preview
                    "Prepend to register with space separator: ")))
      (setq register-separator ?+)
      (set-register register-separator " ")
      (prepend-to-register prompt start end nil)))

  (defun prot/prepend-register-line-separator (start end)
    "Prepend region to register with an empty line in between entries.

A prompt will ask for the register to operate on."
    (interactive "r")
    (let* ((prompt (register-read-with-preview
                    "Prepend to register with line separator: ")))
      (setq register-separator ?+)
      (set-register register-separator "\n\n")
      (prepend-to-register prompt start end nil))))

;;;; Visual bookmarks (bm.el)

(use-package bm
  :ensure
  :init
  (setq bm-restore-repository-on-load t)
  :config
  (setq bm-annotate-on-create nil)
  (setq bm-buffer-persistence t)
  (setq bm-cycle-all-buffers t)
  (setq bm-goto-position nil)
  (setq bm-highlight-style 'bm-highlight-line-and-fringe)
  (setq bm-marker 'bm-marker-right)
  (setq bm-in-lifo-order nil)
  (setq bm-recenter t)
  (setq bm-repository-file "~/.emacs.d/bm-bookmarks")
  (setq bm-repository-size 100)
  (setq bm-show-annotations t)
  (setq bm-wrap-immediately t)
  (setq bm-wrap-search t)
  :bind (("<C-f8>" . bm-next)
         ("<C-S-f8>" . bm-previous)
         ("<s-f8>" . bm-toggle-buffer-persistence)
         ("<f8>" . bm-toggle)))

;;;; Annotations (annotate.el)

(use-package annotate
  :ensure
  :diminish
  :commands (annotate-annotate
             prot/annotate-annotate
             annotate-goto-next-annotation
             annotate-goto-previous-annotation
             annotate-export-annotations
             annotate-integrate-annotations
             annotate-show-annotation-summary)
  :config
  (setq annotate-file "~/.emacs.d/annotations")
  (setq annotate-annotation-column 73)
  (setq annotate-diff-export-context 5)
  (setq annotate-use-messages nil)
  (setq annotate-integrate-marker "")
  (setq annotate-integrate-higlight ?^)
  (setq annotate-fallback-comment "#")
  (setq annotate-blacklist-major-mode '())
  (setq annotate-annotation-max-size-not-place-new-line 50)
  (setq annotate-search-region-lines-delta 4)
  (setq annotate-annotation-position-policy :by-length)
  (setq annotate-summary-ask-query nil)

  (defun prot/annotate-mode ()
    "Toggles `annotate-mode' for the current buffer."
    (if (bound-and-true-p annotate-mode)
        (annotate-mode -1)
      (annotate-mode 1)))

  (defun prot/annotate-annotate ()
    "Ensure `annotate-mode' is enabled for `annotate-annotate'."
    (unless (bound-and-true-p annotate-mode)
      (annotate-mode 1))
    (annotate-annotate))

  (defun prot/annotate-annotate-dwim (&optional arg)
    "Common points of entry for annotations.
Write an annotation or toggle `annotate-mode' by prefixing this
function with the \\[universal-argument]."
    (interactive "P")
    (if arg
        (prot/annotate-mode)
      (prot/annotate-annotate)))

  (defhydra prot/hydra-annotate (:idle nil :color pink :hint nil
                                       :post (deactivate-mark))
    "
   ^Motions^                ^Actions^
---^------------------------^----------------
_n_: Next annotation     _a_: Annotate
_p_: Prev annotation     _i_: Integrate
^^                       _s_: Show summary
"
    ("n" annotate-goto-next-annotation)
    ("p" annotate-goto-previous-annotation)
    ("a" prot/annotate-annotate)
    ("i" annotate-integrate-annotations)
    ("s" annotate-show-annotation-summary)
    ("q" nil "cancel" :color blue))

  :bind (("<f9>" . prot/annotate-annotate-dwim)
         ("<C-f9>" . annotate-goto-next-annotation)
         ("<C-S-f9>" . annotate-goto-previous-annotation)
         :map annotate-mode-map
         ("C-c h a" . prot/hydra-annotate/body)))

;;;; Cursor and mouse settings

;;;;; Cursor appearance and tweaks

(use-package frame
  :commands prot/cursor-type-mode
  :config
  (setq-default cursor-type 'box)
  (setq-default cursor-in-non-selected-windows '(bar . 2))
  (setq-default blink-cursor-blinks 50)
  (setq-default blink-cursor-interval nil) ; 0.75 would be my choice
  (setq-default blink-cursor-delay 0.2)

  (blink-cursor-mode -1)

  (define-minor-mode prot/cursor-type-mode
    "Toggle between static block and pulsing bar cursor."
    :init-value nil
    :global t
    (if prot/cursor-type-mode
        (progn
          (setq-local blink-cursor-interval 0.75)
          (setq-local cursor-type '(bar . 2))
          (setq-local cursor-in-non-selected-windows 'hollow)
          (blink-cursor-mode 1))
      (dolist (local '(blink-cursor-interval
                       cursor-type
                       cursor-in-non-selected-windows))
        (kill-local-variable `,local))
      (blink-cursor-mode -1))))

;;;;; pulse.el (highlight cursor position)

(use-package pulse
  :config
  (defface prot/pulse-line-modus-theme
    '((t :inherit modus-theme-subtle-green :extend t))
    "Ad-hoc face for `prot/pulse-line'.
This is done because it is not possible to highlight empty lines
without the `:extend' property.")

  (defun prot/pulse-line (&optional face)
    "Temporarily highlight the current line."
    (interactive)
    (let ((start (if (eobp)
                     (line-beginning-position 0)
                   (line-beginning-position)))
          (end (line-beginning-position 2))
          (pulse-delay .04)
          (face
           (if face
               face
             'prot/pulse-line-modus-theme)))
      (pulse-momentary-highlight-region start end face)))
  :bind ("<s-escape>" . prot/pulse-line))


;;;;; Mouse wheel behaviour

(use-package mouse
  :config
  ;; In Emacs 27, use Control + mouse wheel to scale text.
  (setq mouse-wheel-scroll-amount
        '(1
          ((shift) . 5)
          ((meta) . 0.5)
          ((control) . text-scale)))
  (setq mouse-drag-copy-region nil)
  (setq make-pointer-invisible t)
  (setq mouse-wheel-progressive-speed t)
  (setq mouse-wheel-follow-mouse t)
  :hook (after-init-hook . mouse-wheel-mode))

;;;;; Scrolling behaviour

(use-package emacs
  :config
  (setq-default scroll-preserve-screen-position t)
  (setq-default scroll-conservatively 1) ; affects `scroll-step'
  (setq-default scroll-margin 0)

  (define-minor-mode prot/scroll-centre-cursor-mode
    "Toggle centred cursor scrolling behaviour."
    :init-value nil
    :lighter " S="
    :global nil
    (if prot/scroll-centre-cursor-mode
        (setq-local scroll-margin (* (frame-height) 2)
                    scroll-conservatively 0
                    maximum-scroll-margin 0.5)
      (dolist (local '(scroll-preserve-screen-position
                       scroll-conservatively
                       maximum-scroll-margin
                       scroll-margin))
        (kill-local-variable `,local))))

  ;; C-c l is used for `org-store-link'.  The mnemonic for this is to
  ;; focus the Line and also works as a variant of C-l.
  :bind ("C-c L" . prot/scroll-centre-cursor-mode))

;;;;; Delete selection

(use-package delsel
  :hook (after-init-hook . delete-selection-mode))

;;;;; Tooltips (tooltip-mode)

(use-package tooltip
  :config
  (setq tooltip-delay 0.5)
  (setq tooltip-short-delay 0.5)
  (setq x-gtk-use-system-tooltips nil)
  (setq tooltip-frame-parameters
        '((name . "tooltip")
          (internal-border-width . 6)
          (border-width . 0)
          (no-special-glyphs . t)))
  :hook (after-init-hook . tooltip-mode))

;;;; Conveniences and minor extras

;;;;; Auto revert mode

(use-package autorevert
  :diminish
  :config
  (setq auto-revert-verbose t)
  :hook (after-init-hook . global-auto-revert-mode))

;;;;; Preserve contents of system clipboard

(use-package emacs
  :config
  (setq save-interprogram-paste-before-kill t))

;;;;; Generic feedback

(use-package emacs
  :config
  (setq frame-title-format '("%b"))
  (setq echo-keystrokes 0.25)
  (setq default-input-method "greek")
  (setq ring-bell-function 'ignore)

  (defalias 'yes-or-no-p 'y-or-n-p)
  (put 'narrow-to-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'dired-find-alternate-file 'disabled nil)
  (put 'overwrite-mode 'disabled t))

;;;;; Newline characters for file ending

(use-package emacs
  :config
  (setq mode-require-final-newline 'visit-save))

;;;;; Altered zap and easier repeats

(use-package emacs
  :config
  (setq repeat-on-final-keystroke t)
  (setq set-mark-command-repeat-pop t)
  :bind ("M-z" . zap-up-to-char))

;;;;; Package lists

(use-package package
  :commands (list-packages
             package-refresh-contents
             package-list-packages)
  :hook (package-menu-mode-hook . hl-line-mode))

;;; Language settings for prose and code

;;;; Line length (column count)

(use-package emacs
  :config
  (setq-default fill-column 72)
  :hook (after-init-hook . column-number-mode))

;;;; Paragraphs and fill-mode

(use-package emacs
  :config
  (setq sentence-end-double-space t)
  (setq colon-double-space nil)
  (setq use-hard-newlines nil)
  (setq sentence-end-without-period nil))

(use-package emacs
  :diminish auto-fill-function
  :config
  (setq adaptive-fill-mode t)

  (defun prot/unfill-region-or-paragraph (&optional region)
    "Unfill paragraph or, when active, the region.
Join all lines in a region, if active, while respecting any empty
lines (so multiple paragraphs are not joined, just unfilled).  If
no region is active, operate on the paragraph.  The idea is to
produce the opposite effect of both `fill-paragraph' and
`fill-region'."
    (interactive)
    (let ((fill-column most-positive-fixnum))
      (if (use-region-p)
          (fill-region (region-beginning) (region-end))
        (fill-paragraph nil region))))

  :hook (text-mode-hook . (lambda ()
                            (turn-on-auto-fill)))
  :bind ("M-Q" . prot/unfill-region-or-paragraph))

;;;; Recognise subwords

(use-package subword
  :diminish
  :hook (prog-mode-hook . subword-mode))

;;;; Comment lines, regions, boxes, etc.

(use-package newcomment
  :config
  (setq comment-empty-lines t)
  (setq comment-fill-column nil)
  (setq comment-multi-line t)
  (setq comment-style 'multi-line)

  (defun prot/comment-dwim (&optional arg)
    "Alternative to `comment-dwim': offers a simple wrapper
around `comment-line' and `comment-dwim'.

If the region is active, then toggle the comment status of the
region or, if the major mode defines as much, of all the lines
implied by the region boundaries.

Else toggle the comment status of the line at point."
    (interactive "*P")
    (if (use-region-p)
        (comment-dwim arg)
      (save-excursion
        (comment-line arg))))

  :bind (("C-;" . prot/comment-dwim)
         ("C-:" . comment-kill)
         ("M-;" . comment-indent)
         ("C-x C-;" . comment-box)))

;;;; Configure 'electric' behaviour

(use-package electric
  :config
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
  :hook (after-init-hook . (lambda ()
                             (electric-indent-mode 1)
                             (electric-pair-mode -1)
                             (electric-quote-mode -1))))

;;;; Parentheses (show-paren-mode)

(use-package paren
  :config
  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-in-periphery nil)
  (setq show-paren-when-point-inside-paren nil)
  :hook (after-init-hook . show-paren-mode))

;;;; Tabs, indentation, and the TAB key

(use-package emacs
  :config
  (setq-default tab-always-indent 'complete)
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil))

;;;; Dictionary (and local sdcv variant)

(use-package sdcv
  :ensure
  :hook (sdcv-mode-hook . (lambda ()
                            (font-lock-mode -1))))

(use-package dictionary
  :ensure)

;;;; Flyspell (spell check)

(use-package flyspell
  :commands (ispell-change-dictionary
             ispell-word
             flyspell-buffer
             flyspell-mode
             flyspell-region)
  :config
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  (setq ispell-program-name "aspell")
  (setq ispell-dictionary "en_GB")

  (defvar prot/ispell-dictionaries
    '(("English" . "en")
      ("Français" . "fr")
      ("Nederlands" . "nl")
      ("Espanõl" . "es"))
    "Alist of dictionaries I may use.
It is used by `prot/ispell-dictionaries-complete'.")

  (defun prot/ispell-dictionaries-complete ()
    "Select an item from `prot/ispell-dictionaries'."
    (interactive)
    (let* ((dicts (mapcar #'car prot/ispell-dictionaries))
           (choice (completing-read "Select dictionary: " dicts nil t))
           (key (cdr (assoc `,choice prot/ispell-dictionaries))))
      (ispell-change-dictionary key)
      (message "Switched to %s" key)))

  (defun prot/flyspell-dwim ()
    "Spell check region or select dictionary.

Use `flyspell-region' on the active region, else invoke
`prot/ispell-dictionaries-complete'."
    (interactive)
    (let ((beg (region-beginning))
          (end (region-end)))
      (if (use-region-p)
          (flyspell-region beg end)
        (prot/ispell-dictionaries-complete))))

  :bind (("M-$" . prot/flyspell-dwim)
         ("C-M-$" . prot/ispell-dictionaries-complete)
         :map flyspell-mode-map
         ("C-;" . nil)))

;;;;; WordReference.com lookup

(use-package emacs
  :config
  (defun dp/wordreference-lookup (dict)
    "Look up a definition in the WordRerefence.com dictionary DICT.

Uses the region if it is active, otherwise the word under the
cursor."
    (let ((word (if (use-region-p)
                    (buffer-substring-no-properties
                     (region-beginning) (region-end))
                  (current-word))))
      (browse-url (concat "https://www.wordreference.com/" dict "/" word))))
  
  (defun dp/wordreference-lookup-en-fr ()
    "Look up a definition in the WordRerefence.com en-fr dictionary."
    (interactive)
    (dp/wordreference-lookup "enfr"))

  (defun dp/wordreference-lookup-fr-en ()
    "Look up a definition in the WordRerefence.com fr-en dictionary."
    (interactive)
    (dp/wordreference-lookup "fren")))

;;;; Code and text linters

;;;;; Flymake

(use-package flymake
  :commands flymake-mode
  :config
  (setq flymake-fringe-indicator-position 'left-fringe)
  (setq flymake-suppress-zero-counters t)
  (setq flymake-start-on-flymake-mode t)
  (setq flymake-no-changes-timeout nil)
  (setq flymake-start-on-save-buffer t)
  (setq flymake-proc-compilation-prevents-syntax-check t)
  (setq flymake-wrap-around nil)

  (defhydra prot/hydra-flymake (:color pink :hint nil)
    "
Actions
-------
_n_: Next error
_p_: Previous error
_d_: Diagnostics' buffer
"
    ("d" flymake-show-diagnostics-buffer)
    ("n" flymake-goto-next-error)
    ("p" flymake-goto-prev-error)
    ("q" nil "cancel" :color blue))
  :bind (:map flymake-mode-map
              ("C-c h l" . prot/hydra-flymake/body)))

;;;;; Flycheck

(use-package flycheck
  :ensure
  :commands flycheck-mode
  :config
  (setq flycheck-check-syntax-automatically
        '(save mode-enabled))

  (defun prot/flycheck-list-errors-toggle ()
    "Toggle the display of `flycheck-mode' diagnostics' buffer."
    (interactive)
    (let ((diagnostics (get-buffer-window flycheck-error-list-buffer)))
      (unless flycheck-mode
        (user-error "Flycheck mode not enabled"))
      (if diagnostics
          (delete-window diagnostics)
        (flycheck-list-errors))))

  (defhydra prot/hydra-flycheck (:color pink :hint nil)
    "
   ^Actions^             ^Helpers^
---^^--------------------^^-----------------
_n_: Next error       _c_: Check buffer
_p_: Previous error   _l_: List diagnostics
_e_: Explain error    _x_: Disable checker
_d_: Display error
"
    ("l" prot/flycheck-list-errors-toggle)
    ("c" flycheck-buffer)
    ("n" flycheck-next-error)
    ("p" flycheck-previous-error)
    ("e" flycheck-explain-error-at-point)
    ("d" flycheck-display-error-at-point)
    ("x" flycheck-disable-checker :color blue)
    ("q" nil "cancel" :color blue))

  :hook (flycheck-error-list-mode-hook . visual-line-mode)
  :bind (:map flycheck-mode-map
              ("C-c h l" . prot/hydra-flycheck/body)))


;;;;;; Flycheck mode line indicator

(use-package flycheck-indicator
  :ensure
  :after flycheck
  :config
  (setq flycheck-indicator-icon-error (string-to-char "!"))
  (setq flycheck-indicator-icon-info (string-to-char "·"))
  (setq flycheck-indicator-icon-warning (string-to-char "*"))
  (setq flycheck-indicator-status-icons
        '((not-checked "%")
          (no-checker "-")
          (running "&")
          (errored "!")
          (finished "=")
          (interrupted "#")
          (suspicious "?")))

  (flycheck-define-checker dp/git-commit-message
    "A git commit message checker using `gitlint'.

See URL `https://jorisroovers.com/gitlint/'."
    :command ("gitlint" "--msg-filename" source)
    :error-patterns
    ((error line-start
            line ": " (id (one-or-more (in alnum)))
            blank
            (message (one-or-more (not (any ":"))))
            (minimal-match (zero-or-more anything))
            line-end))
    :modes vc-git-log-edit-mode)

  (add-to-list 'flycheck-checkers 'dp/git-commit-message t)

  :hook (flycheck-mode-hook . flycheck-indicator-mode))

;;;; Eldoc (elisp live documentation feedback)

(use-package eldoc
  :diminish
  :config
  (global-eldoc-mode 1))

;;;; Support for various major modes

;;;;; Markdown

(use-package markdown-mode
  :ensure
  :mode ("\\.md$" . markdown-mode)
  :config
  ;; Allows for fenced block focus with C-c ' (same as Org blocks).
  (use-package edit-indirect :ensure)
  (setq markdown-command "multimarkdown")
  (setq markdown-fontify-code-blocks-natively t)
  (setq time-stamp-format "%Y-%02m-%02d %02H:%02M:%02S")
  
  (defun dp/reftex-citation ()
    (interactive)
    (let ((reftex-cite-format
           ;; MLA citation style
           '((?\C-m . "[#%l]: %a. *%t*. %d, %u, %y, %p %<."))))
      (reftex-citation)))
  
  :hook (write-file-hooks . time-stamp)
  :bind ("C-c [" . dp/reftex-citation))

;;;;; YAML

(use-package yaml-mode
  :ensure
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

;;;;; CSS

(use-package css-mode
  :mode (("\\.css$" . css-mode)
         ("\\.scss$" . scss-mode))
  :config
  (setq css-fontify-colors nil))

;;; History and state

;;;; Emacs server and desktop

(use-package server
  :hook (after-init-hook . server-start))

(use-package desktop
  :config
  (setq desktop-auto-save-timeout 300)
  (setq desktop-dirname "~/.emacs.d/")
  (setq desktop-base-file-name "desktop")
  (setq desktop-files-not-to-save nil)
  (setq desktop-globals-to-clear nil)
  (setq desktop-load-locked-desktop t)
  (setq desktop-missing-file-warning t)
  (setq desktop-restore-eager 0)
  (setq desktop-restore-frames nil)
  (setq desktop-save 'ask-if-new)
  (desktop-save-mode 1))

(use-package emacs
  :init
  (setq initial-buffer-choice t)
  (setq inhibit-startup-echo-area-message "david") ; read the docstring
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-buffer-menu t))

;;;; Record various types of history

;;;;; Recentf (recent files and directories)

(use-package recentf
  :config
  (setq recentf-save-file "~/.emacs.d/recentf")
  (setq recentf-max-saved-items 200)
  (setq recentf-exclude '(".gz" ".xz" ".zip" "/elpa/" "/ssh:" "/sudo:"))

  (defun prot/recentf-keep-predicate (file)
    "Additional conditions for saving in `recentf-list'.
Add this function to `recentf-keep'.

NEEDS REVIEW."
    (cond
     ((file-directory-p file) (file-readable-p file))))
  (add-to-list 'recentf-keep 'prot/recentf-keep-default-predicate)

  (defun prot/recentf (&optional input)
    "Select item from `recentf-list' using completion.
Use INPUT as an initial, yet editable, filter.

The user's $HOME directory is abbreviated as a tilde."
    (interactive)
    (let* ((files (mapcar 'abbreviate-file-name recentf-list))
           (f (icomplete-vertical-do ()
                (completing-read "Open recentf entry: " files nil t
                                 (when input input)))))
      (find-file f)))

  (defun prot/recentf-dirs (&optional arg)
    "Select directory from `recentf-list' using completion.
With \\[universal-argument] present the list in a `dired' buffer.
This buffer is meant to be reused by subsequent invocations of
this command (otherwise you need to remove the `when' expression.

Without \\[universal-argument], the user's $HOME directory is
abbreviated as a tilde.  In the Dired buffer paths are absolute."
    (interactive "P")
    (let* ((list (mapcar 'abbreviate-file-name recentf-list))
           (dirs (delete-dups
                  (mapcar (lambda (file)
                            (if (file-directory-p file)
                                (directory-file-name file)
                              (substring (file-name-directory file) 0 -1)))
                          list)))
           (buf "*Recentf Dired*")
           (default-directory "~"))
      (when (get-buffer buf)
        (kill-buffer buf))
      (if arg
          (dired (cons (generate-new-buffer-name buf) dirs))
        (icomplete-vertical-do ()
          (find-file
           (completing-read "Recent dirs: " dirs nil t))))))

  :hook (after-init-hook . recentf-mode)
  :bind (("s-r" . prot/recentf)
         ("C-x C-r" . prot/recentf-dirs)))

;;;;; Minibuffer history (savehist-mode)

(use-package savehist
  :config
  (setq savehist-file "~/.emacs.d/savehist")
  (setq history-length 1000)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  :hook (after-init-hook . savehist-mode))

;;;;; Record cursor position

(use-package saveplace
  :config
  (setq save-place-file "~/.emacs.d/saveplace")
  (setq save-place-forget-unreadable-files t)
  (save-place-mode 1))

;;;;; Backups

(use-package emacs
  :config
  (setq backup-directory-alist
        '(("." . "~/.emacs.d/backup/")))
  (setq backup-by-copying t)
  (setq version-control t)
  (setq delete-old-versions t)
  (setq kept-new-versions 6)
  (setq kept-old-versions 2)
  (setq create-lockfiles nil))


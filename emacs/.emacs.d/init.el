;; Emacs Configuration
;; =============================================================================

;; ................................................................. use-package

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(unless package--initialized (package-initialize))

;; Set up `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

;; ............................................................... Base settings

;; Modeline "lighters"
(use-package delight
  :ensure t
  :after use-package)

;; Store customisation settings in a separate file
(use-package cus-edit
  :config
  (setq custom-file "~/.emacs.d/custom.el")

  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))

  (load custom-file))

;; Base typeface configurations
(use-package emacs
  :config
  (setq x-underline-at-descent-line nil)
  (setq underline-minimum-offset 0)
  (setq line-spacing 0.2)

  (defconst prot/fixed-pitch-font "Hack"
    "The default fixed-pitch typeface.")

  (defconst prot/fixed-pitch-params ":hintstyle=hintslight"
    "Fontconfig parameters for the fixed-pitch typeface.")

  (defun prot/default-font (family size)
    "Set frame font to FAMILY at SIZE."
    (set-frame-font
     (concat family "-" (number-to-string size) prot/fixed-pitch-params) t t))

  (defun prot/laptop-fonts ()
    "Fonts for the small laptop screen.

Pass desired argument to `prot/font-sizes' for use on my
small laptop monitor."
    (interactive)
    (when window-system
      (prot/default-font prot/fixed-pitch-font 9)))

  (defun prot/desktop-fonts ()
    "Fonts for the larger desktop screen.

Pass desired argument to `prot/font-sizes' for use on my larger
 desktop monitor (external display connected to my laptop)."
    (interactive)
    (when window-system
      (prot/default-font prot/fixed-pitch-font 9)))

  (defun prot/reading-fonts ()
    "Fonts for focused reading sessions."
    (interactive)
    (when window-system
      (prot/default-font prot/fixed-pitch-font 10.5)))

  (defun prot/fonts-per-monitor ()
    "Use font settings based on screen size.

Choose between `prot/laptop-fonts' and `prot/desktop-fonts'
depending on the width of the monitor.  The calculation is based
on the maximum width of my laptop's screen.  So if an external
display is attached, then iit is considered a desktop scenario.

While this function is interactive, it is best to run it with the
`after-init-hook' or perhaps some other event that tracks
monitor-related events."
    (interactive)
    (when window-system
      (if (<= (display-pixel-width) 1600)
          (prot/laptop-fonts)
        (prot/desktop-fonts))))

  :hook (after-init . prot/fonts-per-monitor))

;; Backups

(use-package emacs
  :config
  (setq-default backup-directory-alist
                `(("" . ,(expand-file-name "backup/" user-emacs-directory)))
                auto-save-default nil
                backup-by-copying t
                delete-old-versions t))

;; ................................................ Completion framework

;; Minibuffer essentials and Icomplete

(use-package minibuffer
  :config
  (setq completion-cycle-threshold 3)
  ;;(setq completion-flex-nospace nil)
  (setq completion-pcm-complete-word-inserts-delimiters t)
  (setq completion-pcm-word-delimiters "-_./:| ")
  (setq completion-show-help nil)
  ;;(setq completion-styles '(partial-completion substring initials flex))
  (setq completion-styles '(partial-completion substring initials))
  (setq completion-category-overrides
        '((file (styles initials basic))
          (buffer (styles initials basic))
          (info-menu (styles basic))))
  (setq completions-format 'vertical)   ; *Completions* buffer
  (setq enable-recursive-minibuffers t)
  (setq read-answer-short t)
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq resize-mini-windows t)

  (file-name-shadow-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1)

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
    (when current-prefix-arg
      (let ((help (get-buffer-window "*Help*")))
        (when help
          (if (not (eq (selected-window) help))
              (select-window help)
            (select-window (get-mru-window)))))))

  ;; Defines, among others, aliases for common actions to Super-KEY.
  ;; Normally these should go in individual package declarations, but
  ;; their grouping here makes things easier to understand.
  :bind (("s-f" . find-file)
         ("s-F" . find-file-other-window)
         ("s-d" . dired)
         ("s-D" . dired-other-window)
         ("s-b" . switch-to-buffer)
         ("s-B" . switch-to-buffer-other-window)
         ("s-h" . prot/describe-symbol-at-point)
         ("s-H" . (lambda ()
                    (interactive)
                    (let ((current-prefix-arg t))
                      (prot/describe-symbol-at-point))))
         ("s-v" . prot/focus-minibuffer-or-completions)
         :map completion-list-mode-map
         ("h" . prot/describe-symbol-at-point)
         ("n" . next-line)
         ("p" . previous-line)
         ("f" . next-completion)
         ("b" . previous-completion)
         ("M-v" . prot/focus-minibuffer)))

(use-package savehist
  :config
  (setq savehist-file "~/.emacs.d/savehist")
  (setq history-length 30000)
  (setq history-delete-duplicates nil)
  (setq savehist-save-minibuffer-history t)
  (savehist-mode 1))

(use-package icomplete
  :demand
  :after minibuffer
  :config
  (setq icomplete-delay-completions-threshold 0)
  (setq icomplete-max-delay-chars 0)
  (setq icomplete-compute-delay 0)
  (setq icomplete-show-matches-on-no-input t)
  (setq icomplete-hide-common-prefix nil)
  (setq icomplete-prospects-height 1)
  (setq icomplete-separator " · ")      ; mid dot
  (setq icomplete-with-completion-tables t)
  (setq icomplete-in-buffer nil)        ; not your job

  ;;(fido-mode -1)                        ; Emacs 27.1
  (icomplete-mode 1)

  (defun prot/icomplete-force-complete-and-exit ()
    "Complete the current icomplete match and exit the minibuffer.

Contrary to `icomplete-force-complete-and-exit', this will
confirm your choice without complaining about incomplete matches.

Those incomplete matches can block you from performing legitimate
actions, such as defining a new tag in an `org-capture' prompt.

In my testing, this is necessary when the variable
`icomplete-with-completion-tables' is non-nil, because then
`icomplete' will be activated practically everywhere it can."
    (interactive)
    (icomplete-force-complete)
    (exit-minibuffer))

  (defun prot/icomplete-kill-ring-save (&optional arg)
    "Expand and save current icomplete match to the kill ring.

With a prefix argument, insert the match to the point in the
current buffer and switch focus back to the minibuffer."
    (interactive "*P")
    (when (and (minibufferp)
               (bound-and-true-p icomplete-mode))
      (icomplete-force-complete)
      (kill-new (field-string-no-properties))
      (when current-prefix-arg
        (kill-new (field-string-no-properties))
        (select-window (get-mru-window))
        (insert (car kill-ring))
        (prot/focus-minibuffer))))

  (defun prot/icomplete-toggle-completion-styles (&optional arg)
    "Toggle between flex and prefix matching.

With pregix ARG use basic completion instead.  These styles are
described in `completion-styles-alist'.

Bind this function in `icomplete-minibuffer-map'."
    (interactive "*P")
    (when (and (minibufferp)
               (bound-and-true-p icomplete-mode))
      (let* ((completion-styles-original completion-styles)
             (basic '(emacs22 basic))
             (flex '(flex initials substring partial-completion))
             (prefix '(partial-completion substring initials flex)))
        (if current-prefix-arg
            (progn
              (setq-local completion-styles basic)
              (message "%s" (propertize "Prioritising BASIC matching" 'face 'highlight)))
          (if (not (eq (car completion-styles) 'flex))
              (progn
                (setq-local completion-styles flex)
                (message "%s" (propertize "Prioritising FLEX matching" 'face 'highlight)))
            (setq-local completion-styles prefix)
            (message "%s" (propertize "Prioritising PREFIX matching" 'face 'highlight)))))))

  (defun contrib/icomplete-remove-directory-path (arg)
    "Remove sub-directory path in `find-file' or equivalent.

Bind this in `minibuffer-local-filename-completion-map'."
    (interactive "p")
    (zap-up-to-char (- arg) ?/))

  :bind (:map icomplete-minibuffer-map
              ("C-m" . minibuffer-complete-and-exit) ; try complete + force current input
              ("C-j" . exit-minibuffer) ; force current input more aggressively
              ("C-n" . icomplete-forward-completions)
              ("<right>" . icomplete-forward-completions)
              ("<down>" . icomplete-forward-completions)
              ("C-p" . icomplete-backward-completions)
              ("<left>" . icomplete-backward-completions)
              ("<up>" . icomplete-backward-completions)
              ;;("<return>" . prot/icomplete-force-complete-and-exit)
              ("<return>" . icomplete-force-complete-and-exit)
              ("<C-backspace>" . contrib/icomplete-remove-directory-path)
              ("M-o w" . prot/icomplete-kill-ring-save)
              ("M-o i" . (lambda ()
                           (interactive)
                           (let ((current-prefix-arg t))
                             (prot/icomplete-kill-ring-save))))
              ;;("C-," . prot/icomplete-toggle-completion-styles) ; toggle flex
              ;;("C-." . (lambda ()                               ; toggle basic
              ;;           (interactive)
              ;;           (let ((current-prefix-arg t))
              ;;             (prot/icomplete-toggle-completion-styles))))))
              ))

;; ............................................................. Recentf

(use-package recentf
  :config
  (setq recentf-save-file "~/.emacs.d/recentf")
  (setq recentf-max-menu-items 10)
  (setq recentf-max-saved-items 200)
  (setq recentf-show-file-shortcuts-flag nil)

  ;; rename entries in recentf when moving files in dired
  (defun rjs/recentf-rename-directory (oldname newname)
    ;; oldname, newname and all entries of recentf-list should already
    ;; be absolute and normalised so I think this can just test whether
    ;; oldname is a prefix of the element.
    (setq recentf-list
          (mapcar (lambda (name)
                    (if (string-prefix-p oldname name)
                        (concat newname (substring name (length oldname)))
                      name))
                  recentf-list))
    (recentf-cleanup))

  (defun rjs/recentf-rename-file (oldname newname)
    (setq recentf-list
          (mapcar (lambda (name)
                    (if (string-equal name oldname)
                        newname
                      oldname))
                  recentf-list))
    (recentf-cleanup))

  (defun rjs/recentf-rename-notify (oldname newname &rest args)
    (if (file-directory-p newname)
        (rjs/recentf-rename-directory oldname newname)
      (rjs/recentf-rename-file oldname newname)))

  (advice-add 'dired-rename-file :after #'rjs/recentf-rename-notify)

  (defun contrib/recentf-add-dired-directory ()
    "Include Dired buffers in the `recentf' list.  Particularly
useful when combined with a completion framework's ability to
display virtual buffers."
    (when (and (stringp dired-directory)
               (equal "" (file-name-nondirectory dired-directory)))
      (recentf-add-file dired-directory)))

  :hook ((after-init . recentf-mode)
         (dired-mode . contrib/recentf-add-dired-directory)))

;; Icomplete vertical mode

(use-package icomplete-vertical
  :ensure t
  :demand
  :after (minibuffer icomplete)
  :config
  (setq icomplete-vertical-prospects-height (/ (window-height) 6))
  (icomplete-vertical-mode -1)

  (defun prot/icomplete-recentf ()
    "Open `recent-list' item in a new buffer.

The user's $HOME directory is abbreviated as a tilde."
    (interactive)
    (icomplete-vertical-do ()
      (let ((files (mapcar 'abbreviate-file-name recentf-list)))
        (find-file
         (completing-read "Open recentf entry: " files nil t)))))

  (defun prot/icomplete-yank-kill-ring ()
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
          (:separator 'dotted-line :height (/ (window-height) 4))
        (when (use-region-p)
          (delete-region (region-beginning) (region-end)))
        (insert
         (completing-read "Yank from kill ring: " kills nil t)))))

  ;; TODO can registers be inserted via completion?
  ;; TODO can mark-ring positions be selected?

  :bind (("s-y" . prot/icomplete-yank-kill-ring)
         ("s-r" . prot/icomplete-recentf)
         :map icomplete-minibuffer-map
         ("C-v" . icomplete-vertical-toggle)))

;; Completion for projects and directory trees

(use-package project
  :after (minibuffer icomplete icomplete-vertical)
  :config
  (defun prot/project-find-file ()
    "Find a file that belongs to the current project."
    (interactive)
    (icomplete-vertical-do ()
      (project-find-file)))

  (defun prot/project-or-dir-find-subdirectory-recursive ()
    "Recursive find subdirectory of project or directory.

This command has the potential for infinite recursion: use it
wisely or prepare to use \\[keyboard-quit]."
    (interactive)
    (let* ((project (vc-root-dir))
           (dir (if project project default-directory))
           ;;(contents (directory-files-recursively dir ".*" t nil nil))
           (contents (directory-files dir t))
           (find-directories (mapcar (lambda (dir)
                                       (when (file-directory-p dir)
                                         (abbreviate-file-name dir)))
                                     contents))
           (subdirs (delete nil find-directories)))
      (icomplete-vertical-do ()
        (dired
         (completing-read "Find sub-directory: " subdirs nil t dir)))))

  (defun prot/find-file-from-dir-recursive ()
    "Find file recursively, starting from present dir."
    (interactive)
    (let* ((dir default-directory)
           ;;(files (directory-files-recursively dir ".*" nil t))
           (files (directory-files-recursively dir ".*" nil)))
      (icomplete-vertical-do ()
        (find-file
         (completing-read "Find file recursively: " files nil t dir)))))

  (defun prot/find-project ()
    "Switch to sub-directory at ~/projects.

Allows you to switch directly to the root directory of a project
inside a given location."
    (interactive)
    (let* ((path "~/projects/")
           (dotless directory-files-no-dot-files-regexp)
           (project-list (project-combine-directories
                          (directory-files path t dotless)))
           (projects (mapcar 'abbreviate-file-name project-list)))
      (icomplete-vertical-do ()
        (dired
         (completing-read "Find project: " projects nil t path)))))

  :bind (("M-s p" . prot/find-project)
         ("M-s f" . prot/project-find-file)
         ("M-s z" . prot/find-file-from-dir-recursive)
         ("M-s d" . prot/project-or-dir-find-subdirectory-recursive)
         ("M-s l" . find-library)
         ("M-s C-M-%" . project-query-replace-regexp)))

;; ............................................... In-buffer completions

;; Dabbrev (dynamic word completion)

(use-package dabbrev
  :after (minibuffer icomplete icomplete-vertical)
  :config
  (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
  (setq dabbrev-abbrev-skip-leading-regexp "\\$\\|\\*\\|/\\|=")
  (setq dabbrev-backward-only nil)
  (setq dabbrev-case-distinction nil)
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-case-replace nil)
  (setq dabbrev-check-other-buffers t)
  (setq dabbrev-eliminate-newlines nil)
  (setq dabbrev-upcase-means-case-search t)

  :bind (("M-/" . dabbrev-expand)))

;; ............................................................. Isearch

(use-package isearch
  :delight
  :config
  (setq search-highlight t)
  (setq search-whitespace-regexp ".*?")
  (setq isearch-lax-whitespace t)
  (setq isearch-regexp-lax-whitespace nil)
  (setq isearch-lazy-highlight t)
  ;; All of the following variables were introduced in Emacs 27.1.
  ;;(setq isearch-lazy-count t)
  ;;(setq lazy-count-prefix-format "(%s/%s) ")
  ;;(setq lazy-count-suffix-format nil)
  ;;(setq isearch-yank-on-move 'shift)
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

  (defun prot/isearch-abort ()
    "Remove non-matching `isearch' input, reverting to previous
successful search and continuing with the search.

This is a modified variant of the original `isearch-abort',
mapped to C-g which will remove the failed match if any and only
afterwards exit the search altogether."
    (interactive)
    (discard-input)
    (while (or (not isearch-success) isearch-error)
      (isearch-pop-state))
    (isearch-update))

  (defun prot/isearch-query-replace-symbol-at-point ()
    "Run `query-replace-regexp' for the symbol at point."
    (interactive)
    (isearch-forward-symbol-at-point)
    (isearch-query-replace-regexp))

  :bind (("M-s M-o" . multi-occur)
         ("M-s %" . prot/isearch-query-replace-symbol-at-point)
         :map minibuffer-local-isearch-map
         ("M-/" . isearch-complete-edit)
         :map isearch-mode-map
         ("M-/" . isearch-complete)
         ("C-SPC" . prot/isearch-mark-and-exit)
         ("DEL" . prot/isearch-abort)
         ("<C-return>" . prot/isearch-other-end)))

;; ................................................. Regular expressions

(use-package re-builder
  :config
  (setq reb-re-syntax 'read))

(use-package visual-regexp
  :ensure t
  :config
  (setq vr/default-replace-preview nil)
  (setq vr/match-separator-use-custom-face t))

;; wgrep

(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t))

;; ripgrep

(use-package rg
  :ensure t
  :after wgrep
  :config
  (setq rg-group-result t)
  (setq rg-hide-command t)
  (setq rg-show-columns nil)
  (setq rg-show-header t)
  (setq rg-custom-type-aliases nil)
  (setq rg-default-alias-fallback "all")

  (rg-define-search prot/grep-vc-or-dir
    :query ask
    :format regexp
    :files "everything"
    :dir (let ((vc (vc-root-dir)))
           (if vc
               vc                         ; search root project dir
             default-directory))          ; or from the current dir
    :confirm prefix
    :flags ("--hidden -g !.git"))

  (defun prot/rg-save-search-as-name ()
    "Save `rg' buffer, naming it after the current search query.

This function is meant to be mapped to a key in `rg-mode-map'."
    (interactive)
    (let ((pattern (car rg-pattern-history)))
      (rg-save-search-as-name (concat "«" pattern "»"))))

  :bind (("M-s g" . prot/grep-vc-or-dir)
         :map rg-mode-map
         ("s" . prot/rg-save-search-as-name)
         ("C-n" . next-line)
         ("C-p" . previous-line)
         ("M-n" . rg-next-file)
         ("M-p" . rg-prev-file)))

;; ............................................................. Buffers

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-strip-common-suffix t)
  (setq uniquify-after-kill-buffer-p t))

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
  :hook
  (ibuffer-mode . hl-line-mode)
  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         ("* f" . ibuffer-mark-by-file-name-regexp)
         ("* g" . ibuffer-mark-by-content-regexp) ; "g" is for "grep"
         ("* n" . ibuffer-mark-by-name-regexp)
         ("s n" . ibuffer-do-sort-by-alphabetic)  ; "sort name" mnemonic
         ("/ g" . ibuffer-filter-by-content)))

(use-package ibuffer-vc
  :ensure t
  :after (ibuffer vc)
  :bind (:map ibuffer-mode-map
              ("/ V" . ibuffer-vc-set-filter-groups-by-vc-root)
              ("/ <deletechar>" . ibuffer-clear-filter-groups)))

;; ............................................................. Windows

(use-package window
  :init
  (setq display-buffer-alist
        '(;; top side window
          ("\\*\\(Flycheck\\|Flymake\\|Package-Lint\\|vc-git :\\).*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . top)
           (slot . 0)
           (window-parameters . ((no-other-window . t))))
          ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Messages\\)\\*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . top)
           (slot . 1)
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
          ("\\*e?shell.*"
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
           (window-parameters . ((no-other-window . t)
                                 (mode-line-format . (" "
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

  :hook ((help-mode . visual-line-mode)
         (custom-mode . visual-line-mode))

  :bind (("s-n" . next-buffer)
         ("s-p" . previous-buffer)
         ("s-o" . other-window)
         ("s-2" . split-window-below)
         ("s-3" . split-window-right)
         ("s-0" . delete-window)
         ("s-1" . delete-other-windows)
         ("s-5" . delete-frame)
         ("C-x +" . balance-windows-area)
         ("s-q" . window-toggle-side-windows)))

;; These are all experimental.  Just showcasing the power of passing
;; parameters to windows or frames.
(use-package emacs
  :commands (prot/window-dired-vc-root-left
             prot/make-frame-floating-with-current-buffer
             prot/display-buffer-at-bottom)
  :config
  (defun prot/window-dired-vc-root-left ()
    "Open root directory of current version-controlled repository
or the present working directory with `dired' and bespoke window
parameters.  This is meant as a proof-of-concept function,
illustrating how to leverage window rules to display a buffer,
plus a few concomitant extras."
    (interactive)
    (let ((dir (if (eq (vc-root-dir) nil)
                   (dired-noselect default-directory)
                 (dired-noselect (vc-root-dir)))))
      (display-buffer-in-side-window
       dir `((side . left)
             (slot . -1)
             (window-width . 0.16)
             (window-parameters . ((no-other-window . t)
                                   (no-delete-other-windows . t)
                                   (mode-line-format . (" "
                                                        mode-line-buffer-identification))))))
      (with-current-buffer dir
        (rename-buffer "*Dired-Side*")
        (setq-local window-size-fixed 'width)))
    (with-eval-after-load 'ace-window
      (when (boundp 'aw-ignored-buffers)
        (add-to-list 'aw-ignored-buffers "*Dired-Side*"))))

  (defun prot/make-frame-floating-with-current-buffer ()
    "Display the current buffer in a new floating frame.

This passes certain parameters to the newly created frame:

- use a different name than the default;
- use a graphical frame;
- do not display the minibuffer.

The name is meant to be used by the external rules of my tiling
window manager (BSPWM) to present the frame in a floating state."
    (interactive)
    (make-frame '((name . "my_float_window")
                  (window-system . x)
                  (minibuffer . nil))))

  (defun prot/display-buffer-at-bottom ()
    "Move the current buffer to the bottom of the frame.  This is
useful to take a buffer out of a side window.

The window parameters of this function are provided mostly for
didactic purposes."
    (interactive)
    (let ((buffer (current-buffer)))
      (with-current-buffer buffer
        (delete-window)
        (display-buffer-at-bottom
         buffer `((window-parameters . ((mode-line-format . (" "
                                                             mode-line-buffer-identification))))))))))

(use-package winner
  :hook (after-init . winner-mode)
  :bind (("<s-right>" . winner-redo)
         ("<s-left>" . winner-undo)))

(use-package windmove
  :config
  ;;(setq windmove-create-window nil)     ; Emacs 27.1
  :bind (("C-s-k" . windmove-up)
         ("C-s-l" . windmove-right)
         ("C-s-j" . windmove-down)
         ("C-s-h" . windmove-left)))

;; ............................................................... Dired

(use-package dired
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-listing-switches "-AFlh")
  (setq dired-dwim-target t)
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . hl-line-mode)))

(use-package dired-aux
  :config
  (setq dired-isearch-filenames 'dwim)
  ;; The following variables were introduced in Emacs 27.1
  ;;(setq dired-create-destination-dirs 'ask)
  ;;(setq dired-vc-rename-file t)
  :bind (:map dired-mode-map
              ("C-+" . dired-create-empty-file)
              ("M-s f" . nil)))

(use-package find-dired
  :after dired
  :config
  (setq find-ls-option '("-ls" . "-AFhl"))
  (setq find-name-arg "-iname"))

(use-package async
  :ensure t)

(use-package dired-async
  :after (dired async)
  :hook (dired-mode . dired-async-mode))

(use-package dired-narrow
  :ensure t
  :after dired
  :config
  (setq dired-narrow-exit-when-one-left t)
  (setq dired-narrow-enable-blinking t)
  (setq dired-narrow-blink-time 0.3)
  :bind (:map dired-mode-map
              ("M-s n" . dired-narrow)))

(use-package wdired
  :after dired
  :commands wdired-change-to-wdired-mode
  :config
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))

(use-package peep-dired
  :ensure t
  :after dired
  :config
  (setq peep-dired-cleanup-on-disable t)
  (setq peep-dired-enable-on-directories nil)
  (setq peep-dired-ignored-extensions
        '("mkv" "webm" "mp4" "mp3" "ogg" "iso"))
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

(use-package dired-subtree
  :ensure t
  :after dired
  :config
  (setq dired-subtree-use-backgrounds nil)
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<C-tab>" . dired-subtree-cycle)
              ("<S-iso-lefttab>" . dired-subtree-remove)))

(use-package dired-x
  :after dired
  :config
  (setq dired-clean-up-buffers-too t)
  (setq dired-clean-confirm-killing-deleted-buffers t)
  (setq dired-x-hands-off-my-keys t)
  (setq dired-bind-man nil)
  (setq dired-bind-info nil)

  (defun prot/kill-current-filename ()
    "Place the current buffer's file name in the `kill-ring'."
    (interactive)
    (kill-new (dired-filename-at-point)))

  (defun prot/insert-current-filename ()
    "Insert at point the current buffer's file name."
    (interactive)
    (insert (dired-filename-at-point)))

  :bind (("C-x C-j" . dired-jump)
         ("s-j" . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window)
         ("s-J" . dired-jump-other-window)))

(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode))

;; ........................................................ Applications

(use-package calendar
  :config
  (setq calendar-mark-diary-entries-flag t)
  (setq calendar-week-start-day 1)      ; Monday
  (setq calendar-date-style 'iso)
  (setq calendar-holidays
        (append holiday-general-holidays holiday-local-holidays
                holiday-other-holidays holiday-christian-holidays
                holiday-solar-holidays))
  :hook (calendar-today-visible . calendar-mark-today))

(use-package diary-lib
  :config
  (setq diary-header-line-flag nil)
  (setq diary-mail-addr "david@daporter.net")
  (setq diary-mail-days 3)
  (setq diary-number-of-entries 3)
  (setq diary-comment-start ";")
  (setq diary-date-forms
        '((day "/" month "[^/0-9]")
          (day "/" month "/" year "[^0-9]")
          (day " *" monthname " *" year "[^0-9]")
          (monthname " *" day "[^,0-9]")
          (monthname " *" day ", *" year "[^0-9]")
          (year "[-/]" month "[-/]" day "[^0-9]")
          (dayname "\\W"))))

(use-package org
  :config
  ;; agenda and basic directory structure
  (setq org-directory "~/org")
  (setq org-default-notes-file "~/org/notes.org")
  (setq org-agenda-files '("~/org" "~/.emacs.d"))
  (setq org-deadline-warning-days 3)
  (setq org-refile-targets
        '((org-agenda-files . (:maxlevel . 2))
          (nil . (:maxlevel . 2))))
  (setq org-refile-use-outline-path t)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-cache t)
  (setq org-fontify-done-headline t)
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-track-ordered-property-with-tag t)
  ;; log
  (setq org-log-done 'time)
  (setq org-read-date-prefer-future 'time)
  ;;
  (setq org-special-ctrl-a/e t)
  (setq org-catch-invisible-edits 'show)
  (setq org-hide-emphasis-markers t)
  (setq org-loop-over-headlines-in-active-region 'start-level)

  :hook (org-mode . org-indent-mode)

  :bind (("C-c l" . org-store-link)
         :map org-mode-map
         ("<C-return>" . nil)
         ("<C-S-return>" . nil)))

(use-package org-src
  :after org
  :config
  (setq org-src-window-setup 'current-window)
  (setq org-src-preserve-indentation t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 0))

(use-package htmlize
  :ensure t
  :after org
  (setq htmlize-ignore-face-size t))

(use-package org-superstar
  :ensure t
  :after org
  :config
  (setq org-superstar-remove-leading-stars t))

;; ............................................................... Theme

;; Disable GUI components
(use-package emacs
  :init
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  :config
  (setq use-file-dialog nil)
  (setq use-dialog-box t)		; only for mouse events
  (setq inhibit-splash-screen t)
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z"))
  (global-unset-key (kbd "C-h h")))

;; Themes
(use-package modus-operandi-theme
  :ensure t)

(use-package modus-vivendi-theme
  :ensure t)

(use-package emacs
  :config
  (setq custom-safe-themes t)

  (defun prot/modus-operandi ()
    "Enable some `modus-operandi' variables and load the theme."
    (setq modus-operandi-theme-slanted-constructs t
          modus-operandi-theme-bold-constructs nil
          modus-operandi-theme-visible-fringes t
          modus-operandi-theme-subtle-diffs t
          modus-operandi-theme-distinct-org-blocks nil
          modus-operandi-theme-proportional-fonts nil
          modus-operandi-theme-scale-headings t
          modus-operandi-theme-scale-1 1.05
          modus-operandi-theme-scale-2 1.1
          modus-operandi-theme-scale-3 1.15
          modus-operandi-theme-scale-4 1.2)
    (load-theme 'modus-operandi t))

  (defun prot/modus-vivendi ()
    "Enable some `modus-vivendi' variables and load the theme."
    (setq modus-vivendi-theme-slanted-constructs t
          modus-vivendi-theme-bold-constructs nil
          modus-vivendi-theme-visible-fringes t
          modus-vivendi-theme-subtle-diffs t
          modus-vivendi-theme-distinct-org-blocks nil
          modus-vivendi-theme-proportional-fonts nil
          modus-vivendi-theme-scale-headings t
          modus-vivendi-theme-scale-1 1.05
          modus-vivendi-theme-scale-2 1.1
          modus-vivendi-theme-scale-3 1.15
          modus-vivendi-theme-scale-4 1.2)
    (load-theme 'modus-vivendi t))

  (defcustom prot/modus-themes-toggle-hook nil
    "Hook that runs after `prot/modus-themes-toggle' is invoked."
    :type 'hook)

  (defun prot/modus-themes-toggle ()
    "Toggle between `modus-operandi' and `modus-vivendi' themes.

Also run `prot/modus-themes-toggle-hook'."
    (interactive)
    (if (eq (car custom-enabled-themes) 'modus-operandi)
        (prot/modus-vivendi)
      (prot/modus-operandi))
    (run-hooks 'prot/modus-themes-toggle-hook))

  (defun prot/describe-face (pos)
    "Print message with name of face at point.

A lightweight alternative to `what-cursor-position' and its
prefixed variant."
    (interactive "d")
    (let ((face (or (get-char-property pos 'face)
                    (get-char-property pos 'read-cf-name))))
      (message "%s" (or face
                        "No face at point"))))

  :bind (("<f5>" . prot/modus-themes-toggle)
         ("s-=" . prot/describe-face))
  :hook (after-init . prot/modus-operandi))

;; ........................................................... Mode line

(use-package emacs
  :config
  (setq mode-line-percent-position '(-3 "%p"))
  ;;(setq mode-line-defining-kbd-macro
  ;;      (propertize "Macro" 'face 'mode-line-emphasis))
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
                  " "
                  mode-line-modes
                  " "
                  mode-line-misc-info
                  mode-line-end-spaces)))

(use-package battery
  :config
  (setq battery-mode-line-format " [%b%p%%]")
  (setq battery-mode-line-limit 97)
  (setq battery-update-interval 180)
  (setq battery-load-low 20)
  (setq battery-load-critical 10)
  :hook (after-init . display-battery-mode))

(use-package time
  :config
  (setq display-time-format "%Y-%m-%d  %H:%M")
  ;;;; Covered by `display-time-format'
  ;; (setq display-time-24hr-format t)
  ;; (setq display-time-day-and-date t)
  (setq display-time-interval 60)
  (setq display-time-mail-directory nil)
  (setq display-time-default-load-average nil)
  :hook (after-init . display-time-mode))

;; ...................................... Window elements and indicators

(use-package emacs
  :config
  (setq window-divider-default-right-width 1)
  (setq window-divider-default-bottom-width 1)
  (setq window-divider-default-places 'right-only)
  :hook (after-init . window-divider-mode))

(use-package fringe
  :config
  (fringe-mode '(8 . 8))
  (setq-default fringes-outside-margins nil)
  (setq-default indicate-buffer-boundaries nil)
  (setq-default indicate-empty-lines nil)
  (setq-default overflow-newline-into-fringe t))

(use-package diff-hl
  :ensure t
  :after vc
  :config
  (setq diff-hl-draw-borders nil)
  (setq diff-hl-side 'left)
  :hook ((after-init . global-diff-hl-mode)))

(use-package hl-line
  :config
  (setq hl-line-sticky-flag nil))

(use-package emacs
  :config
  (defun prot/toggle-invisibles ()
    "Toggles the display of indentation and space characters."
    (interactive)
    (if (bound-and-true-p whitespace-mode)
        (whitespace-mode -1)
      (whitespace-mode)))

  (defun prot/toggle-line-numbers ()
    "Toggles the display of line numbers.  Applies to all buffers."
    (interactive)
    (if (bound-and-true-p display-line-numbers-mode)
        (display-line-numbers-mode -1)
      (display-line-numbers-mode)))
  :bind (("<f6>" . prot/toggle-invisibles)
         ("<f7>" . prot/toggle-line-numbers)))

(use-package olivetti
  :ensure t
  :delight
  :config
  (setq olivetti-body-width 100)
  (setq olivetti-minimum-body-width 72)
  (setq olivetti-recall-visual-line-mode-entry-state t)

  (defun prot/toggle-olivetti-mode ()
    "Toggle `olivetti-mode' without fringes and larger fonts."
    (interactive)
    (if olivetti-mode
        (progn
          (olivetti-mode -1)
          (fringe-mode '(8 . 8))
          (prot/fonts-per-monitor))
      (olivetti-mode 1)
      (fringe-mode '(0 . 0))
      (prot/reading-fonts)))
  :bind ("C-c o" . prot/toggle-olivetti-mode))

(use-package rainbow-blocks
  :ensure t
  :delight
  :commands rainbow-blocks-mode
  :config
  (setq rainbow-blocks-highlight-braces-p t)
  (setq rainbow-blocks-highlight-brackets-p t)
  (setq rainbow-blocks-highlight-parens-p t))

;; ................................................... Language settings

(use-package emacs
  :config
  (setq-default fill-column 72)
  (setq sentence-end-double-space t)
  (setq sentence-end-without-period nil)
  (setq colon-double-space nil)
  :hook (after-init . column-number-mode))

(use-package subword
  :delight
  :hook (prog-mode . subword-mode))

(use-package emacs
  :hook (text-mode . (lambda ()
                       (turn-on-auto-fill)
                       (delight 'auto-fill-function nil t)
                       (setq adaptive-fill-mode t))))

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

;; ............................................. Spelling and dictionary

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

  (defun dap/ispell-toggle-dictionaries ()
    "Toggle between English and French dictionaries."
    (interactive)
    (if (string= ispell-current-dictionary "en")
        (ispell-change-dictionary "fr")
      (ispell-change-dictionary "en")))

  (defun prot/flyspell-dwim (&optional beg end)
    "Run `flyspell-region' on the active region, else toggle the
ispell dictionaries with `dap/ispell-toggle-dictionaries'."
    (interactive "r")
    (if (use-region-p)
        (flyspell-region beg end)
      (dap/ispell-toggle-dictionaries)))

  :hook (prog-mode . flyspell-prog-mode)
  :bind (("M-$" . prot/flyspell-dwim)
         ("C-M-$" . dap/ispell-toggle-dictionaries)
         :map flyspell-mode-map
         ("C-;" . nil)))

(use-package emacs
  :config
  (defun dap/dict-lookup-en-fr ()
    "Look up a definition in the WordRerefence.com en-fr dictionary.

Uses the region if it is active, otherwise the word under the
cursor."
    (interactive)
    (let ((word (if (use-region-p)
                    (buffer-substring-no-properties
                     (region-beginning) (region-end))
                  (current-word))))
      (browse-url (concat "https://www.wordreference.com/enfr/" word)))))


(use-package flycheck
  :ensure t
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))

  (flycheck-define-checker dap/git-commit-message
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

  (add-to-list 'flycheck-checkers 'dap/git-commit-message t)

  :hook (after-init . global-flycheck-mode))

(use-package flycheck-indicator
  :ensure t
  :after flycheck
  :hook (flycheck-mode . flycheck-indicator-mode))

(use-package eldoc
  :config
  (global-eldoc-mode -1))

(use-package haskell-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode))

(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

(use-package emacs
  :mode (("sxhkdrc" . conf-mode)
         ("Xmodmap" . conf-xdefaults-mode)))

;; Configure 'electric' behaviour

(use-package electric
  :config
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
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
  :hook (after-init . (lambda ()
                        (electric-indent-mode 1)
                        (electric-pair-mode -1)
                        (electric-quote-mode -1))))

;; Parentheses

(use-package paren
  :config
  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-in-periphery t)
  (setq show-paren-when-point-inside-paren nil)
  :hook (after-init . show-paren-mode))

;; Tabs, indentation, and the TAB key

;; I believe tabs, in the sense of inserting the tab character, are best
;; suited for indentation. While spaces are superior at precisely
;; aligning text. However, I understand that elisp uses its own
;; approach, which I do not want to interfere with. Also, Emacs tends to
;; perform alignments by mixing tabs with spaces, which can actually
;; lead to misalignments depending on certain variables such as the size
;; of the tab. As such, I am disabling tabs by default.

;; If there ever is a need to use different settings in other modes, we
;; can customise them via hooks. This is not an issue I have encountered
;; yet and am therefore refraining from solving a problem that does not
;; affect me.

(use-package emacs
  :config
  (setq-default tab-always-indent t)
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil))

;; ............................................................ Custom movements

;; Mark by semantic unit

(use-package expand-region
  :ensure t
  :pin gnu                              ; Prefer ELPA version
  :config
  (setq expand-region-smart-cursor t)
  :bind (("C-=" . er/expand-region)
         ("C-M-=" . er/mark-outside-pairs)
         ("C-+" . er/mark-symbol)))

;; Go to last change

(use-package goto-last-change
  :ensure t
  :bind ("C-z" . goto-last-change))

;; ............................................................. Version control

;; VC

(use-package vc-dir
  :config
  (defun prot/vc-dir-project ()
    "Unconditionally display `vc-diff' for the current project."
    (interactive)
    (vc-dir (vc-root-dir)))

  (defun prot/vc-dir-jump ()
    "Jump to present directory in a `vc-dir' buffer."
    (interactive)
    (vc-dir default-directory))

  :bind (("C-x v p" . prot/vc-dir-project)
         ("C-x v j" . prot/vc-dir-jump))) ; similar to `dired-jump'

(use-package vc-git
  :config
  (setq vc-git-diff-switches "--patch-with-stat")
  (setq vc-git-print-log-follow t))

(use-package vc-annotate
  :config
  (setq vc-annotate-display-mode 'scale) ; scale to oldest
  :bind (("C-x v a" . vc-annotate)       ; `vc-update-change-log' is not in git
         :map vc-annotate-mode-map
         ("t" . vc-annotate-toggle-annotation-visibility)))

(use-package diff
  :config
  (setq diff-default-read-only t)
  (setq diff-advance-after-apply-hunk t)
  (setq diff-update-on-the-fly t))

(use-package log-edit
  :config
  (setq log-edit-confirm 'changed)
  (setq log-edit-keep-buffer nil)
  (setq log-edit-require-final-newline t)
  (setq log-edit-setup-add-author nil))

;; ....................................................................... Email

;; Base email settings

(use-package auth-source
  :config
  (setq auth-sources '("~/.authinfo.gpg" "~/.authinfo"))
  (setq user-full-name "David Porter")
  (setq user-mail-address "david@daporter.net"))

(use-package epa-file
  :config
  (setq epa-file-cache-passphrase-for-symmetric-encryption t)
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
  (add-to-list 'mm-body-charset-encoding-alist '(utf-8 . base64))

  (defun prot/message-header-add-gcc ()
    "While `gnus' is running, add a Gcc header, if missing.

The Gcc header places a copy of the outgoing message to the
appropriate maildir directory.

In the absence of a Gcc header, the outgoing message will not
appear in the appropriate directory, though it will still be
sent.

Add this function to `message-header-setup-hook'."
    (if (gnus-alive-p)
        (progn
          (when (message-fetch-field "Gcc")
            (message-remove-header "Gcc")
            (message-add-header "Gcc: nnimap+migadu:Sent")))
      (message "Gnus is not running. No GCC field inserted.")))

  :hook ((message-header-setup . prot/message-header-add-gcc)
         (message-setup . message-sort-headers)))

;; Gnus

(use-package gnus
  :config
  (setq gnus-directory "~/news")
  (setq gnus-select-method '(nnnil))
  (setq gnus-secondary-select-methods
        '((nntp "news.gwene.org")
          (nntp "news.gmane.io")
          (nnimap "migadu"
                  (nnimap-address "imap.migadu.com"
                                  (nnimap-stream ssl)
                                  (nnimap-authinfo-file "~/.authinfo.gpg")))))

  (setq gnus-parameters '(("migadu" (posting-style
                                     (gcc "nnimap+migadu:Sent")))))
  (setq gnus-gcc-mark-as-read t)
  (setq gnus-agent t)
  (setq gnus-novice-user nil)
  (setq gnus-check-new-newsgroups 'ask-server)
  (setq gnus-read-active-file 'some)
  (setq gnus-use-dribble-file t)
  (setq gnus-always-read-dribble-file t)

  (defun dap/archive-message ()
    "Move the current message to the mailbox `Archive'."
    (interactive)
    (gnus-summary-move-article nil "nnimap+migadu:Archive"))

  :bind (("C-c m" . gnus)
         :map gnus-summary-mode-map
         ("C-c a" . dap/archive-message)))

(use-package nnmail
  :config
  (setq nnmail-expiry-wait 30))

(use-package mm-encode
  :config
  (setq mm-encrypt-option 'guided))

(use-package mml-sec
  :config
  (setq mml-secure-openpgp-encrypt-to-self t)
  (setq mml-secure-openpgp-sign-with-sender t)
  (setq mml-secure-smime-encrypt-to-self t)
  (setq mml-secure-smime-sign-with-sender t))

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
  (setq gnus-max-image-proportion 0.3)
  (setq gnus-treat-display-smileys nil)
  (setq gnus-article-mode-line-format "%G %S %m")
  (setq gnus-visible-headers
        '("^From:" "^To:" "^Cc:" "^Newsgroups:" "^Subject:" "^Date:"
          "Followup-To:" "Reply-To:" "^Organization:" "^X-Newsreader:"
          "^X-Mailer:"))
  (setq gnus-sorted-header-list gnus-visible-headers)
  :bind (:map gnus-article-mode-map
              ("s" . gnus-mime-save-part)
              ("o" . gnus-mime-copy-part)))

(use-package gnus-async
  :after gnus
  :config
  (setq gnus-asynchronous t)
  (setq gnus-use-article-prefetch 15))

(use-package gnus-group
  :after gnus
  :demand
  :config
  (setq gnus-level-subscribed 6)
  (setq gnus-level-unsubscribed 7)
  (setq gnus-level-zombie 8)
  (setq gnus-list-groups-with-ticked-articles nil)
  (setq gnus-group-sort-function
        '((gnus-group-sort-by-unread)
          (gnus-group-sort-by-alphabet)
          (gnus-group-sort-by-rank)))
  (setq gnus-group-mode-line-format "%%b")
  :hook
  (gnus-group-mode . hl-line-mode)
  (gnus-select-group-hook . gnus-group-set-timestamp)
  :bind (:map gnus-agent-group-mode-map
              ("M-n" . gnus-topic-goto-next-topic)
              ("M-p" . gnus-topic-goto-previous-topic)))

(use-package gnus-topic
  :after (gnus gnus-group)
  :config
  (setq gnus-topic-display-empty-topics nil)
  :hook
  (gnus-group-mode . gnus-topic-mode))

(use-package gnus-sum
  :after (gnus gnus-group)
  :demand
  :config
  (setq gnus-auto-select-first nil)
  (setq gnus-summary-ignore-duplicates t)
  (setq gnus-suppress-duplicates t)
  (setq gnus-summary-goto-unread nil)
  (setq gnus-summary-make-false-root 'none)
  (setq gnus-summary-thread-gathering-function
        'gnus-gather-threads-by-references)
  (setq gnus-thread-sort-functions
        '(gnus-thread-sort-by-number
          gnus-thread-sort-by-date))
  (setq gnus-subthread-sort-functions 'gnus-thread-sort-by-date)
  (setq gnus-user-date-format-alist
        '(((gnus-seconds-today) . "Today at %R")
          ((+ 86400 (gnus-seconds-today)) . "Yesterday, %R")
          (t . "%Y-%m-%d %R")))
  (setq gnus-summary-line-format
        "%U%R%z %-16,16&user-date;  %4L:%-30,30f  %B%S\n")
  (setq gnus-summary-mode-line-format "%p")
  (setq gnus-sum-thread-tree-indent          "  ")
  (setq gnus-sum-thread-tree-root            "")
  (setq gnus-sum-thread-tree-false-root      "")
  (setq gnus-sum-thread-tree-single-indent   "")
  (setq gnus-sum-thread-tree-leaf-with-other "├─➤ ")
  (setq gnus-sum-thread-tree-vertical        "│ ")
  (setq gnus-sum-thread-tree-single-leaf     "└─➤ ")

  :hook
  (gnus-summary-mode . hl-line-mode)
  (gnus-summary-exit-hook . gnus-topic-sort-groups-by-alphabet)
  (gnus-summary-exit-hook . gnus-group-sort-groups-by-rank)
  :bind (:map gnus-agent-summary-mode-map
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

(use-package gnus-srvr
  :after gnus
  :hook
  ((gnus-browse-mode gnus-server-mode) . hl-line-mode))

(use-package gnus-dired
  :after (gnus dired)
  :hook (dired-mode . gnus-dired-mode))

;; .............................................................. Elfeed

(use-package elfeed
  :ensure t
  :commands elfeed
  :config
  (setq elfeed-use-curl t)
  (setq elfeed-curl-max-connections 10)
  (setq elfeed-db-directory "~/.emacs.d/elfeed")
  (setq elfeed-enclosure-default-dir "~/dl")
  (setq elfeed-search-clipboard-type 'CLIPBOARD)
  (setq elfeed-search-title-max-width (current-fill-column))
  (setq elfeed-search-title-max-width 100)
  (setq elfeed-search-title-min-width 30)
  (setq elfeed-search-trailing-width 16)
  (setq elfeed-show-truncate-long-urls t)
  (setq elfeed-show-unique-buffers t)
  (setq elfeed-sort-order 'ascending)

  (defun prot/feeds ()
    "Loads a file with RSS/Atom feeds.  This file contains valid
syntax for use by the `elfeed' package."
    (let ((feeds "~/.emacs.d/feeds.el.gpg"))
      (when (file-exists-p feeds)
        (message "* loading feeds")
        (load-file feeds))))

  (defun ambrevar/elfeed-play-with-mpv ()
    "Play entry link with mpv."
    (interactive)
    (let ((entry (if (eq major-mode 'elfeed-show-mode)
                     elfeed-show-entry (elfeed-search-selected :single)))
          (quality-arg "")
          (quality-val (completing-read "Resolution: "
                                        '("480" "720" "1080")
                                        nil nil)))
      (setq quality-val (string-to-number quality-val))
      (message "Opening %s with height≤%s..."
               (elfeed-entry-link entry) quality-val)
      (when (< 0 quality-val)
        (setq quality-arg
              (format "--ytdl-format=[height<=?%s]" quality-val)))
      (start-process "elfeed-mpv" nil "mpv"
                     quality-arg (elfeed-entry-link entry))))

  :hook (elfeed-search-mode . prot/feeds)
  :bind (:map elfeed-search-mode-map
              ("v" . (lambda ()
                       (interactive)
                       (ambrevar/elfeed-play-with-mpv)
                       (elfeed-search-untag-all-unread)))
              ("w" . elfeed-search-yank)
              ("g" . elfeed-update)
              ("G" . elfeed-search-update--force)
              :map elfeed-show-mode-map
              ("v" . ambrevar/elfeed-play-with-mpv)
              ("w" . elfeed-show-yank)))

;; ............................................ Emacs server and desktop

(use-package server
  :hook (after-init . server-start))

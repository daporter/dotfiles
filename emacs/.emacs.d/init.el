;; Emacs Configuration
;; =============================================================================

;; ................................................................. use-package

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/"))

(unless package--initialized (package-initialize))

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
  ;; The following is VERY IMPORTANT.  Write hooks using their real name
  ;; instead of a shorter version: after-init ==> `after-init-hook'.
  ;;
  ;; This is to empower help commands with their contextual awareness,
  ;; such as `describe-symbol'.
  (setq use-package-hook-name-suffix nil))

(eval-when-compile
  (require 'use-package))

;; ............................................................... Base settings

;; Modeline "lighters"
(use-package diminish
  :ensure
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
  :commands (prot/font-line-spacing
             prot/font-fonts-per-monitor)
  :config
  (setq x-underline-at-descent-line nil)
  (setq underline-minimum-offset 0)

  (defconst prot/font-default-font-family "Hack"
    "The default typeface.
This is meant to be applied to the `default' and `fixed-pitch'
faces.")

  ;; The continuation of Fira Sans (I wish Fira Mono had italics,
  ;; because I like it even more than Hack).
  (defconst prot/font-variable-pitch-font-family "FiraGO"
    "The default proportionately-spaced typeface.
This is meant to be applied to the `variable-pitch' face.")

  (defconst prot/font-fontconfig-parameters
    "autohint=true:hintstyle=hintfull:embeddedbitmap=false"
    "Additional parameters for the given font family.
These are specific to the fontconfig backend for GNU/Linux systems.")

  (defvar prot/font-switch-fonts-hook nil
    "Hook that is called from `prot/font-set-fonts-completion'.")

  (defconst prot/font-sizes-alist
    '(("laptop" . 8.5)
      ("desktop" . 9.5))
    "Alist of desired font point sizes.")

  (defun prot/set-face-attribute-font (face family size &optional parameters)
    "Set FACE font to FAMILY at SIZE with optional PARAMETERS."
    (let ((params (if parameters
                      parameters
                    prot/font-fontconfig-parameters)))
      (set-face-attribute
       `,face nil :font
       (concat family "-" (number-to-string size) ":" params))))

  (defun prot/font-set-fonts-completion (&optional points font-mono font-var)
    "Set default font size using presets or in specified POINTS."
    (interactive)
    (let* ((displays (mapcar #'car prot/font-sizes-alist))
           (choice (unless points
                     (completing-read "Pick font size: " displays nil t)))
           (size (if points
                     points
                   (cdr (assoc `,choice prot/font-sizes-alist))))
           (mono (if font-mono font-mono prot/font-default-font-family))
           (var (if font-var font-var prot/font-variable-pitch-font-family)))
      (when window-system
        (prot/set-face-attribute-font 'default mono size)
        (prot/set-face-attribute-font 'fixed-pitch mono size)
        ;; Increasing the size on this to account for the innate
        ;; difference between the families I use.  Maybe there is some
        ;; more flexible way of creating visual harmony between
        ;; typefaces with distinct inherent heights.
        ;;
        ;; TODO normalise multi-font heights?
        (prot/set-face-attribute-font 'variable-pitch var (+ size 1))))
    (run-hooks 'prot/font-switch-fonts-hook))

  ;; TODO a better approach would be to check for a list of fonts that
  ;; are known to require further tweaks and which I might want to use.
  (defun prot/font-line-spacing ()
    "Determine desirable `line-spacing', based on font family.
Add this to `prot/font-switch-fonts-hook'."
    (if (string= (face-attribute 'default :family) "Source Code Pro")
        (setq-default line-spacing 1)
      (setq-default line-spacing nil)))

  (defun prot/font-fonts-per-monitor (&optional font)
    "Use font settings based on screen size."
    (if (<= (display-pixel-width) 1600)
        (prot/font-set-fonts-completion 8.5)
      (prot/font-set-fonts-completion 9.5)))

  :hook ((after-init-hook . prot/font-fonts-per-monitor)
         (prot/font-switch-fonts-hook . prot/font-line-spacing))
  ;; Awkward key because I do not need it very often.  Maybe once a day.
  ;; The "C-c f" is used elsewhere.
  :bind ("C-c F" . prot/font-set-fonts-completion))

(use-package emacs
  :config
  (defconst prot/variable-pitch-font "DejaVu Sans Condensed"
    "The default variable-pitch typeface.")

  (set-face-attribute 'variable-pitch nil :family prot/variable-pitch-font :height 1.0)
  (set-face-attribute 'fixed-pitch nil :family prot/default-font :height 1.0))

(use-package face-remap
  :diminish buffer-face-mode            ; the actual mode
  :config
  (defun prot/variable-pitch-mode ()
    "Toggle `variable-pitch-mode' and additional parameters."
    (interactive)
    (if (bound-and-true-p buffer-face-mode)
        (progn
          (variable-pitch-mode -1)
          (setq-local cursor-type 'box))   ; TODO better restore original value
      (variable-pitch-mode 1)
      (setq-local cursor-type 'bar))))

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

    (setq orderless-style-dispatchers '(prot/orderless-literal-dispatcher
                                        prot/orderless-initialism-dispatcher))
    :bind (:map minibuffer-local-completion-map
                ("SPC" . nil)         ; space should never complete
                ("?" . nil)))         ; valid regexp character


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
    (when arg
      (let ((help (get-buffer-window "*Help*")))
        (when help
          (if (not (eq (selected-window) help))
              (select-window help)
            (select-window (get-mru-window)))))))

  (defun prot/completions-kill-save-symbol ()
    "Add symbol-at-point to the kill ring.

Intended for use in the \\*Completions\\* buffer.  Bind this to a
key in `completion-list-mode-map'."
    (interactive)
    (kill-new (thing-at-point 'symbol)))

  ;; Defines, among others, aliases for common actions to Super-KEY.
  ;; Normally these should go in individual package declarations, but
  ;; their grouping here makes things easier to understand.
  :bind (("s-f" . find-file)
         ("s-F" . find-file-other-window)
         ("s-d" . dired)
         ("s-D" . dired-other-window)
         ("s-b" . switch-to-buffer)
         ("s-B" . switch-to-buffer-other-window)
         ("C-s-h" . prot/describe-symbol-at-point)
         ("C-s-H" . (lambda ()
                      (interactive)
                      (prot/describe-symbol-at-point '(4))))
         ("s-v" . prot/focus-minibuffer-or-completions)
         :map minibuffer-local-completion-map
         ("<return>" . minibuffer-force-complete-and-exit)
         ("C-j" . exit-minibuffer)
         :map completion-list-mode-map
         ("h" . prot/describe-symbol-at-point)
         ("w" . prot/completions-kill-save-symbol)
         ("n" . next-line)
         ("p" . previous-line)
         ("f" . next-completion)
         ("b" . previous-completion)
         ("M-v" . prot/focus-minibuffer)))

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
      (pulse-momentary-highlight-one-line (point) 'modus-theme-intense-red)))

  :hook ((imenu-after-jump-hook . prot/imenu-recenter-pulse)
         (imenu-after-jump-hook . (lambda ()
                                    (when (and (eq major-mode 'org-mode)
                                               (org-at-heading-p))
                                      (org-show-entry)
                                      (org-reveal t)))))
  :bind ("C-." . prot/imenu-vertical))

(use-package imenu-list
  :ensure
  :defer
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

(use-package flimenu
  :ensure
  :config
  (flimenu-global-mode 1))

(use-package savehist
  :config
  (setq savehist-file "~/.emacs.d/savehist")
  (setq history-length 30000)
  (setq history-delete-duplicates nil)
  (setq savehist-save-minibuffer-history t)
  (savehist-mode 1))

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
  ;; (setq icomplete-separator " · ")
  ;; (setq icomplete-separator " │ ")
  ;; (setq icomplete-separator " ┊ ")
  (setq icomplete-separator (propertize " ┆ " 'face 'shadow))
  (setq icomplete-with-completion-tables t)
  (setq icomplete-in-buffer t)
  (setq icomplete-tidy-shadowed-file-names nil)

  ;;(fido-mode -1)                        ; Emacs 27.1
  (icomplete-mode 1)

  (defun prot/icomplete-minibuffer-truncate ()
    "Truncate minibuffer lines in `icomplete-mode'.
  This should only affect the horizontal layout and is meant to
  enforce `icomplete-prospects-height' being set to 1.

  Hook it to `icomplete-minibuffer-setup-hook'."
    (when (and (minibufferp)
               (bound-and-true-p icomplete-mode))
      (setq truncate-lines t)))

  :hook (icomplete-minibuffer-setup-hook . prot/icomplete-minibuffer-truncate)
  :bind (:map icomplete-minibuffer-map
              ("<tab>" . icomplete-force-complete)
              ("<return>" . icomplete-force-complete-and-exit) ; exit with completion
              ("C-j" . exit-minibuffer) ; force current input unconditionally
              ("C-n" . icomplete-forward-completions)
              ("<right>" . icomplete-forward-completions)
              ("<down>" . icomplete-forward-completions)
              ("C-p" . icomplete-backward-completions)
              ("<left>" . icomplete-backward-completions)
              ("<up>" . icomplete-backward-completions)
              ("<C-backspace>" . icomplete-fido-backward-updir)))

;; ............................................................. Recentf

(use-package recentf
  :config
  (setq recentf-save-file "~/.emacs.d/recentf")
  (setq recentf-max-saved-items 200)
  (setq recentf-exclude '(".gz" ".xz" ".zip" "/elpa/" "/ssh:" "/doas:"))

  (defun prot/recentf-keep-predicate (file)
    "Additional conditions for saving in `recentf-list'.
Add this function to `recentf-keep'.

NEEDS REVIEW."
    (cond
     ((file-directory-p file) (file-readable-p file))))
  (add-to-list 'recentf-keep 'prot/recentf-keep-default-predicate)

  (defun prot/recentf ()
    "Select item from `recentf-list' using completion.
The user's $HOME directory is abbreviated as a tilde."
    (interactive)
    (icomplete-vertical-do ()
      (let ((files (mapcar 'abbreviate-file-name recentf-list)))
        (find-file
         (completing-read "Open recentf entry: " files nil t)))))

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

;; Icomplete vertical mode

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

;; Completion for projects and directory trees

(use-package project
  :after (minibuffer icomplete icomplete-vertical) ; read those
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
        (let* ((filenames-all (directory-files-recursively default-directory ".*" t))
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
    (let* ((dirs (list "~/projects/"))
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

;; ............................................... In-buffer completions

(use-package emacs
  :after (minibuffer icomplete icomplete-vertical) ; review those first
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

;; Dabbrev (dynamic word completion)

(use-package dabbrev
  :after (minibuffer icomplete icomplete-vertical) ; read those as well
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
  :bind (("M-/" . dabbrev-expand)
         ("C-M-/" . dabbrev-completion)
         ("s-/" . dabbrev-completion)))

;; ............................................................. Isearch

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

  :bind (("M-s %" . prot/isearch-query-replace-symbol-at-point)
         ("s-s" . prot/isearch-for-region)
         :map minibuffer-local-isearch-map
         ("M-/" . isearch-complete-edit)
         :map isearch-mode-map
         ("C-g" . isearch-cancel)       ; instead of `isearch-abort'
         ("M-/" . isearch-complete)
         ("C-SPC" . prot/isearch-mark-and-exit)
         ("<backspace>" . prot/isearch-abort-dwim)
         ("<C-return>" . prot/isearch-other-end)))

;; ................................................. Regular expressions

(use-package re-builder
  :config
  (setq reb-re-syntax 'read))

(use-package visual-regexp
  :ensure
  :config
  (setq vr/default-replace-preview nil)
  (setq vr/match-separator-use-custom-face t))

(use-package replace
  :config
  (setq list-matching-lines-jump-to-current-line t)
  ;; See my "Modus themes" for these inherited faces
  (setq list-matching-lines-buffer-name-face
        '(:inherit modus-theme-intense-neutral :weight bold))
  (setq list-matching-lines-current-line-face
        '(:inherit modus-theme-special-mild))
  :hook ((occur-mode-hook . hl-line-mode)
         (occur-mode-hook . (lambda ()
                              (toggle-truncate-lines t))))
  :bind (("M-s M-o" . multi-occur)
         :map occur-mode-map
         ("t" . toggle-truncate-lines)))

;; wgrep

(use-package wgrep
  :ensure
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t))

;; ripgrep

(use-package rg
  :ensure
  :after wgrep
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

(use-package ibuffer-vc
  :ensure
  :after (ibuffer vc)
  :bind (:map ibuffer-mode-map
              ("/ V" . ibuffer-vc-set-filter-groups-by-vc-root)
              ("/ <deletechar>" . ibuffer-clear-filter-groups)))

;; ............................................................. Windows

(use-package emacs
  :config
  (defvar prot/window-configuration nil
    "Current window configuration.
Intended for use by `prot/window-monocle'.")

  (defun prot/window-single-toggle ()
    "Toggle between multiple windows and single window.
This is the equivalent of maximising a window.  Tiling window
managers such as DWM, BSPWM refer to this state as 'monocle'."
    (interactive)
    (if (one-window-p)
        (when prot/window-configuration
          (set-window-configuration prot/window-configuration))
      (setq prot/window-configuration (current-window-configuration))
      (delete-other-windows)))
  :bind ("s-m" . prot/window-single-toggle))

(use-package window
  :init
  (setq display-buffer-alist
        '(;; top side window
          ("\\*elfeed-mpv-output.*"
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
  (setq switch-to-buffer-in-dedicated-window 'pop)
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
  :hook (after-init-hook . winner-mode)
  :bind (("<s-right>" . winner-redo)
         ("<s-left>" . winner-undo)))

(use-package windmove
  :config
  (setq windmove-create-window nil)
  :bind (("s-k" . windmove-up)
         ("s-l" . windmove-right)
         ("s-j" . windmove-down)
         ("s-h" . windmove-left)))

;; ............................................................... Dired

(use-package dired
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-listing-switches "-AFlh")
  (setq dired-dwim-target t)
  :hook ((dired-mode-hook . dired-hide-details-mode)
         (dired-mode-hook . hl-line-mode)))

(use-package dired-aux
  :config
  (setq dired-isearch-filenames 'dwim)
  ;; The following variables were introduced in Emacs 27.1
  (setq dired-create-destination-dirs 'ask)
  (setq dired-vc-rename-file t)

  ;; TODO defmacro to avoid duplication of code in `fd' functions
  ;; TODO how can a defmacro produce named functions that are then
  ;; mapped to keys?
  (defun prot/dired-fd-dirs (&optional arg)
    "Search for directories in VC root or PWD.
With \\[universal-argument] put the results in a `dired' buffer.
This relies on the external 'fd' executable."
    (interactive "P")
    (let* ((vc (vc-root-dir))
           (dir (expand-file-name (if vc vc default-directory)))
           (regexp (read-regexp
                    (concat "Subdirectories matching REGEXP in "
                            (propertize dir 'face 'bold)
                            ": ")))
           (names (process-lines "fd" "-i" "-H" "-a" "-t" "d" "-c" "never" regexp dir))
           (buf "*FD Dired*"))
      (if names
          (if arg
              (dired (cons (generate-new-buffer-name buf) names))
            (icomplete-vertical-do ()
              (find-file
               (completing-read (concat
                                 "Files or directories matching "
                                 (propertize regexp 'face 'success)
                                 (format " (%s)" (length names))
                                 ": ")
                                names nil t)))))
      (user-error (concat "No matches for " "«" regexp "»" " in " dir))))

  (defun prot/dired-fd-files-and-dirs (&optional arg)
    "Search for files and directories in VC root or PWD.
With \\[universal-argument] put the results in a `dired' buffer.
This relies on the external 'fd' executable."
    (interactive "P")
    (let* ((vc (vc-root-dir))
           (dir (expand-file-name (if vc vc default-directory)))
           (regexp (read-regexp
                    (concat "Files and dirs matching REGEXP in "
                            (propertize dir 'face 'bold)
                            ": ")))
           (names (process-lines "fd" "-i" "-H" "-a" "-t" "d" "-t" "f" "-c" "never" regexp dir))
           (buf "*FD Dired*"))
      (if names
          (if arg
              (dired (cons (generate-new-buffer-name buf) names))
            (icomplete-vertical-do ()
              (find-file
               (completing-read (concat
                                 "Files and directories matching "
                                 (propertize regexp 'face 'success)
                                 (format " (%s)" (length names))
                                 ": ")
                                names nil t)))))
      (user-error (concat "No matches for " "«" regexp "»" " in " dir))))

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

(use-package find-dired
  :after dired
  :config
  (setq find-ls-option '("-ls" . "-AFhl"))
  (setq find-name-arg "-iname"))

(use-package async :ensure)

(use-package dired-async
  :after (dired async)
  :hook (dired-mode-hook . dired-async-mode))

(use-package dired-narrow
  :ensure
  :after dired
  :config
  (setq dired-narrow-exit-when-one-left t)
  (setq dired-narrow-enable-blinking t)
  (setq dired-narrow-blink-time 0.3)
  :bind (:map dired-mode-map
              ("/" . dired-narrow-regexp)))

(use-package wdired
  :after dired
  :commands wdired-change-to-wdired-mode
  :config
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))

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

(use-package dired-subtree
  :ensure
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

(use-package diredfl
  :ensure
  :config
  (setq diredfl-ignore-compressed-flag nil)
  :hook (dired-mode-hook . diredfl-mode))

;; ........................................................ Applications

(use-package calendar
  :config
  (setq calendar-mark-diary-entries-flag t)
  (setq calendar-time-display-form
        '(24-hours ":" minutes
                   (when time-zone
                     (concat " (" time-zone ")"))))
  (setq calendar-week-start-day 1)      ; Monday
  (setq calendar-date-style 'iso)
  (setq calendar-holidays
        (append holiday-general-holidays holiday-local-holidays
                holiday-other-holidays holiday-christian-holidays
                holiday-islamic-holidays holiday-oriental-holidays
                holiday-solar-holidays))

  (use-package solar
    :config
    (setq calendar-latitude 48.85
          calendar-longitude 2.29))

  :hook (calendar-today-visible-hook . calendar-mark-today))

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

(use-package org-plus-contrib
  :ensure
  :custom
  ;; agenda and basic directory structure
  (org-directory "~/org/")
  (org-archive-location "archive/%s_archive::")
  (org-archive-file-header-format
   "#+FILETAGS: ARCHIVE\nArchived entries from file %s\n")
  (org-deadline-warning-days 3)
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)

  ;; Include the todo keywords
  (org-fast-tag-selection-include-todo t)
  (org-use-fast-todo-selection t)
  (org-todo-keywords
   '((sequence "TODO(t)" "WAITING(w@/!)" "|" "DONE(d)" "CANCELLED(c@/!)")))

  (org-fontify-done-headline nil)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-heading-line nil)
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-track-ordered-property-with-tag t)
  ;; log
  (org-log-done 'time)
  (org-log-note-clock-out nil)
  (org-log-redeadline nil)
  (org-log-reschedule nil)
  (org-read-date-prefer-future 'time)
  ;; general
  (org-special-ctrl-a/e t)
  (org-hide-emphasis-markers t)
  (org-catch-invisible-edits 'show)
  (org-loop-over-headlines-in-active-region 'start-level)
  (org-imenu-depth 7)

  :init
  (require 'org-checklist)
  
  :hook (org-mode-hook . org-indent-mode)
  :bind (:map org-mode-map
              ("<C-return>" . nil)
              ("<C-S-return>" . nil)))

(use-package ol
  :config
  (setq org-link-keep-stored-after-insertion t)
  :bind (:map org-mode-map
              ("C-c l" . org-store-link)
              ("C-c S-l" . org-toggle-link-display)
              ("C-c C-S-l" . org-insert-last-stored-link)))

(use-package org-agenda
  :custom
  ;; Show the daily agenda by default instead of the weekly one.
  ;; Make the global TODO list into a list of tasks available for
  ;; scheduling.
  (org-agenda-span 'day)
  (org-agenda-todo-ignore-scheduled t)
  (org-agenda-todo-ignore-timestamp t)
  
  (org-refile-targets
   '(("~/gtd/projects.org" :maxlevel . 3)
     ("~/gtd/maybe.org" :level . 1)))
  (org-agenda-files
   '("~/gtd/inbox.org"
     "~/gtd/projects.org"))
  (org-agenda-custom-commands
   '(("A" "Agenda"
      ((agenda "")
       (todo "TODO"
             ((org-agenda-overriding-header "To Refile:")
              (org-agenda-files '("~/gtd/inbox.org"))))))
     ("n" "Next tasks"
      ((todo "TODO"
             ((org-agenda-overriding-header "Next tasks:")))))))

  :bind (("C-c a" . org-agenda)
         ("s-a" . org-agenda)))

(use-package org-habit
  :after org-plus-contrib
  :config
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 80)
  (setq org-habit-show-habits-only-for-today t))

(use-package org-cliplink
  :ensure
  :after org-plus-contrib
  :demand
  :bind ("C-c y l" . org-cliplink))

(use-package org-protocol
  :demand)

(use-package org-capture
  :config
  (setq org-capture-templates
        '(("t" "Todo" entry (file "~/gtd/inbox.org")
           "* TODO %?\n%U\n%a\n")
          ("r" "Reply to an email" entry (file+headline "~/gtd/inbox.org")
           "* TODO Reply to %:from on  %:subject\n SCHEDULED: %t\n%U\n%a\n"
           :immediate-finish t)
          ("l" "link" entry (file "~/gtd/inbox.org")
           "* TODO %(org-cliplink-capture)" :immediate-finish t)
          ("c" "org-protocol-capture" entry (file "~/gtd/inbox.org")
           "* TODO [[%:link][%:description]]\n%:initial" :immediate-finish t)))

  (setq org-capture-templates-contexts
        '(("r" ((in-mode . "gnus2-article-mode")
                (in-mode . "gnus-summary-mode")))))

  :bind ("C-c c" . org-capture))

(use-package org-list
  :config
  (setq org-list-allow-alphabetical t))

(use-package org-src
  :after org-plus-contrib
  :config
  (setq org-src-window-setup 'current-window)
  (setq org-src-preserve-indentation t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 0))

(use-package htmlize
  :ensure
  :after org-plus-contrib
  (setq htmlize-ignore-face-size t))

(use-package org-roam
  :ensure
  :diminish
  :custom
  (org-roam-directory "~/Sync/notes/")
  :hook
  (after-init-hook . org-roam-mode)
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n b" . org-roam-switch-to-buffer))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))

(use-package org-journal
  :ensure
  :after org-roam
  :custom
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir org-roam-directory)
  (org-journal-date-format "%A, %d %B %Y")
  :bind
  ("C-c n j" . org-journal-new-entry))

(use-package org-ref
  :ensure
  :config
  (setq reftex-default-bibliography '("~/Sync/bibliography/references.bib"))
  (setq org-ref-default-bibliography reftex-default-bibliography)
  (setq org-ref-bibliography-notes "~/Sync/bibliography/notes.org")
  (setq org-ref-pdf-directory "~/Sync/bibliography/bibtex-pdfs/"))

(use-package deft
  :ensure
  :after org-roam
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory)
  :bind
  ("C-c n d" . deft))

(use-package pdf-tools
  :ensure
  :config
  (pdf-tools-install))

(use-package nov
  :ensure
  :mode ("\\.epub\\'" . nov-mode))

;; ............................................................... Theme

;; Disable GUI components
(use-package emacs
  :init
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  :config
  (setq use-file-dialog nil)
  (setq use-dialog-box t)       ; only for mouse events
  (setq inhibit-splash-screen t)
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z"))
  (global-unset-key (kbd "C-h h")))

;; Themes

(use-package modus-vivendi-theme
  :ensure
  :init
  (setq modus-vivendi-theme-slanted-constructs t
        modus-vivendi-theme-bold-constructs t
        modus-vivendi-theme-visible-fringes t
        modus-vivendi-theme-proportional-fonts t
        modus-vivendi-theme-distinct-org-blocks t
        modus-vivendi-theme-rainbow-headings t
        modus-vivendi-theme-scale-headings t))

(use-package modus-operandi-theme
  :ensure
  :init
  (setq modus-operandi-theme-slanted-constructs t
        modus-operandi-theme-bold-constructs t
        modus-operandi-theme-visible-fringes t
        modus-operandi-theme-proportional-fonts t
        modus-operandi-theme-distinct-org-blocks t
        modus-operandi-theme-rainbow-headings t
        modus-operandi-theme-scale-headings t)
  :config
  (load-theme 'modus-operandi t))

;; ........................................................... Mode line

(use-package emacs
  :commands contrib/toggle-mode-line
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
                  mode-line-end-spaces))

  (defun contrib/toggle-mode-line ()
    "Toggle modeline visibility in the current buffer."
    (interactive)
    (if mode-line-format
        (setq-local mode-line-format nil)
      (kill-local-variable 'mode-line-format))))

(use-package battery
  :config
  (setq battery-mode-line-format " [%b%p%%]")
  (setq battery-mode-line-limit 97)
  (setq battery-update-interval 180)
  (setq battery-load-low 20)
  (setq battery-load-critical 10)
  :hook (after-init-hook . display-battery-mode))

(use-package time
  :config
  (setq display-time-format "%Y-%m-%d  %H:%M")
  ;;;; Covered by `display-time-format'
  ;; (setq display-time-24hr-format t)
  ;; (setq display-time-day-and-date t)
  (setq display-time-interval 60)
  (setq display-time-mail-directory nil)
  (setq display-time-default-load-average nil)
  :hook (after-init-hook . display-time-mode))

;; ...................................... Window elements and indicators

(use-package emacs
  :config
  (setq window-divider-default-right-width 1)
  (setq window-divider-default-bottom-width 1)
  (setq window-divider-default-places 'right-only)
  :hook (after-init-hook . window-divider-mode))

(use-package fringe
  :config
  (fringe-mode nil)
  (setq-default fringes-outside-margins nil)
  (setq-default indicate-buffer-boundaries nil)
  (setq-default indicate-empty-lines nil)
  (setq-default overflow-newline-into-fringe t))

(use-package diff-hl
  :ensure
  :after vc
  :config
  (setq diff-hl-draw-borders nil)
  (setq diff-hl-side 'left)
  :hook ((after-init-hook . global-diff-hl-mode)))

(use-package hl-line
  :config
  (setq hl-line-sticky-flag nil))

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

(use-package display-line-numbers
  :config
  (defun prot/toggle-line-numbers ()
    "Toggles the display of line numbers.  Applies to all buffers."
    (interactive)
    (if (bound-and-true-p display-line-numbers-mode)
        (display-line-numbers-mode -1)
      (display-line-numbers-mode)))
  :bind ("<f7>" . prot/toggle-line-numbers))

(use-package olivetti
  :ensure
  :diminish
  :config
  (setq olivetti-body-width 100)
  (setq olivetti-minimum-body-width 80)
  (setq olivetti-recall-visual-line-mode-entry-state t)

  (defun prot/olivetti-mode ()
    "Toggle `olivetti-mode' with additional parameters.
Fringes are disabled for the current window.  For the
font-related changes see `prot/variable-pitch-mode'."
    (interactive)
    (if (bound-and-true-p olivetti-mode)
        (progn
          (olivetti-mode -1)
          (set-window-fringes (selected-window) nil) ; Use default width
          (prot/variable-pitch-mode)
          (contrib/toggle-mode-line))
      (olivetti-mode 1)
      (set-window-fringes (selected-window) 0 0)
      (prot/variable-pitch-mode)
      (contrib/toggle-mode-line)))
  :bind ("C-c o" . prot/olivetti-mode))

(use-package rainbow-blocks
  :ensure
  :diminish
  :commands rainbow-blocks-mode
  :config
  (setq rainbow-blocks-highlight-braces-p t)
  (setq rainbow-blocks-highlight-brackets-p t)
  (setq rainbow-blocks-highlight-parens-p t))

;; ................................................... Language settings

(use-package smartparens
  :ensure)

(use-package emacs
  :config
  (setq-default fill-column 72)
  (setq sentence-end-double-space t)
  (setq sentence-end-without-period nil)
  (setq colon-double-space nil)
  :hook (after-init-hook . column-number-mode))

(use-package subword
  :diminish
  :hook (prog-mode-hook . subword-mode))

(use-package emacs
  :diminish auto-fill-function
  :hook (text-mode-hook . (lambda ()
                            (turn-on-auto-fill))))

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

(use-package ispell
  :config
  (setq ispell-program-name "aspell")
  (setq ispell-dictionary "british-ise-w_accents")

  (defun dp/ispell-toggle-dictionaries ()
    "Toggle between English and French dictionaries."
    (interactive)
    (if (string= ispell-current-dictionary "british-ise-w_accents")
        (ispell-change-dictionary "francais-lrg")
      (ispell-change-dictionary "british-ise-w_accents")))

  :bind ("C-M-$" . dp/ispell-toggle-dictionaries))

(use-package flyspell
  :commands (flyspell-buffer
             flyspell-mode
             flyspell-region)
  :config
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  
  :bind (:map flyspell-mode-map
              ("C-;" . nil)))

(use-package emacs
  :config
  (defun dp/dict-lookup-en-fr ()
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
  :ensure
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))

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

  (add-to-list 'flycheck-checkers 'dp/git-commit-message t))

(use-package flycheck-indicator
  :ensure
  :after flycheck
  :hook (flycheck-mode-hook . flycheck-indicator-mode))

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
  :bind (:map flymake-mode-map
              ("C-c d" . flymake-show-diagnostics-buffer)
              ("C-c n" . flymake-goto-next-error)
              ("C-c p" . flymake-goto-prev-error)))

(use-package eldoc
  :diminish
  :config
  (global-eldoc-mode 1))

(use-package haskell-mode
  :ensure)

(use-package markdown-mode
  :ensure
  :config
  (setq markdown-fontify-code-blocks-natively t)
  :mode ("\\.md$" . markdown-mode))

(use-package yaml-mode
  :ensure
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
  :hook (after-init-hook . (lambda ()
                             (electric-indent-mode 1)
                             (electric-pair-mode -1)
                             (electric-quote-mode -1))))

;; Parentheses

(use-package paren
  :config
  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-in-periphery t)
  (setq show-paren-when-point-inside-paren nil)
  :hook (after-init-hook . show-paren-mode))

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
  :ensure
  :pin gnu                              ; Prefer ELPA version
  :config
  (setq expand-region-smart-cursor t)
  :bind (("C-=" . er/expand-region)
         ("C-M-=" . er/mark-outside-pairs)
         ("C-+" . er/mark-symbol)))

;; Go to last change

(use-package goto-last-change
  :ensure
  :bind ("C-z" . goto-last-change))

(use-package goto-last-change
  :ensure
  :bind ("C-z" . goto-last-change))

;; ................................................... Cursor and mouse settings

(use-package emacs
  :config
  (setq-default cursor-type 'box)
  (setq-default cursor-in-non-selected-windows '(bar . 2))
  (setq-default blink-cursor-blinks 50)
  (setq-default blink-cursor-interval 0.75)
  (setq-default blink-cursor-delay 0.2)
  :hook (after-init-hook . blink-cursor-mode))

(use-package pulse
  :config
  (defun prot/pulse-line (&optional face)
    "Temporarily highlight the current line."
    (interactive)
    (let ((pulse-delay .06)
          (face
           (if face
               face
             'modus-theme-intense-red)))
      (pulse-momentary-highlight-one-line (point) face)))
  :bind ("<s-escape>" . prot/pulse-line))

(use-package mouse
  :config
  ;; In Emacs 27, use Control + mouse wheel to scale text.
  (setq mouse-wheel-scroll-amount
        '(1
          ((shift) . 5)
          ((meta) . 0.5)
          ((control) . text-scale)))
  (setq mouse-drag-copy-region t)
  (setq make-pointer-invisible t)
  (setq mouse-wheel-progressive-speed t)
  (setq mouse-wheel-follow-mouse t)
  :hook (after-init-hook . mouse-wheel-mode))

(use-package emacs
  :config
  (setq scroll-preserve-screen-position t)
  (setq scroll-conservatively 1)        ; affects `scroll-step'
  (setq scroll-margin 0))

(use-package tooltip
  :config
  (setq tooltip-delay 0.5)
  (setq tooltip-short-delay 0.5)
  (setq x-gtk-use-system-tooltips nil)
  :hook (after-init-hook . tooltip-mode))

(use-package autorevert
  :diminish
  :config
  (setq auto-revert-verbose t)
  :hook (after-init-hook . global-auto-revert-mode))

(use-package emacs
  :config
  (setq save-interprogram-paste-before-kill t))

(use-package emacs
  :config
  (setq frame-title-format '("%b"))
  (setq echo-keystrokes 0.25)
  (setq ring-bell-function 'ignore)

  (defalias 'yes-or-no-p 'y-or-n-p)
  (put 'narrow-to-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'dired-find-alternate-file 'disabled nil)
  (put 'overwrite-mode 'disabled t))

(use-package emacs
  :config
  (setq mode-require-final-newline 'visit-save))

;; ............................................................. Version control

;; VC

(use-package vc
  :config
  (setq vc-find-revision-no-save t)
  (require 'log-view)                   ; needed for the key bindings
  :bind (("C-x v b" . vc-retrieve-tag)  ; "branch" switch
         ("C-x v t" . vc-create-tag)
         ("C-x v f" . vc-log-incoming)  ; the actual git fetch
         ("C-x v F" . vc-update)        ; "F" because "P" is push
         ("C-x v d" . vc-diff)
         (:map log-view-mode-map
               ("<tab>" . log-view-toggle-entry-display)
               ("<return>" . log-view-find-revision)
               ("s" . vc-log-search)
               ("o" . vc-log-outgoing)
               ("f" . vc-log-incoming)
               ("F" . vc-update)
               ("P" . vc-push))))

(use-package vc-dir
  :config
  (defun prot/vc-dir-project ()
    "Unconditionally display `vc-diff' for the current project."
    (interactive)
    (vc-dir (vc-root-dir)))
  :bind (("C-x v p" . prot/vc-dir-project)
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
  (setq vc-git-diff-switches "--patch-with-stat")
  (setq vc-git-print-log-follow t))

(use-package vc-annotate
  :config
  (setq vc-annotate-display-mode 'scale) ; scale to oldest
  :bind (("C-x v a" . vc-annotate)       ; `vc-update-change-log' is not in git
         :map vc-annotate-mode-map
         ("t" . vc-annotate-toggle-annotation-visibility)))

(use-package magit
  :ensure)

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

(use-package magit-diff
  :after magit
  :config
  ;; In the fucused hunk, highlight changes within a line, not just the
  ;; line itself.
  (setq magit-diff-refine-hunk t))

(use-package diff
  :config
  (setq diff-default-read-only t)
  (setq diff-advance-after-apply-hunk t)
  (setq diff-update-on-the-fly t)
  ;; The following are from Emacs 27.1
  (setq diff-refine nil)
  (setq diff-font-lock-prettify nil)
  (setq diff-font-lock-syntax nil)

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
  ;; (which I bind to another key---see previous section).
  :bind (("C-x v =" . prot/diff-buffer-with-file)
         :map diff-mode-map
         ("C-c C-n" . prot/diff-restrict-view-dwim)))

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
  (add-to-list 'mm-body-charset-encoding-alist '(utf-8 . base64))

  (defun prot/message-header-add-gcc ()
    "While `gnus' is running, add pre-populated Gcc header.

The Gcc header places a copy of the outgoing message to the
appropriate maildir directory.

In the absence of a Gcc header, the outgoing message will not
appear in the appropriate directory, though it will still be
sent.

Add this function to `message-header-setup-hook'."
    (if (gnus-alive-p)
        (progn
          (when (message-fetch-field "Gcc")
            (message-remove-header "Gcc"))
          (message-add-header "Gcc: nnimap+migadu:Sent"))
      (message "Gnus is not running. No GCC field inserted.")))

  :hook ((message-header-setup-hook . prot/message-header-add-gcc)
         (message-setup-hook . message-sort-headers)))

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

  (defun dp/archive-message ()
    "Move the current message to the mailbox `Archive'."
    (interactive)
    (gnus-summary-move-article nil "nnimap+migadu:Archive"))

  :bind (("C-c g" . gnus)
         ("s-g" . gnus)
         :map gnus-summary-mode-map
         ("C-c A" . dp/archive-message)))

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
  (setq gnus-max-image-proportion 0.7)
  (setq gnus-treat-display-smileys nil)
  (setq gnus-article-mode-line-format "%G %S %m")
  (setq gnus-visible-headers
        '("^From:" "^To:" "^Cc:" "^Newsgroups:" "^Subject:" "^Date:"
          "Followup-To:" "Reply-To:" "^Organization:" "^X-Newsreader:"
          "^X-Mailer:"))
  (setq gnus-sorted-header-list gnus-visible-headers)
  :bind (:map gnus-article-mode-map
              ("i" . gnus-article-show-images)
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
  (setq gnus-activate-level 4)
  (setq gnus-list-groups-with-ticked-articles nil)
  (setq gnus-group-sort-function
        '((gnus-group-sort-by-unread)
          (gnus-group-sort-by-alphabet)
          (gnus-group-sort-by-rank)))
  (setq gnus-group-mode-line-format "%%b")
  :hook
  (gnus-group-mode-hook . hl-line-mode)
  (gnus-select-group-hook . gnus-group-set-timestamp)
  :bind (:map gnus-agent-group-mode-map
              ("M-n" . gnus-topic-goto-next-topic)
              ("M-p" . gnus-topic-goto-previous-topic)))

(use-package gnus-topic
  :after (gnus gnus-group)
  :config
  (setq gnus-topic-display-empty-topics nil)
  :hook
  (gnus-group-mode-hook . gnus-topic-mode))

(use-package gnus-sum
  :after (gnus gnus-group)
  :demand
  :config
  (setq gnus-auto-select-first nil)
  (setq gnus-summary-ignore-duplicates t)
  (setq gnus-suppress-duplicates t)
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
        '(("nnimap\\+migadu:.*"
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
  ((gnus-browse-mode-hook gnus-server-mode-hook) . hl-line-mode))

(use-package gnus-dired
  :after (gnus dired)
  :hook (dired-mode-hook . gnus-dired-mode))

;; Custom movements and motions

(use-package emacs
  :config
  (defun prot/copy-line ()
    "Copies the entirety of the current line."
    (interactive)
    (copy-region-as-kill (point-at-bol) (point-at-eol))
    (message "Current line copied"))

  (defun prot/insert-double-quotes (&optional arg)
    "Insert a pair of double quotes or wrap ARG with them."
    (interactive "P")
    (insert-pair arg ?\" ?\"))

  (defun prot/insert-double-smart-quotes (&optional arg)
    "Insert a pair of double smart quotes or wrap ARG with them."
    (interactive "P")
    (insert-pair arg ?\“ ?\”))

  (defun prot/insert-single-smart-quotes (&optional arg)
    "Insert a pair of single smart quotes or wrap ARG with them."
    (interactive "P")
    (insert-pair arg ?\‘ ?\’))

  (defun prot/insert-elisp-quotes (&optional arg)
    "Insert a pair of elisp symbol quotes or wrap ARG with them."
    (interactive "P")
    (insert-pair arg ?\` ?\'))

  (defun prot/multi-line-next ()
    "Moves point 15 lines down."
    (interactive)
    (forward-line 15))

  (defun prot/multi-line-prev ()
    "Moves point 15 lines up."
    (interactive)
    (forward-line -15))

  (defun prot/kill-line-backward ()
    "Kill from point to the beginning of the line."
    (interactive)
    (kill-line 0))

  (defun prot/new-line-below ()
    "Create a new line below the current one.  Move the point to
the absolute beginning.  Also see `prot/new-line-above'."
    (interactive)
    (end-of-line)
    (newline))

  (defun prot/new-line-above ()
    "Create a new line above the current one.  Move the point to
the absolute beginning.  Also see `prot/new-line-below'."
    (interactive)
    (beginning-of-line)
    (newline)
    (forward-line -1))

  (defun contrib/rename-file-and-buffer ()
    "Rename current buffer and if the buffer is visiting a file, rename it too."
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

  (defun prot/transpose-chars ()
    "Always transposes the two characters before point.  There is
no 'dragging' the character forward.  This is the behaviour of
`transpose-chars' when point is at end-of-line."
    (interactive)
    (transpose-chars -1)
    (forward-char))

  (defun prot/transpose-or-swap-lines (arg)
    "If region is active, swap the line at mark (region
beginning) with the one at point (region end).  This leverages a
facet of the built-in `transpose-lines'.  Otherwise transpose the
current line with the one before it ('drag' line downward)."
    (interactive "p")
    (if (use-region-p)
        (transpose-lines 0)
      (transpose-lines arg)))

  (defun prot/transpose-or-swap-paragraphs (arg)
    "If region is active, swap the paragraph at mark (region
beginning) with the one at point (region end).  This leverages a
facet of the built-in `transpose-paragraphs'.  Otherwise
transpose the current paragraph with the one after it ('drag'
paragraph downward)."
    (interactive "p")
    (if (use-region-p)
        (transpose-paragraphs 0)
      (transpose-paragraphs arg)))

  (defun prot/transpose-or-swap-sentences (arg)
    "If region is active, swap the sentence at mark (region
beginning) with the one at point (region end).  This leverages a
facet of the built-in `transpose-sentences'.  Otherwise transpose
the sentence before point with the one after it ('drag' sentence
forward/downward).  Also `fill-paragraph' afterwards.

Note that, by default, sentences are demarcated by two spaces."
    (interactive "p")
    (if (use-region-p)
        (transpose-sentences 0)
      (transpose-sentences arg))
    (fill-paragraph))

  (defun prot/transpose-or-swap-words (arg)
    "If region is active, swap the word at mark (region
beginning) with the one at point (region end).

Otherwise, and while inside a sentence, this behaves as the
built-in `transpose-words', dragging forward the word behind the
point.  The difference lies in its behaviour at the end of a
line, where it will always transpose the word at point with the
one behind it (effectively the last two words).

This addresses two patterns of behaviour I dislike in the
original command:

1. When a line follows, `M-t' will transpose the last word of the
line at point with the first word of the line below.

2. While at the end of the line, `M-t' will not transpose the
last two words, but will instead move point one word backward.
To actually transpose the last two words, you need to invoke the
command twice."
    (interactive "p")
    (if (use-region-p)
        (transpose-words 0)
      (if (eq (point) (point-at-eol))
          (progn
            (backward-word 1)
            (transpose-words 1)
            (forward-char 1))
        (transpose-words arg))))

  (defun prot/unfill-region-or-paragraph (&optional region)
    "Join all lines in a region, if active, while respecting any
empty lines (so multiple paragraphs are not joined, just
unfilled).  If no region is active, operate on the paragraph.
The idea is to produce the opposite effect of both
`fill-paragraph' and `fill-region'."
    (interactive)
    (let ((fill-column most-positive-fixnum))
      (if (use-region-p)
          (fill-region (region-beginning) (region-end))
        (fill-paragraph nil region))))

  (defun prot/yank-replace-line-or-region ()
    "Replace the line at point with the contents of the last
stretch of killed text.  If the region is active, operate over it
instead.  This command can then be followed by the standard
`yank-pop' (default is bound to M-y)."
    (interactive)
    (if (use-region-p)
        (progn
          (delete-region (region-beginning) (region-end))
          (yank))
      (delete-region (point-at-bol) (point-at-eol))
      (yank)))

  :bind (("<C-M-backspace>" . backward-kill-sexp)
         ("M-c" . capitalize-dwim)
         ("M-l" . downcase-dwim)        ; "lower" case
         ("M-u" . upcase-dwim)
         ("<C-f2>" . contrib/rename-file-and-buffer)
         ("C-S-w" . prot/copy-line)
         ("M-=" . count-words)
         ("M-\"" . prot/insert-double-quotes)
         ("C-M-\"" . prot/insert-double-smart-quotes)
         ("C-M-'" . prot/insert-single-smart-quotes)
         ("M-`" . prot/insert-elisp-quotes)
         ("C-s-k" . kill-this-buffer)
         ("M-k" . prot/kill-line-backward)
         ("C-S-n" . prot/multi-line-next)
         ("C-S-p" . prot/multi-line-prev)
         ("<C-return>" . prot/new-line-below)
         ("<C-S-return>" . prot/new-line-above)
         ("M-SPC" . cycle-spacing)
         ("M-o" . delete-blank-lines)
         ("C-t" . prot/transpose-chars)
         ("C-x C-t" . prot/transpose-or-swap-lines)
         ("C-S-t" . prot/transpose-or-swap-paragraphs)
         ("C-x M-t" . prot/transpose-or-swap-sentences)
         ("M-t" . prot/transpose-or-swap-words)
         ("M-Q" . prot/unfill-region-or-paragraph)
         ("C-S-y" . prot/yank-replace-line-or-region)))

(use-package beginend
  :ensure
  :demand
  :diminish beginend-global-mode
  :config
  (dolist (mode beginend-modes) (diminish (cdr mode)))
  (beginend-global-mode 1))

(use-package elfeed
  :ensure
  :config
  (setq elfeed-use-curl t)
  (setq elfeed-curl-max-connections 10)
  (setq elfeed-db-directory "~/.emacs.d/elfeed/")
  (setq elfeed-enclosure-default-dir "~/Downloads/")
  (setq elfeed-search-filter "@4-months-ago +unread")
  (setq elfeed-sort-order 'descending)
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
        (split-window win (/ (frame-height) 3) 'below)
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

  ;; TODO make this buffer more useful, such as running it in a
  ;; shell-aware mode.
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
           (video (format "--ytdl-format=[height<=?%s]" height))
           (buf (pop-to-buffer "*elfeed-mpv-output*")))
      (run-hooks 'prot/elfeed-mpv-hook)
      (if enclosure              ; make this its own parametrised function
          (progn
            (start-process "audio-mpv" buf "mpv" audio enclosure)
            (message (concat "Launching MPV for " (propertize enclosure 'face 'success))))
        (start-process "video-mpv" buf "mpv" video link)
        (message (concat "Launching MPV for " (propertize link 'face 'success))))))

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
          (prot/pulse-line 'modus-theme-subtle-cyan)))))

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
         :map elfeed-show-mode-map
         ;; TODO any way to do this without lambda?
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

(use-package shr
  :config
  (setq shr-use-fonts nil)
  (setq shr-use-colors nil)
  (setq shr-max-image-proportion 0.7)
  (setq shr-image-animate nil)
  (setq shr-width (current-fill-column)))

;; Support the HTML pre tag with proper syntax highlighting.
(use-package shr-tag-pre-highlight
  :ensure
  :after shr
  :config
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight))
  (when (version< emacs-version "26")
    (with-eval-after-load 'eww
      (advice-add 'eww-display-html :around
                  'eww-display-html--override-shr-external-rendering-functions))))

(use-package eww
  :commands (eww
             eww-browse-url
             eww-search-words
             eww-open-in-new-buffer
             eww-open-file
             prot/eww-visit-history)
  :config
  (setq eww-restore-desktop t)
  (setq eww-header-line-format "%u")

  (defun prot/eww-visit-history ()
    "Revisit `eww' history using completion."
    (interactive)
    (let ((history eww-prompt-history))
      (icomplete-vertical-do ()
        (eww
         (completing-read "Visit website from history: " history nil t)))))

  (defvar prot/eww-global-map
    (let ((map (make-sparse-keymap)))
      (define-key map "s" 'eww-search-words)
      (define-key map "o" 'eww-open-in-new-buffer)
      (define-key map "f" 'eww-open-file)
      (define-key map "w" 'prot/eww-visit-history)
      map)
    "Key map to scope `eww' bindings for global usage.
The idea is to bind this to a prefix key, so that its defined
keys follow the pattern of <PREFIX> <KEY>.")
  :bind-keymap ("C-c w" . prot/eww-global-map))

;; ............................................ Emacs server and desktop

(use-package server
  :hook (after-init-hook . server-start))

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

(setq initial-buffer-choice t)			; always start with *scratch*

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

(internal-set-lisp-face-attribute 'default :family "Hack" 0)
(internal-set-lisp-face-attribute 'default :weight 'normal 0)
(internal-set-lisp-face-attribute 'default :height 90 0)
(internal-set-lisp-face-attribute 'fixed-pitch :family "Hack" 0)
(internal-set-lisp-face-attribute 'fixed-pitch :weight 'normal 0)
(internal-set-lisp-face-attribute 'variable-pitch :family "Noto Serif" 0)
(internal-set-lisp-face-attribute 'variable-pitch :weight 'normal 0)
(internal-set-lisp-face-attribute 'variable-pitch :height 1.1 0)
(set-face-attribute 'bold nil :weight 'bold)
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
(setq completion-show-help nil)
(setq completions-format 'one-column)
(setq completions-detailed t)
(setq enable-recursive-minibuffers t)
(setq resize-mini-windows t)
(setq minibuffer-eldef-shorten-default t)

;; Grouping of completions for Emacs 28
(setq completions-group t)
(setq completions-group-sort nil)
(setq completions-group-format
      (concat
       (propertize "    " 'face 'completions-group-separator)
       (propertize " %s " 'face 'completions-group-title)
       (propertize " " 'face 'completions-group-separator
                   'display '(space :align-to right))))

(file-name-shadow-mode 1)
(minibuffer-depth-indicate-mode 1)
(minibuffer-electric-default-mode 1)

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
        '( vc-retrieve-tag embark-prefix-help-command org-capture
           prot-bongo-playlist-insert-playlist-file))

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
(add-hook 'completion-list-mode-hook #'prot-minibuffer-completions-cursor)
(add-hook 'completion-list-mode-hook #'prot-minibuffer-hl-line)
(add-hook 'completion-list-mode-hook #'prot-minibuffer-display-line-numbers)

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

;; Enables previews inside the standard *Completions* buffer.
(add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)

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
  (define-key map (kbd "C-x r r") #'consult-register)
  (define-key map (kbd "M-s M-i") #'consult-imenu)
  (define-key map (kbd "M-s M-s") #'consult-outline)
  (define-key map (kbd "M-s M-y") #'consult-yank)
  (define-key map (kbd "M-s M-l") #'consult-line))
(define-key consult-narrow-map (kbd "?") #'consult-narrow-help)

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

(setq embark-collect-initial-view-alist
      '((file . list)
        (buffer . list)
        (symbol . list)
        (line . list)
        (xref-location . list)
        (kill-ring . zebra)
        (t . list)))
(setq embark-collect-live-update-delay 0.5)
(setq embark-collect-live-initial-delay 0.8)

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

;;;; Applications and Utilities

;;;;; Bookmarking

(setq bookmark-use-annotations nil)
(setq bookmark-automatically-show-annotations t)
(setq bookmark-fontify nil)

(add-hook 'bookmark-bmenu-mode-hook #'hl-line-mode)

(require 'prot-bookmark)
(prot-bookmark-extra-keywords 1)

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
        (:name "reference" :query "tag:reference not tag:archived" :key "r")
        (:name "todo" :query "tag:todo not tag:archived" :key "t")
        (:name "mailing lists" :query "tag:list" :key "m")
        ;; Emacs
        (:name "emacs-devel" :query "from:emacs-devel@gnu.org or to:emacs-devel@gnu.org" :key "ed")
        (:name "emacs-humanities" :query "from:emacs-humanities@gnu.org or to:emacs-humanities@gnu.org" :key "eh")
        ;; CLI tools
        (:name "notmuch" :query "from:notmuch@notmuchmail.org or to:notmuch@notmuchmail.org" :key "cn")
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
(setq elfeed-sort-order 'ascending)
(setq elfeed-search-title-max-width 80)
(setq elfeed-search-title-min-width 30)
(setq elfeed-search-trailing-width 25)
(setq elfeed-show-truncate-long-urls t)

(add-hook 'elfeed-search-mode-hook
	  (lambda ()
	    (load-file (concat user-emacs-directory "feeds.el.gpg"))))

(add-hook 'elfeed-show-mode-hook
          (lambda ()
	    (setq-local shr-width (current-fill-column))))

(define-key global-map (kbd "C-c e") 'elfeed)

(provide 'init)

;;;;; HTML Rendering and EWW

(setq browse-url-browser-function 'eww-browse-url)
(setq browse-url-secondary-browser-function 'browse-url-default-browser)

(setq shr-cookie-policy nil)
(setq shr-max-image-proportion 0.6)

(setq url-cookie-untrusted-urls '(".*"))

(require 'eww)
(setq eww-restore-desktop t)
(setq eww-search-prefix "https://duckduckgo.com/html/?q=")
(setq eww-download-directory (expand-file-name "~/Downloads"))
(setq eww-bookmarks-directory (locate-user-emacs-file "eww-bookmarks/"))
(setq eww-history-limit 150)
(setq eww-browse-url-new-window-is-tab nil)

(defun dp-eww-readable ()
  "Use more opinionated `eww-readable'.

Set width is set to `current-fill-column'.  Adjust size of
images."
  (interactive)
  (let ((shr-width (current-fill-column))
        (shr-max-image-proportion 0.35))
    (eww-readable)))

(define-key eww-link-keymap (kbd "v") nil) ; stop overriding `eww-view-source'
(define-key eww-mode-map (kbd "L") #'eww-list-bookmarks)
(define-key eww-mode-map (kbd "R") #'dp-eww-readable)
(define-key dired-mode-map (kbd "E") #'eww-open-file) ; to render local HTML files
(define-key eww-buffers-mode-map (kbd "d") #'eww-bookmark-kill) ; it actually deletes
(define-key eww-bookmark-mode-map (kbd "d") #'eww-bookmark-kill) ; same

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

;;;;;; Shell Scripts

;;;;; Paragraphs

(setq-default fill-column 72)
(add-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'prog-mode-hook (lambda ()
			    (setq-local fill-column 80)))

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

(setq-default tab-always-indent 'complete)
(setq-default tab-first-completion 'word-or-paren-or-punct)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;;;;; Spellcheck

(setq ispell-dictionary "en_GB-ise-w_accents")

(setq dp-spell-dictionaries
      '("en_GB-ise-w_accents" "fr_FR-lrg" "nl" "es"))

(defvar dp-spell--dictionary-hist '()
  "Input history for `dp-spell-change-dictionary'.")

(defun dp-spell--dictionary-prompt ()
  "Helper prompt to select from `dp-spell-dictionaries'."
  (let ((def (car dp-spell--dictionary-hist)))
    (completing-read
     (format "Select dictionary [%s]: " def)
     dp-spell-dictionaries
     nil t nil 'dp-spell--dictionary-hist def)))

(defun dp-spell-change-dictionary (dictionary)
  "Select a DICTIONARY from `dp-spell-dictionaries'."
  (interactive
   (list (dp-spell--dictionary-prompt)))
  (ispell-change-dictionary dictionary))

(define-key global-map (kbd "C-M-$") #'dp-spell-change-dictionary)

;;;;;; Aode and Text Linters

;;;;;;; Flymake

(require 'flymake)
(setq flymake-suppress-zero-counters t)
(setq flymake-no-changes-timeout nil)
(let ((map flymake-mode-map))
  (define-key map (kbd "C-c ! s") #'flymake-start)
  (define-key map (kbd "C-c ! d") #'flymake-show-diagnostics-buffer)
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

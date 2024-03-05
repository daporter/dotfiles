;;; my-iceberg-theme.el --- My implementation of Google's Material 3 theme  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  David Porter

;; Author: David Porter <david@daporter.net>
;; Keywords: faces

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(deftheme my-iceberg
  "My version of the Iceberg theme."
  :background-mode 'dark)

;; To make picking colours easier, use a website like https://hslpicker.com/ .

(let* ((bg-1 "#040406")                 ; l; 2%
       (bg   "#0A0B0F")                 ; hsl 230 20% 5%
       (bg+1 "#14161F")                 ; l: 10%
       (bg+2 "#1F212E")                 ; l: 15%
       (bg+3 "#33374D")                 ; l: 25%

       (fg-2 "#70758f")                 ; l: 50%
       (fg-1 "#aaadbb")                 ; l: 70%
       (fg   "#e2e3e8")                 ; hsl 230 11% 90%
       (fg+1 "#f1f1f4")                 ; l: 95%

       (red        "#edabab")           ; hsl: 0   65 80
       (orange     "#edc6ab")           ; hsl: 25  65 80
       (yellow     "#e6ddb3")           ; hsl: 50  50 80
       (green      "#dde6b3")           ; hsl: 80  50 80
       (cyan       "#b3dee6")           ; hsl: 190 50 80
       (blue       "#b3c8e6")           ; hsl: 215 50 80
       (magenta    "#bfb3e6")           ; hsl: 255 50 80
       (red-bg-1   (color-darken-name  red     80))
       (red-bg     (color-darken-name  red     70))
       (red-bg+1   (color-darken-name  red     55))
       (red+1      (color-lighten-name red     25))
       (orange-bg  (color-darken-name  orange  70))
       (orange+1   (color-darken-name  orange  25))
       (yellow-bg  (color-darken-name  yellow  70))
       (yellow+1   (color-lighten-name yellow  25))
       (green-bg-1 (color-darken-name  green   80))
       (green-bg   (color-darken-name  green   70))
       (green-bg+1 (color-darken-name  green   55))
       (green+1    (color-darken-name  green   25))
       (cyan-bg    (color-darken-name  cyan    70))
       (cyan+1     (color-lighten-name cyan    25))
       (blue-bg    (color-darken-name  blue    70))
       (blue+1     (color-darken-name  blue    25))
       (magenta-bg (color-darken-name  magenta 70))
       (magenta+1  (color-darken-name  magenta 25))

       (black "black")
       (white "white"))

  (custom-theme-set-faces
   'my-iceberg

   `(border                ((t (:foreground ,bg+2 :background ,bg-1))))
   `(cursor                ((t (:background ,white))))
   `(default               ((t (:foreground ,fg :background ,bg))))
   `(error                 ((t (:foreground ,red))))
   `(fringe                ((t (:foreground ,fg-2))))
   `(header-line           ((t (:background ,bg-1 :weight bold))))
   `(header-line-highlight ((t (:background ,bg+2))))
   `(link                  ((t (:foreground ,blue :underline t))))
   `(link-visited          ((t (:foreground ,magenta :underline t))))
   `(match                 ((t (:background ,bg+3))))
   `(minibuffer-prompt     ((t (:foreground ,cyan))))
   `(mode-line             ((t (:family "Roboto"   :foreground ,fg-1 :background ,bg-1 :box (:color ,bg+2)))))
   `(mode-line-inactive    ((t (:inherit mode-line :foreground ,bg+3 :background ,bg   :box (:color ,bg+2)))))
   `(region                ((t (:background ,bg+2))))
   `(secondary-selection   ((t (:background ,bg+3))))
   `(shadow                ((t (:foreground ,fg-2))))
   `(success               ((t (:foreground ,green))))
   `(tooltip               ((t (:background ,bg+2 :foreground ,white))))
   `(trailing-whitespace   ((t (:foreground ,black :background ,red))))
   `(vertical-border       ((t (:foreground ,bg+3))))
   `(warning               ((t (:foreground ,orange))))
   `(widget-button         ((t (:foreground ,fg-1 :background ,bg+1 :box (:color ,bg+2)))))
   `(widget-field          ((t (:foreground ,fg-1 :background ,bg+1 :box (:line-width (-1 . -1) :color ,bg+2)))))

   `(fill-column-indicator ((t (:foreground ,bg+1))))

   `(font-lock-builtin-face       ((t (:foreground ,cyan))))
   `(font-lock-comment-face       ((t (:inherit shadow :slant italic))))
   `(font-lock-constant-face      ((t (:foreground ,magenta :weight semi-bold))))
   `(font-lock-escape-face        ((t (:foreground ,magenta))))
   `(font-lock-function-name-face ((t (:inherit bold))))
   `(font-lock-function-call-face ((t (:inherit default))))
   `(font-lock-keyword-face       ((t (:foreground ,blue ))))
   `(font-lock-negation-char-face ((t (:foreground ,orange))))
   `(font-lock-preprocessor-face  ((t (:foreground ,orange))))
   `(font-lock-punctuation-face   ((t (:foreground ,fg-1))))
   `(font-lock-string-face        ((t (:foreground ,yellow))))
   `(font-lock-type-face          ((t (:foreground ,cyan :slant italic))))
   `(font-lock-variable-name-face ((t (:inherit default))))
   '(font-lock-warning-face       ((t (:inherit error))))

   ;; Avy
   `(avy-lead-face   ((t (:foreground ,bg-1 :background ,red))))
   `(avy-lead-face-0 ((t (:inherit avy-lead-face))))
   `(avy-lead-face-1 ((t (:inherit avy-lead-face))))
   `(avy-lead-face-2 ((t (:inherit avy-lead-face))))

   ;; Compilation
   `(compilation-info           ((t (:foreground ,green))))
   `(compilation-warning        ((t (:foreground ,yellow :bold t))))
   `(compilation-error          ((t (:foreground ,red))))
   `(compilation-mode-line-fail ((t (:foreground ,red :weight bold))))
   `(compilation-mode-line-exit ((t (:foreground ,green :weight bold))))

   ;; Completion
   '(completions-annotations ((t (:inherit shadow))))

   ;; Corfu
   `(corfu-default    ((t (:foreg ,fg :background ,bg+1))))
   `(corfu-current    ((t (:background ,bg+2))))
   '(corfu-deprecated ((t (:inherit font-lock-comment-face :strike-through t))))
   `(corfu-border     ((t (:background ,bg+3))))
   `(corfu-bar        ((t (:background ,fg-2))))

   ;; Custom
   `(custom-button       ((t (:inherit widget-button :box (:style released-button)))))
   `(custom-button-mouse ((t (:inherit custom-button))))
   `(custom-state        ((t (:foreground ,green))))

   ;; Diff
   `(diff-added             ((t (:background ,green-bg))))
   `(diff-file-header       ((t (:inherit (diff-header bold)))))
   `(diff-header            ((t (:foreground ,cyan :background ,bg+1))))
   `(diff-indicator-added   ((t (:inherit diff-added :foreground ,fg-1))))
   `(diff-indicator-removed ((t (:inherit diff-removed :foreground ,fg-1))))
   `(diff-refine-added      ((t (:background ,green-bg+1))))
   `(diff-refine-removed    ((t (:background ,red-bg+1))))
   `(diff-removed           ((t (:background ,red-bg))))

   ;; Dired
   `(dired-broken-symlink ((t (:background ,red))))
   `(dired-directory      ((t (:inherit bold :foreground ,blue))))
   `(dired-flagged        ((t (:inherit bold :foreground ,red))))
   `(dired-header         ((t (:inherit bold :foreground ,cyan))))
   `(dired-ignored        ((t (:inherit shadow))))
   `(dired-mark           ((t (:inherit bold))))
   `(dired-marked         ((t (:inherit bold :foreground ,green))))
   `(dired-perm-write     ((t (:foreground ,green))))
   `(dired-special        ((t (:foreground ,magenta))))
   `(dired-symlink        ((t (:foreground ,yellow))))
   `(dired-warning        ((t (:inherit warning))))

   ;; Eglot
   `(eglot-mode-line             ((t (:foreground ,magenta))))
   `(eglot-highlight-symbol-face ((t (:background ,magenta-bg :foreground ,fg+1))))

   ;; EShell
   `(eshell-ls-archive    ((t (:foreground ,cyan))))
   `(eshell-ls-backup     ((t (:inherit shadow))))
   `(eshell-ls-clutter    ((t (:foreground ,red :weight bold))))
   `(eshell-ls-directory  ((t (:foreground ,blue :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,green :weight bold))))
   `(eshell-ls-missing    ((t (:foreground ,red :weight bold))))
   `(eshell-ls-product    ((t (:inherit shadow))))
   `(eshell-ls-readonly   ((t (:foreground ,orange))))
   `(eshell-ls-special    ((t (:foreground ,magenta))))
   `(eshell-ls-symlink    ((t (:foreground ,yellow))))
   `(eshell-ls-unreadable ((t (:inherit error))))
   `(eshell-prompt        ((t (:foreground ,blue :weight bold))))

   ;; Flymake
   `(flymake-errline  ((t (:underline (:style wave :color ,red)))))
   `(flymake-warnline ((t (:underline (:style wave :color ,yellow)))))
   `(flymake-infoline ((t (:underline (:style wave :color ,green)))))

   ;; Flyspell
   `(flyspell-incorrect ((t (:underline (:style wave :color ,red)))))
   `(flyspell-duplicate ((t (:underline (:style wave :color ,yellow)))))

   ;; Git
   `(git-commit-summary                    ((t (:inherit default))))
   `(magit-blame-heading                   ((t (:foreground ,fg :background ,bg+1))))
   `(magit-branch-local                    ((t (:foreground ,green))))
   `(magit-branch-remote                   ((t (:foreground ,blue))))
   `(magit-diff-added                      ((t (:foreground ,fg-1 :background ,green-bg-1))))
   `(magit-diff-added-highlight            ((t (:background ,green-bg))))
   `(magit-diff-context                    ((t (:foreground ,fg-1 :background ,bg+1))))
   `(magit-diff-context-highlight          ((t (:background ,bg+2))))
   `(magit-diff-file-heading               ((t (:weight semi-bold))))
   `(magit-diff-hunk-heading               ((t (:foreground ,fg-2 :background ,bg+1))))
   `(magit-diff-hunk-heading-highlight     ((t (:foreground ,fg-1 :background ,bg+3))))
   `(magit-diff-removed                    ((t (:foreground ,fg-1 :background ,red-bg-1))))
   `(magit-diff-removed-highlight          ((t (:background ,red-bg))))
   `(magit-diff-revision-summary           ((t (:foreground ,blue :weight bold))))
   `(magit-diff-revision-summary-highlight ((t (:foreground ,blue :weight bold))))
   `(magit-item-highlight                  ((t (:background ,bg+1))))
   `(magit-log-author                      ((t (:inherit message-header-to))))
   `(magit-log-date                        ((t (:foreground ,cyan))))
   `(magit-log-head-label-head             ((t (:foreground ,fg :background ,bg+1))))
   `(magit-log-head-label-local            ((t (:foreground ,blue :background bg+1))))
   `(magit-log-head-label-remote           ((t (:foreground ,green :background ,bg+1))))
   `(magit-log-head-label-tags             ((t (:foreground ,yellow :background ,bg+1))))
   `(magit-log-sha1                        ((t (:foreground ,red))))
   `(magit-section-heading                 ((t (:foreground ,cyan :weight semi-bold))))
   `(magit-section-highlight               ((t (:background ,bg+1))))
   `(magit-tag                             ((t (:foreground ,yellow :background ,bg))))

   ;; Help
   `(help-key-binding ((t (:background ,bg+1 :weight light :box (:line-width (-1 . -1) :color ,bg+2)))))

   ;; Line Highlighting
   `(highlight ((t (:background ,bg+2))))
   `(hl-line   ((t (:background ,bg-1))))

   ;; Line Numbers
   `(line-number              ((t (:inherit fixed-pitch :foreground ,fg-2))))
   `(line-number-current-line ((t (:inherit line-number :foreground ,yellow))))

   ;; Linum
   `(linum ((t (:foreground ,blue-bg))))

   ;; Marginalia
   `(marginalia-date            ((t (:foreground ,cyan))))
   `(marginalia-size            ((t (:foreground ,magenta))))
   `(marginalia-file-priv-dir   ((t (:inherit eshell-ls-directory))))
   `(marginalia-file-priv-exec  ((t (:foreground ,red))))
   `(marginalia-file-priv-key   ((t (:inherit help-key-binding))))
   `(marginalia-file-priv-link  ((t (:inherit eshell-ls-symlink))))
   `(marginalia-file-priv-owner ((t (:inherit shadow))))
   `(marginalia-file-priv-read  ((t (:foreground ,green))))
   `(marginalia-file-priv-write ((t (:foreground ,orange))))

   ;; Markdown
   `(markdown-blockquote-face         ((t (:inherit org-quote))))
   `(markdown-code-face               ((t (:inherit org-code))))
   `(markdown-header-face             ((t (:inherit bold))))
   `(markdown-header-face-1           ((t (:inherit org-level-1))))
   `(markdown-header-face-2           ((t (:inherit org-level-2))))
   `(markdown-header-face-3           ((t (:inherit org-level-3))))
   `(markdown-header-face-4           ((t (:inherit org-level-4))))
   `(markdown-header-face-5           ((t (:inherit org-level-5))))
   `(markdown-header-face-6           ((t (:inherit org-level-6))))
   `(markdown-html-attr-name-face     ((t (:inherit fixed-pitch))))
   `(markdown-html-attr-value-face    ((t (:inherit fixed-pitch))))
   `(markdown-html-entity-face        ((t (:inherit fixed-pitch))))
   `(markdown-html-tag-delimiter-face ((t (:inherit fixed-pitch))))
   `(markdown-html-tag-name-face      ((t (:inherit fixed-pitch))))
   `(markdown-inline-code-face        ((t (:inherit markdown-code-face))))
   `(markdown-language-info-face      ((t (:inherit fixed-pitch))))
   `(markdown-language-keyword-face   ((t (:inherit fixed-pitch))))
   `(markdown-markup-face             ((t (:inherit (fixed-pitch shadow)))))
   `(markdown-metadata-key-face       ((t (:inherit org-document-info-keyword))))
   `(markdown-metadata-value-face     ((t (:inherit markdown-metadata-key-face))))
   `(markdown-pre-face                ((t (:inherit org-block))))
   `(markdown-table-face              ((t (:inherit fixed-pitch))))
   `(markdown-url-face                ((t (:inherit (fixed-pitch shadow)))))

   ;; Message
   `(message-header-cc         ((t (:foreground ,green+1))))
   `(message-header-name       ((t (:inherit bold))))
   `(message-header-newsgroups ((t (:inherit message-header-other))))
   `(message-header-other      ((t (:foreground ,magenta+1))))
   `(message-header-subject    ((t (:foreground ,magenta))))
   `(message-header-to         ((t (:foreground ,green))))
   `(message-header-xheader    ((t (:inherit message-header-other))))
   `(message-mml               ((t (:inherit widget-button :foreground ,fg-2))))

   ;; Notmuch
   `(notmuch-crypto-decryption            ((t (:inherit bold))))
   `(notmuch-crypto-part-header           ((t (:foreground ,green-bg))))
   `(notmuch-crypto-signature-bad         ((t (:inherit error))))
   `(notmuch-crypto-signature-good        ((t (:inherit success))))
   `(notmuch-crypto-signature-good-key    ((t (:inherit success))))
   `(notmuch-crypto-signature-unknown     ((t (:inherit warning))))
   `(notmuch-jump-key                     ((t (:inherit help-key-binding))))
   `(notmuch-message-summary-face         ((t (:inherit bold))))
   `(notmuch-search-count                 ((t (:foreground ,fg-2))))
   `(notmuch-search-date                  ((t (:foreground ,cyan))))
   `(notmuch-search-flagged-face          ((t (:inherit font-lock-keyword-face))))
   `(notmuch-search-matching-authors      ((t (:foreground ,green))))
   `(notmuch-search-non-matching-authors  ((t (:inherit shadow))))
   `(notmuch-search-subject               ((t (:foreground ,fg))))
   `(notmuch-search-unread-face           ((t (:inherit bold))))
   `(notmuch-tag-added                    ((t (:foreground ,cyan :underline t))))
   `(notmuch-tag-deleted                  ((t (:foreground ,red :strike-through t))))
   `(notmuch-tag-face                     ((t (:foreground ,blue))))
   `(notmuch-tag-flagged                  ((t (:inherit font-lock-keyword-face))))
   `(notmuch-tag-unread                   ((t (:foreground ,magenta))))
   `(notmuch-tree-match-author-face       ((t (:inherit notmuch-search-matching-authors))))
   `(notmuch-tree-match-date-face         ((t (:inherit notmuch-search-date))))
   `(notmuch-tree-match-face              ((t (:foreground ,fg))))
   `(notmuch-tree-match-tag-face          ((t (:inherit notmuch-tag-face))))
   `(notmuch-tree-no-match-date-face      ((t (:inherit shadow))))
   `(notmuch-tree-no-match-face           ((t (:inherit shadow))))
   `(notmuch-wash-cited-text              ((t (:inherit message-cited-text-1))))
   `(notmuch-wash-toggle-button           ((t (:inherit widget-n :foreground ,fg-2))))

   ;; Orderless
   `(orderless-match-face-0 ((t (:inherit bold :foreground  ,cyan))))
   `(orderless-match-face-1 ((t (:inherit bold :foreground  ,green))))
   `(orderless-match-face-2 ((t (:inherit bold :foreground  ,blue))))
   `(orderless-match-face-3 ((t (:inherit bold :foreground  ,magenta))))

   ;; Org Mode
   `(org-agenda-date               ((t (:foreground ,fg-2))))
   `(org-agenda-date-today         ((t (:inherit bold :foreground ,fg-2))))
   `(org-agenda-date-weekend       ((t (:inherit org-agenda-date))))
   `(org-agenda-date-weekend-today ((t (:inherit (bold org-agenda-date)))))
   `(org-agenda-dimmed-todo-face   ((t (:inherit font-lock-comment-face))))
   `(org-agenda-done               ((t (:foreground ,green))))
   `(org-agenda-structure          ((t (:foreground ,fg-2))))
   `(org-block                     ((t (:inherit fixed-pitch :background ,bg+1))))
   `(org-block-begin-line          ((t (:inherit (fixed-pitch org-meta-line) :height 0.9))))
   `(org-block-end-line            ((t (:inherit org-block-begin-line))))
   `(org-code                      ((t (:inherit fixed-pitch :foreground ,magenta :background ,bg+1))))
   `(org-column                    ((t (:background ,bg+1))))
   `(org-column-title              ((t (:inhenrit (org-column bold) :underline t))))
   `(org-date                      ((t (:inherit (org-agenda-date bold) :underline t))))
   `(org-document-info             ((t (:foreground ,cyan))))
   `(org-document-info-keyword     ((t (:inherit (fixed-pitch font-lock-comment-face) :height 0.9))))
   `(org-document-title            ((t (:inherit bold :foreground ,yellow :height 1.44))))
   `(org-done                      ((t (:inherit (fixed-pitch shadow)))))
   `(org-drawer                    ((t (:inherit org-meta-line))))
   `(org-ellipsis                  ((t (:inherit shadow))))
   `(org-footnote                  ((t (:foreground ,magenta))))
   `(org-formula                   ((t (:foreground ,yellow))))
   `(org-headline-done             ((t (:inherit shadow))))
   `(org-hide                      ((t (:inherit fixed-pitch :foreground ,bg))))
   `(org-indent                    ((t (:inherit (fixed-pitch org-hide)))))
   `(org-level-1                   ((t (:inherit bold :foreground ,red :height 1.3))))
   `(org-level-2                   ((t (:inherit bold :foreground ,orange :height 1.2))))
   `(org-level-3                   ((t (:inherit bold :foreground ,yellow :height 1.1))))
   `(org-level-4                   ((t (:inherit (bold italic) :foreground ,green))))
   `(org-level-5                   ((t (:inherit (bold italic) :foreground ,cyan))))
   `(org-level-6                   ((t (:inherit italic :foreground ,magenta))))
   `(org-level-7                   ((t (:inherit italic :foreground ,blue))))
   `(org-level-8                   ((t (:inherit italic :foreground ,fg))))
   `(org-link                      ((t (:inherit link))))
   `(org-meta-line                 ((t (:inherit (fixed-pitch shadow) :height 0.9))))
   `(org-modern-date-active        ((t (:inherit org-modern-label :foreground ,fg-1 :background ,bg+2))))
   `(org-modern-date-inactive      ((t (:inherit org-modern-label :foreground ,fg-2 :background ,bg+1))))
   `(org-modern-done               ((t (:inherit org-modern-label :foreground ,fg-2 :background ,bg+2))))
   `(org-modern-time-inactive      ((t (:inherit org-modern-date-inactive))))
   `(org-modern-todo               ((t (:inherit org-modern-label :foreground ,fg+1 :background ,orange-bg))))
   `(org-priority                  ((t (:foreground ,yellow))))
   `(org-quote                     ((t (:background ,bg+1))))
   `(org-scheduled                 ((t (:foreground ,green))))
   `(org-scheduled-previously      ((t (:foreground ,cyan))))
   `(org-scheduled-today           ((t (:inherit bold :foreground ,green))))
   `(org-sexp-date                 ((t (:foreground ,fg-2))))
   `(org-special-keyword           ((t (:inherit org-drawer))))
   `(org-table                     ((t (:inherit fixed-pitch :foreground ,bg+3))))
   `(org-tag                       ((t (:inherit bold :foreground ,magenta))))
   `(org-todo                      ((t (:inherit fixed-pitch :foreground ,orange))))
   `(org-upcoming-deadline         ((t (:foreground ,red))))
   `(org-verbatim                  ((t (:inherit org-code))))
   `(org-warning                   ((t (:inherit warning))))

     ;; Outline
   `(outline-1 ((t (:inherit org-level-1))))
   `(outline-2 ((t (:inherit org-level-2))))
   `(outline-3 ((t (:inherit org-level-3))))
   `(outline-4 ((t (:inherit org-level-4))))
   `(outline-5 ((t (:inherit org-level-5))))
   `(outline-6 ((t (:inherit org-level-6))))
   `(outline-7 ((t (:inherit org-level-7))))
   `(outline-8 ((t (:inherit org-level-8))))

   ;; Search
   `(isearch         ((t (:foreground ,bg-1 :background ,yellow))))
   `(isearch-group-1 ((t (:foreground ,bg-1 :background ,magenta))))
   `(isearch-group-2 ((t (:foreground ,bg-1 :background ,cyan))))
   `(isearch-fail    ((t (:foreground ,bg-1 :background ,red))))
   `(lazy-highlight  ((t (:foreground ,fg+1 :background ,yellow-bg))))

   ;; Sh
   `(sh-quoted-exec ((t (:foreground ,red))))

   ;; Show Paren
   `(show-paren-match    ((t (:box (:line-width (-1 . -1) :color ,fg-2)))))
   `(show-paren-mismatch ((t (:background ,red-bg))))

   ;; Terminal
   `(ansi-color-black          ((t (:foreground ,black))))
   `(ansi-color-red            ((t (:foreground ,red))))
   `(ansi-color-green          ((t (:foreground ,green))))
   `(ansi-color-blue           ((t (:foreground ,blue))))
   `(ansi-color-yellow         ((t (:foreground ,yellow))))
   `(ansi-color-magenta        ((t (:foreground ,magenta))))
   `(ansi-color-cyan           ((t (:foreground ,cyan))))
   `(ansi-color-white          ((t (:foreground ,fg+1))))
   `(ansi-color-bright-black   ((t (:foreground ,bg-1))))
   `(ansi-color-bright-red     ((t (:foreground ,red+1))))
   `(ansi-color-bright-green   ((t (:foreground ,green+1))))
   `(ansi-color-bright-blue    ((t (:foreground ,blue+1))))
   `(ansi-color-bright-yellow  ((t (:foreground ,yellow+1))))
   `(ansi-color-bright-magenta ((t (:foreground ,magenta+1))))
   `(ansi-color-bright-cyan    ((t (:foreground ,cyan+1))))
   `(ansi-color-bright-white   ((t (:foreground ,white))))

   ;; Transient
   `(transient-key ((t (:inherit help-key-binding))))
   `(transient-heading ((t (:inherit bold :foreground ,yellow))))
   `(transient-key-stay ((t (:inherit transient-key :foreground ,green))))
   `(transient-key-exit ((t (:inherit transient-key :foreground ,orange))))
   `(transient-key-return ((t (:inherit transient-key :foreground ,yellow))))

   ;; Which Function
   `(which-func ((t (:foreground ,magenta))))

   ;; Which-Key
   `(which-key-command-description-face   ((t (:inherit default))))
   `(which-key-key-face                   ((t (:inherit help-key-binding))))
   `(which-key-local-map-description-face ((t (:foreground ,green))))

   ;; Whitespace
   `(trailing-whitespace         ((t (:foreground ,magenta :background ,magenta))))
   `(whitespace-big-indent       ((t (:foreground ,yellow))))
   `(whitespace-empty            ((t (:inherit warning))))
   `(whitespace-hspace           ((t (:inherit default))))
   `(whitespace-indentation      ((t (:foreground ,bg+1))))
   `(whitespace-line             ((t (:underline (:style wave :color ,magenta)))))
   `(whitespace-newline          ((t (:inherit whitespace-space))))
   `(whitespace-space            ((t (:inherit fixed-pitch :foreground ,bg+3))))
   `(whitespace-space-after-tab  ((t (:inherit warning))))
   `(whitespace-space-before-tab ((t (:inherit warning))))
   `(whitespace-tab              ((t (:inherit whitespace-space))))
   `(whitespace-trailing         ((t (:inherit trailing-whitespace))))

   ))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'my-iceberg)
;;; my-iceberg-theme.el ends here

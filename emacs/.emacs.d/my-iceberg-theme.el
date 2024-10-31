;;; my-iceberg-theme.el --- My implementation of the Iceberg theme  -*- lexical-binding: t; -*-

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

(require 'color)

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

       (red        "#e48181")           ; hsl: 0   65 70
       (orange     "#e4aa81")           ; hsl: 25  65 70
       (yellow     "#e4d281")           ; hsl: 50  65 70
       (green      "#cbd98c")           ; hsl: 80  50 70
       (cyan       "#8ccdd9")           ; hsl: 190 50 70
       (blue       "#8cacd9")           ; hsl: 215 50 70
       (magenta    "#9e8cd9")           ; hsl: 255 50 70

       (factor-bg-1 80)
       (factor-bg   75)
       (factor-bg+1 60)
       (factor-fg+1 25)

       (red-bg-1     (color-darken-name  red     factor-bg-1))
       (red-bg       (color-darken-name  red     factor-bg))
       (red-bg+1     (color-darken-name  red     factor-bg+1))
       (red-fg+1     (color-lighten-name red     factor-fg+1))
       (orange-bg    (color-darken-name  orange  factor-bg))
       (yellow-bg    (color-darken-name  yellow  factor-bg))
       (yellow-bg+1  (color-darken-name  yellow  factor-bg+1))
       (yellow-fg+1  (color-lighten-name yellow  factor-fg+1))
       (green-bg-1   (color-darken-name  green   factor-bg-1))
       (green-bg     (color-darken-name  green   factor-bg))
       (green-bg+1   (color-darken-name  green   factor-bg+1))
       (green-fg+1   (color-darken-name  green   factor-fg+1))
       (cyan-bg      (color-darken-name  cyan    factor-bg))
       (cyan-fg+1    (color-lighten-name cyan    factor-fg+1))
       (blue-fg+1    (color-darken-name  blue    factor-fg+1))
       (magenta-bg   (color-darken-name  magenta factor-bg))
       (magenta-fg+1 (color-darken-name  magenta factor-fg+1))

       (black "black")
       (white "white"))

  (custom-theme-set-faces
   'my-iceberg

;;;; Built-in Packages

;;;;; Base

   `(border                ((t (:foreground ,bg+2 :background ,bg-1))))
   `(cursor                ((t (:background ,orange))))
   `(default               ((t (:foreground ,fg :background ,bg))))
   `(error                 ((t (:foreground ,red))))
   `(escape-glyph          ((t (:foreground ,yellow))))
   `(homoglyph             ((t (:foreground ,orange))))
   `(fringe                ((t (:foreground ,fg-2))))
   `(header-line           ((t (:background ,bg-1 :weight bold))))
   `(header-line-highlight ((t (:background ,bg+2))))
   `(highlight             ((t (:background ,bg+2))))
   `(link                  ((t (:foreground ,blue :underline t))))
   `(link-visited          ((t (:foreground ,magenta :underline t))))
   `(match                 ((t (:background ,bg+3))))
   `(minibuffer-prompt     ((t (:foreground ,cyan))))
   `(mode-line             ((t (:family "Roboto"   :foreground ,fg-1 :background ,bg+2 :box (:color ,bg+3)))))
   `(mode-line-inactive    ((t (:inherit mode-line :foreground ,bg+3 :background ,bg+1 :box (:color ,bg+3)))))
   `(nobreak-hyphen        ((t (:inherit default))))
   `(nobreak-space         ((t (:inherit default))))
   `(region                ((t (:background ,bg+3))))
   `(secondary-selection   ((t (:background ,cyan-bg))))
   `(shadow                ((t (:foreground ,fg-2))))
   `(success               ((t (:foreground ,green))))
   `(tooltip               ((t (:background ,bg+2 :foreground ,white))))
   `(trailing-whitespace   ((t (:foreground ,magenta :background ,magenta))))
   `(trailing-whitespace   ((t (:foreground ,black :background ,red))))
   `(vertical-border       ((t (:foreground ,bg+3))))
   `(warning               ((t (:foreground ,orange))))
   `(widget-button         ((t (:foreground ,fg-1 :background ,bg+1 :box (:color ,bg+2)))))
   `(widget-field          ((t (:foreground ,fg-1 :background ,bg+1 :box (:line-width (-1 . -1) :color ,bg+2)))))

;;;;; Compilation

   `(compilation-info           ((t (:foreground ,green))))
   `(compilation-warning        ((t (:foreground ,yellow :bold t))))
   `(compilation-error          ((t (:foreground ,red))))
   `(compilation-mode-line-fail ((t (:foreground ,red :weight bold))))
   `(compilation-mode-line-exit ((t (:foreground ,green :weight bold))))

;;;;; Completion

   '(completions-annotations ((t (:inherit shadow))))

;;;;; Custom

   `(custom-button       ((t (:inherit widget-button :box (:style released-button)))))
   `(custom-button-mouse ((t (:inherit custom-button))))
   `(custom-state        ((t (:foreground ,green))))

;;;;; Diff

   `(diff-added             ((t (:background ,green-bg))))
   `(diff-file-header       ((t (:inherit (diff-header bold)))))
   `(diff-header            ((t (:foreground ,cyan :background ,bg+1))))
   `(diff-indicator-added   ((t (:inherit diff-added :foreground ,fg-1))))
   `(diff-indicator-removed ((t (:inherit diff-removed :foreground ,fg-1))))
   `(diff-refine-added      ((t (:background ,green-bg+1))))
   `(diff-refine-removed    ((t (:background ,red-bg+1))))
   `(diff-removed           ((t (:background ,red-bg))))

;;;;; Dired

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

;;;;; Eglot

   `(eglot-mode-line             ((t (:foreground ,magenta))))
   `(eglot-highlight-symbol-face ((t (:background ,magenta-bg :foreground ,fg+1))))

;;;;; Ediff

   `(ediff-current-diff-Ancestor ((t (:background ,bg+2))))
   `(ediff-current-diff-A        ((t (:background ,red-bg))))
   `(ediff-current-diff-B        ((t (:background ,green-bg))))
   `(ediff-current-diff-C        ((t (:background ,yellow-bg))))
   `(ediff-fine-diff-A           ((t (:background ,red-bg+1))))
   `(ediff-fine-diff-B           ((t (:background ,green-bg+1))))
   `(ediff-fine-diff-C           ((t (:background ,yellow-bg+1))))
   `(ediff-even-diff-Ancestor    ((t (:background ,bg+2))))
   `(ediff-even-diff-A           ((t (:background ,bg+2))))
   `(ediff-even-diff-B           ((t (:background ,bg+2))))
   `(ediff-even-diff-C           ((t (:background ,bg+2))))
   `(ediff-odd-diff-Ancestor     ((t (:inherit ediff-even-diff-Ancestor))))
   `(ediff-odd-diff-A            ((t (:inherit ediff-even-diff-A))))
   `(ediff-odd-diff-B            ((t (:inherit ediff-even-diff-B))))
   `(ediff-odd-diff-C            ((t (:inherit ediff-even-diff-C))))

;;;;; Eldoc

   `(eldoc-highlight-function-argument ((t (:inherit eglot-highlight-symbol-face))))

;;;;; EShell

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

;;;;;  Fill Column Indicator

   `(fill-column-indicator ((t (:foreground ,bg+1))))

;;;;; Flymake

   `(flymake-error   ((t (:underline (:style wave :color ,red)))))
   `(flymake-note    ((t (:underline (:style wave :color ,green)))))
   `(flymake-warning ((t (:underline (:style wave :color ,yellow)))))

;;;;; Font Lock

   `(font-lock-builtin-face       ((t (:foreground ,cyan))))
   `(font-lock-comment-face       ((t (:inherit shadow :slant italic))))
   `(font-lock-constant-face      ((t (:foreground ,magenta :weight semi-bold))))
   `(font-lock-escape-face        ((t (:foreground ,magenta))))
   `(font-lock-function-name-face ((t (:inherit bold))))
   `(font-lock-function-call-face ((t (:foreground unspecified))))
   `(font-lock-operator-face      ((t (:foreground ,fg-1))))
   `(font-lock-keyword-face       ((t (:foreground ,blue ))))
   `(font-lock-negation-char-face ((t (:foreground ,orange))))
   `(font-lock-preprocessor-face  ((t (:foreground ,orange))))
   `(font-lock-punctuation-face   ((t (:foreground ,fg-1))))
   `(font-lock-string-face        ((t (:foreground ,yellow :family "Iosevka Slab"))))
   `(font-lock-type-face          ((t (:foreground ,cyan :slant italic))))
   `(font-lock-variable-name-face ((t (:foreground unspecified))))
   '(font-lock-warning-face       ((t (:inherit error))))

;;;;; Flyspell

   `(flyspell-incorrect ((t (:underline (:style wave :color ,red)))))
   `(flyspell-duplicate ((t (:underline (:style wave :color ,yellow)))))

;;;;; Help

   `(help-key-binding ((t (:foreground ,orange :background ,bg+2 :weight light :box (:line-width (-1 . -1) :color ,bg+3)))))

;;;;; HL-Line

   `(hl-line ((t (:background ,bg+3))))

;;;;; Line Numbers

   `(line-number              ((t (:inherit fixed-pitch :foreground ,bg+3))))
   `(line-number-current-line ((t (:inherit line-number :foreground ,yellow))))

;;;;; Man

   `(Man-overstrike           ((t (:inherit bold :foreground ,yellow))))
   `(Man-underline            ((t (:inherit underline :foreground ,magenta))))

;;;;; Message

   `(message-header-cc         ((t (:foreground ,green-fg+1))))
   `(message-header-name       ((t (:inherit bold))))
   `(message-header-newsgroups ((t (:inherit message-header-other))))
   `(message-header-other      ((t (:foreground ,fg-1))))
   `(message-header-subject    ((t (:inherit bold :foreground ,yellow))))
   `(message-header-to         ((t (:foreground ,blue))))
   `(message-header-xheader    ((t (:inherit message-header-other))))
   `(message-mml               ((t (:inherit widget-button :foreground ,fg-2))))
   `(message-cited-text-1      ((t (:foreground ,fg-1))))

;;;;; Org Mode

   `(org-agenda-date               ((t (:foreground ,fg-2))))
   `(org-agenda-date-today         ((t (:inherit bold :foreground ,fg-2))))
   `(org-agenda-date-weekend       ((t (:inherit org-agenda-date))))
   `(org-agenda-date-weekend-today ((t (:inherit (bold org-agenda-date)))))
   `(org-agenda-dimmed-todo-face   ((t (:inherit font-lock-comment-face))))
   `(org-agenda-done               ((t (:foreground ,green))))
   `(org-agenda-structure          ((t (:foreground ,fg-2))))
   `(org-block                     ((t (:inherit fixed-pitch :background ,bg+1))))
   `(org-block-begin-line          ((t (:inherit (fixed-pitch org-meta-line) :height 0.9 :family "Iosevka Slab"))))
   `(org-block-end-line            ((t (:inherit org-block-begin-line :family "Iosevka Slab"))))
   `(org-code                      ((t (:inherit fixed-pitch :foreground ,magenta :background ,bg+1 :family "Iosevka Slab"))))
   `(org-column                    ((t (:background ,bg+1))))
   `(org-column-title              ((t (:inhenrit (org-column bold) :underline t))))
   `(org-date                      ((t (:inherit (org-agenda-date bold) :underline t))))
   `(org-document-info             ((t (:foreground ,cyan))))
   `(org-document-info-keyword     ((t (:inherit (fixed-pitch font-lock-comment-face) :height 0.9 :family "Iosevka Slab"))))
   `(org-document-title            ((t (:inherit bold :foreground ,yellow :height 1.4))))
   `(org-done                      ((t (:inherit (fixed-pitch shadow) :family "Iosevka Slab"))))
   `(org-drawer                    ((t (:inherit org-meta-line :family "Iosevka Slab"))))
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
   `(org-todo                      ((t (:inherit fixed-pitch :foreground ,orange :family "Iosevka Slab"))))
   `(org-upcoming-deadline         ((t (:foreground ,red))))
   `(org-verbatim                  ((t (:inherit org-code))))
   `(org-warning                   ((t (:inherit warning))))

;;;;; Outline

   `(outline-1 ((t (:inherit org-level-1))))
   `(outline-2 ((t (:inherit org-level-2))))
   `(outline-3 ((t (:inherit org-level-3))))
   `(outline-4 ((t (:inherit org-level-4))))
   `(outline-5 ((t (:inherit org-level-5))))
   `(outline-6 ((t (:inherit org-level-6))))
   `(outline-7 ((t (:inherit org-level-7))))
   `(outline-8 ((t (:inherit org-level-8))))

;;;;; Proced

   `(proced-cpu                  ((t (:foreground ,magenta))))
   `(proced-emacs-pid            ((t (:inherit (proced-pid bold)))))
   `(proced-executable           ((t (:foreground ,fg :inherit bold))))
   `(proced-mem                  ((t (:foreground ,cyan))))
   `(proced-memory-high-usage    ((t (:foreground ,orange))))
   `(proced-memory-low-usage     ((t (:foreground ,green))))
   `(proced-memory-medium-usage  ((t (:foreground ,yellow))))
   `(proced-pid                  ((t (:foreground ,blue))))
   `(proced-session-leader-pid   ((t (:inherit (proced-pid underline)))))
   `(proced-sort-header          ((t (:foreground ,yellow :inherit underline))))
   `(proced-time-colon           ((t (:inherit shadow))))
   `(proced-user                 ((t (:inherit shadow))))

;;;;; RE Builder

   `(reb-match-0 ((t (:inherit highlight))))
   `(reb-match-1 ((t (:inherit isearch))))
   `(reb-match-2 ((t (:inherit isearch-group-1))))
   `(reb-match-3 ((t (:inherit isearch-group-2))))

;;;;; Isearch

   `(isearch         ((t (:foreground ,bg-1 :background ,yellow))))
   `(isearch-group-1 ((t (:foreground ,bg-1 :background ,magenta))))
   `(isearch-group-2 ((t (:foreground ,bg-1 :background ,cyan))))
   `(isearch-fail    ((t (:foreground ,bg-1 :background ,red))))
   `(lazy-highlight  ((t (:foreground ,fg+1 :background ,yellow-bg))))

;;;;; Sh

   `(sh-quoted-exec ((t (:foreground ,red))))

;;;;; Show Paren

   `(show-paren-match    ((t (:foreground ,yellow :background ,yellow-bg))))
   `(show-paren-mismatch ((t (:background ,red-bg))))

;;;;; Terminal

   `(ansi-color-black          ((t (:foreground ,black))))
   `(ansi-color-red            ((t (:foreground ,red))))
   `(ansi-color-green          ((t (:foreground ,green))))
   `(ansi-color-blue           ((t (:foreground ,blue))))
   `(ansi-color-yellow         ((t (:foreground ,yellow))))
   `(ansi-color-magenta        ((t (:foreground ,magenta))))
   `(ansi-color-cyan           ((t (:foreground ,cyan))))
   `(ansi-color-white          ((t (:foreground ,fg+1))))
   `(ansi-color-bright-black   ((t (:foreground ,bg-1))))
   `(ansi-color-bright-red     ((t (:foreground ,red-fg+1))))
   `(ansi-color-bright-green   ((t (:foreground ,green-fg+1))))
   `(ansi-color-bright-blue    ((t (:foreground ,blue-fg+1))))
   `(ansi-color-bright-yellow  ((t (:foreground ,yellow-fg+1))))
   `(ansi-color-bright-magenta ((t (:foreground ,magenta-fg+1))))
   `(ansi-color-bright-cyan    ((t (:foreground ,cyan-fg+1))))
   `(ansi-color-bright-white   ((t (:foreground ,white))))

;;;;; Which Function

   `(which-func ((t (:foreground ,magenta))))

;;;;; Whitespace

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

;;;; Third-Party Packages

;;;;; Avy

   `(avy-goto-char-timer-face ((t (:inherit bold :foreground ,black :background ,red))))
   `(avy-lead-face            ((t (:inherit bold :foreground ,black :background ,yellow))))
   `(avy-lead-face-0          ((t (:inherit avy-lead-face))))
   `(avy-lead-face-1          ((t (:inherit avy-lead-face))))
   `(avy-lead-face-2          ((t (:inherit avy-lead-face))))

;;;;; Corfu

   `(corfu-default    ((t (:foreground ,fg :background ,bg+1))))
   `(corfu-current    ((t (:background ,bg+2))))
   '(corfu-deprecated ((t (:inherit font-lock-comment-face :strike-through t))))
   `(corfu-border     ((t (:background ,bg+3))))
   `(corfu-bar        ((t (:background ,fg-2))))

;;;;; Denote

   `(denote-faces-date      ((t (:foreground ,cyan))))
   `(denote-faces-keywords  ((t (:foreground ,magenta))))
   `(denote-faces-signature ((t (:foreground ,yellow))))

;;;;; Embrace

   `(embrace-help-key-face  ((t (:inherit help-key-binding))))
   `(embrace-help-pair-face ((t (:foreground ,fg :background ,bg))))

;;;;; Magit

   `(git-commit-summary                    ((t (:inherit default))))
   `(magit-blame-heading                   ((t (:foreground ,fg :background ,bg+1))))
   `(magit-branch-local                    ((t (:foreground ,green))))
   `(magit-branch-remote                   ((t (:foreground ,blue))))
   `(magit-diff-added                      ((t (:foreground ,fg-1 :background ,green-bg-1))))
   `(magit-diff-added-highlight            ((t (:background ,green-bg))))
   `(magit-diff-context                    ((t (:foreground ,fg-2))))
   `(magit-diff-context-highlight          ((t (:background ,bg+2))))
   `(magit-diff-file-heading               ((t (:weight semi-bold))))
   `(magit-diff-hunk-heading               ((t (:foreground ,fg-1 :background ,bg+1))))
   `(magit-diff-hunk-heading-highlight     ((t (:foreground ,fg-1 :background ,bg+3))))
   `(magit-diff-removed                    ((t (:foreground ,fg-1 :background ,red-bg-1))))
   `(magit-diff-removed-highlight          ((t (:background ,red-bg))))
   `(magit-diff-revision-summary           ((t (:foreground ,blue :weight bold))))
   `(magit-diff-revision-summary-highlight ((t (:foreground ,blue :weight bold))))
   `(magit-diff-whitespace-warning         ((t (:inherit diff-refine-added))))
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

;;;;; Marginalia

   `(marginalia-date            ((t (:foreground ,cyan))))
   `(marginalia-size            ((t (:foreground ,magenta))))
   `(marginalia-file-priv-dir   ((t (:inherit eshell-ls-directory))))
   `(marginalia-file-priv-exec  ((t (:foreground ,red))))
   `(marginalia-file-priv-key   ((t (:inherit help-key-binding))))
   `(marginalia-file-priv-link  ((t (:inherit eshell-ls-symlink))))
   `(marginalia-file-priv-owner ((t (:inherit shadow))))
   `(marginalia-file-priv-read  ((t (:foreground ,green))))
   `(marginalia-file-priv-write ((t (:foreground ,orange))))

;;;;; Markdown

   `(markdown-blockquote-face         ((t (:inherit org-quote))))
   `(markdown-code-face               ((t (:inherit fixed-pitch :background ,bg+1))))
   `(markdown-header-face             ((t (:inherit bold))))
   `(markdown-header-face-1           ((t (:inherit bold :foreground ,red :height 1.3))))
   `(markdown-header-face-2           ((t (:inherit bold :foreground ,orange :height 1.2))))
   `(markdown-header-face-3           ((t (:inherit bold :foreground ,yellow :height 1.1))))
   `(markdown-header-face-4           ((t (:inherit (bold italic) :foreground ,green))))
   `(markdown-header-face-5           ((t (:inherit (bold italic) :foreground ,cyan))))
   `(markdown-header-face-6           ((t (:inherit italic :foreground ,magenta))))
   `(markdown-html-attr-name-face     ((t (:inherit fixed-pitch))))
   `(markdown-html-attr-value-face    ((t (:inherit fixed-pitch))))
   `(markdown-html-entity-face        ((t (:inherit fixed-pitch))))
   `(markdown-html-tag-delimiter-face ((t (:inherit fixed-pitch))))
   `(markdown-html-tag-name-face      ((t (:inherit fixed-pitch))))
   `(markdown-inline-code-face        ((t (:inherit markdown-code-face :family "Iosevka Slab"))))
   `(markdown-language-info-face      ((t (:inherit fixed-pitch))))
   `(markdown-language-keyword-face   ((t (:inherit (fixed-pitch shadow)))))
   `(markdown-line-break-face         ((t (:inherit default))))
   `(markdown-markup-face             ((t (:inherit (fixed-pitch shadow)))))
   `(markdown-metadata-key-face       ((t (:inherit (fixed-pitch font-lock-comment-face) :height 0.9))))
   `(markdown-metadata-value-face     ((t (:inherit markdown-metadata-key-face))))
   `(markdown-pre-face                ((t (:inherit fixed-pitch :background ,bg+1 :extend t :family "Iosevka Slab"))))
   `(markdown-table-face              ((t (:inherit fixed-pitch))))
   `(markdown-url-face                ((t (:inherit (fixed-pitch shadow)))))

;;;;; Nerd Icons

   `(nerd-icons-blue     ((t (:foreground ,blue))))
   `(nerd-icons-blue-alt ((t (:foreground ,blue-fg+1))))

;;;;; Notmuch

   `(notmuch-crypto-decryption            ((t (:inherit bold))))
   `(notmuch-crypto-part-header           ((t (:foreground ,green-bg))))
   `(notmuch-crypto-signature-bad         ((t (:inherit error))))
   `(notmuch-crypto-signature-good        ((t (:inherit success))))
   `(notmuch-crypto-signature-good-key    ((t (:inherit success))))
   `(notmuch-crypto-signature-unknown     ((t (:inherit warning))))
   `(notmuch-jump-key                     ((t (:inherit help-key-binding))))
   `(notmuch-message-summary-face         ((t (:inherit bold))))
   `(notmuch-search-count                 ((t (:foreground ,fg-2))))
   `(notmuch-search-date                  ((t (:foreground ,fg-1))))
   `(notmuch-search-flagged-face          ((t (:foreground ,yellow))))
   `(notmuch-search-matching-authors      ((t (:foreground ,fg))))
   `(notmuch-search-non-matching-authors  ((t (:inherit shadow))))
   `(notmuch-search-subject               ((t (:foreground ,fg))))
   `(notmuch-search-unread-face           ((t (:inherit bold))))
   `(notmuch-tag-added                    ((t (:inherit (notmuch-tag-face underline)))))
   `(notmuch-tag-deleted                  ((t (:inherit notmuch-tag-face :strike-through t))))
   `(notmuch-tag-face                     ((t (:inherit italic :foreground ,fg-1))))
   `(notmuch-tag-flagged                  ((t (:inherit notmuch-tag-face :foreground ,orange))))
   `(notmuch-tag-unread                   ((t (:inherit notmuch-tag-face))))
   `(notmuch-tree-match-author-face       ((t (:inherit notmuch-search-matching-authors))))
   `(notmuch-tree-match-date-face         ((t (:inherit notmuch-search-date))))
   `(notmuch-tree-match-face              ((t (:foreground ,fg))))
   `(notmuch-tree-match-tag-face          ((t (:inherit notmuch-tag-face))))
   `(notmuch-tree-no-match-date-face      ((t (:inherit shadow))))
   `(notmuch-tree-no-match-face           ((t (:inherit shadow))))
   `(notmuch-wash-cited-text              ((t (:inherit message-cited-text-1))))
   `(notmuch-wash-toggle-button           ((t (:inherit (shadow italic)))))

;;;;; Orderless

   `(orderless-match-face-0 ((t (:background  ,yellow-bg))))
   `(orderless-match-face-1 ((t (:background  ,orange-bg))))
   `(orderless-match-face-2 ((t (:background  ,red-bg))))
   `(orderless-match-face-3 ((t (:background  ,magenta-bg))))

;;;;; Org Noter

   `(org-noter-notes-exist-face    ((t (:inherit bold :foreground  ,green))))
   `(org-noter-no-notes-exist-face ((t (:inherit bold :foreground  ,orange))))

;;;;; Transient

   `(transient-key ((t (:inherit help-key-binding))))
   `(transient-heading ((t (:inherit bold :foreground ,yellow))))
   `(transient-key-stay ((t (:inherit transient-key :foreground ,green))))
   `(transient-key-exit ((t (:inherit transient-key :foreground ,orange))))
   `(transient-key-return ((t (:inherit transient-key :foreground ,yellow))))

;;;;; Which-Key

   `(which-key-command-description-face   ((t (:inherit default))))
   `(which-key-key-face                   ((t (:inherit help-key-binding))))
   `(which-key-local-map-description-face ((t (:foreground ,green))))

   ))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'my-iceberg)
;;; my-iceberg-theme.el ends here

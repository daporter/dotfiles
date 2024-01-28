;;; daporter-theme.el --- My theme                   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  David Porter

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

;; This theme is based on the 2014 Material Design palette:
;;   https://m2.material.io/design/color/the-color-system.html#tools-for-picking-colors

;;; Code:

(deftheme daporter
  "My colour theme."
  :background-mode 'dark)

(let ((base04 "#000000")
      (base03 "#121212")
      (base02 "#2e2e2e")
      (base01 "#4a4a4a")
      (base00 "#666666")
      (base0  "#858585")
      (base1  "#a1a1a1")
      (base2  "#bdbdbd")
      (base3  "#d9d9d9")
      (base4  "#ffffff")

      (yellow100 "#FFF9C4")
      (yellow300 "#FFF59D")
      (yellow500 "#b3a100")
      (yellow700 "#665c00")
      (yellow900 "#332e00")

      (orange100 "#FFE0B2")
      (orange300 "#FFCC80")
      (orange500 "#ca8216")
      (orange700 "#a16812")
      (orange900 "#5c3b0a")
      
      (red100 "#FFCDD2")
      (red300 "#EF9A9A")
      (red500 "#c2160a")
      (red700 "#790e06")
      (red900 "#490804")
      
      (magenta100 "#E1BEE7")
      (magenta300 "#CE93D8")
      (magenta500 "#6f1c7d")
      (magenta700 "#4a1254")
      (magenta900 "#370e3f")
      
      (blue100 "#BBDEFB")
      (blue300 "#90CAF9")
      (blue500 "#0a6fc2")
      (blue700 "#064579")
      (blue900 "#042a49")

      (cyan100 "#B2EBF2")
      (cyan300 "#80DEEA")
      (cyan500 "#00b4cc")
      (cyan700 "#007180")
      (cyan900 "#00444d")

      (green100 "#C8E6C9")
      (green300 "#A5D6A7")
      (green500 "#3e8e41")
      (green700 "#275929")
      (green900 "#173518"))

  (custom-theme-set-faces
   'daporter

   ;; --- Base ----------------------------------------------------------
   `(default   ((t (:foreground ,base4 :background ,base03))))
   `(highlight ((t (:background ,base02))))

   ;; --- Header & mode line --------------------------------------------
   `(mode-line
     ((t (:foreground ,base3 :background ,base04
                      :box (:color ,base04 :line-width 2)))))
   `(mode-line-inactive    ((t (:foreground ,base01 :inherit mode-line))))
   '(mode-line-buffer-id   ((t (:weight regular :background nil))))
   '(mode-line-emphasis    ((t (:weight regular :background nil))))
   `(window-divider        ((t (:foreground ,base00))))
   `(header-line           ((t (:foreground ,base4 :background ,base04))))
   `(fill-column-indicator ((t (:foreground ,base02))))

   ;; --- Structural ----------------------------------------------------
   '(region        ((t (:inherit highlight))))
   `(fringe        ((t (:foreground ,base00))))
   `(hl-line       ((t (:background ,base02))))
   `(link          ((t (:foreground ,blue300 :underline t))))
   `(widget-button ((t (:foreground ,base3 :background ,base02
                        :box (:color ,base02)))))
   `(widget-field  ((t (:foreground ,base3 :background ,base02
                        :box (:line-width (1 . -1) :color ,base03)))))

   ;; --- Semantic ------------------------------------------------------
   `(success             ((t (:foreground ,green300))))
   `(warning             ((t (:foreground ,orange300))))
   `(error               ((t (:foreground ,base4 :background ,red700))))
   `(compilation-error   ((t (:foreground ,red300))))
   '(match               ((t (:weight bold))))
   
   ;; --- General -------------------------------------------------------
   `(minibuffer-prompt   ((t (:foreground ,base4 :background ,base03
                              :weight bold))))
   `(show-paren-match    ((t (:background ,blue700 :weight bold))))
   '(show-paren-mismatch ((t (:inherit error))))
   '(nobreak-hyphen      ((t (:weight bold))))
   '(nobreak-space       ((t (:weight bold))))

   ;; --- Dired -------------------------------------------------------
   '(dired-symlink       ((t (:inherit link))))

   ;; --- Font lock -----------------------------------------------------
   `(font-lock-comment-face       ((t (:foreground ,base1))))
   `(font-lock-string-face        ((t (:foreground ,blue300))))
   '(font-lock-function-name-face ((t (:weight bold))))
   `(font-lock-variable-name-face ((t (:foreground ,base4))))
   `(font-lock-keyword-face       ((t (:foreground ,base4))))
   '(font-lock-builtin-face       ((t (:weight bold))))
   '(font-lock-type-face          ((t (:slant italic))))
   '(font-lock-constant-face      ((t (:weight bold))))
   `(font-lock-preprocessor-face  ((t (:foreground ,yellow300))))
   `(font-lock-warning-face       ((t (:foreground ,orange300))))

   ;; --- Eglot ---------------------------------------------------------
   `(eglot-mode-line             ((t (:foreground ,base3))))
   `(eglot-highlight-symbol-face ((t (:background ,cyan900))))

    ;; --- Eshell -------------------------------------------------------
   '(eshell-prompt ((t (:weight bold))))

   ;; --- Flymake -------------------------------------------------------
   `(flymake-error       ((t (:underline (:style wave :color ,red300)))))
   `(flymake-warning     ((t (:underline (:style wave :color ,orange300)))))
   `(flymake-note        ((t (:underline (:style wave :color ,green300)))))
   '(compilation-error   ((t (:inherit error))))
   '(compilation-warning ((t (:inherit warning))))
   '(compilation-column-number ((t :inherit default :inherit underline)))

   ;; --- Help -------------------------------------------------------
   `(help-key-binding ((t (:inherit fixed-pitch :background ,base03
                           :box (:line-width (-1 . -1) :color ,base02)))))

   ;; --- HL-Todo -------------------------------------------------------
   `(hl-todo ((t (:foreground ,red300 :weight bold))))

   ;; --- Isearch -------------------------------------------------------
   `(isearch ((t (:foreground ,base04 :background ,yellow500))))
   `(lazy-highlight ((t (:foreground ,base04 :background ,yellow700))))
   
   ;; --- Magit ---------------------------------------------------------
   '(git-commit-summary           ((t (:inherit default))))
   `(magit-branch-local           ((t (:foreground ,green300))))
   `(magit-branch-remote          ((t (:foreground ,blue300))))
   '(magit-tag                    ((t (:inherit default))))
   `(magit-section-heading        ((t (:foreground ,yellow300 :weight bold))))
   `(magit-diffstat-added         ((t (:foreground ,green300))))
   `(magit-diffstat-removed       ((t (:foreground ,red300))))
   `(magit-diff-removed-highlight ((t (:background ,red700))))
   `(magit-diff-added-highlight   ((t (:background ,green700))))
   `(magit-log-author             ((t (:foreground ,green300))))

   ;; --- Message -------------------------------------------------------
   '(message-header-name    ((t (:weight bold))))
   `(message-header-subject ((t (:foreground ,yellow300))))
   `(message-header-to      ((t (:foreground ,yellow300))))
   '(message-header-cc      ((t (:inherit default))))
   `(message-header-xheader ((t (:foreground ,base1))))
   `(message-header-other   ((t (:foreground ,base1))))
   `(message-separator      ((t (:foreground ,base01))))
   `(message-cited-text-1   ((t (:foreground ,base3))))
   `(message-cited-text-2   ((t (:foreground ,base1))))
   `(message-cited-text-3   ((t (:foreground ,base00))))
   `(message-cited-text-4   ((t (:foreground ,base01))))
   `(message-mml            ((t (:foreground ,base01))))

   ;; --- Notmuch -------------------------------------------------------
   `(notmuch-search-count         ((t (:foreground ,base1))))
   `(notmuch-search-flagged-face  ((t (:foreground ,yellow300))))
   `(notmuch-tag-face             ((t (:foreground ,cyan300))))
   `(notmuch-tag-unread           ((t (:weight bold))))
   `(notmuch-tag-flagged          ((t (:weight bold))))
   `(notmuch-tag-added            ((t (:underline t :weight bold))))
   `(notmuch-tag-deleted          ((t (:foreground ,base1
                                       :strike-through ,base1))))
   `(notmuch-message-summary-face ((t (:background ,base03))))
   `(notmuch-wash-toggle-button   ((t (:foreground ,base00 :slant italic))))

   ;; --- Orderless -----------------------------------------------------
   `(orderless-match-face-0 ((t (:background ,cyan900))))
   `(orderless-match-face-1 ((t (:background ,cyan900))))
   `(orderless-match-face-2 ((t (:background ,cyan900))))
   `(orderless-match-face-3 ((t (:background ,cyan900))))

   ;; --- Org --------------------------------------------------------
   `(org-date            ((t (:foreground ,base1))))
   `(org-done            ((t (:foreground ,base00))))
   `(org-drawer          ((t (:inherit fixed-pitch :foreground ,base1))))
   '(org-level-2         ((t (:inherit org-level-1))))
   '(org-level-3         ((t (:inherit org-level-1))))
   '(org-level-4         ((t (:inherit org-level-1))))
   '(org-level-5         ((t (:inherit org-level-1))))
   '(org-level-6         ((t (:inherit org-level-1))))
   '(org-level-7         ((t (:inherit org-level-1))))
   '(org-level-8         ((t (:inherit org-level-1))))
   `(org-headline-done   ((t (:foreground ,base00))))
   `(org-special-keyword ((t (:inherit fixed-pitch :foreground ,base1))))
   `(org-todo            ((t (:foreground ,green300 :weight bold))))
   
   ;; --- Org Agenda --------------------------------------------------------
   '(org-agenda-structure           ((t (:weight bold))))
   '(org-agenda-structure-secondary ((t (:slant italic))))

   ;; --- Proced --------------------------------------------------------
   '(proced-user                ((t (:inherit default))))
   '(proced-pid                 ((t (:inherit default))))
   '(proced-session-leader-pid  ((t (:inherit default :weight bold))))
   `(proced-emacs-pid           ((t (:foreground ,yellow300))))
   '(proced-cpu                 ((t (:inherit default))))
   '(proced-mem                 ((t (:inherit default))))
   `(proced-memory-high-usage   ((t (:foreground ,orange300))))
   '(proced-memory-medium-usage ((t (:inherit default))))
   `(proced-memory-low-usage    ((t (:foreground ,green300))))
   `(proced-time-colon          ((t (:foreground ,base0))))
   '(proced-executable          ((t (:inherit default :weight bold))))
   '(proced-sort-header         ((t (:weight bold :underline t))))
   
   ))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'daporter)

;;; daporter-theme.el ends here

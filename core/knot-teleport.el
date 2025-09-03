;;; knot-teleport.el --- Spacetime teleportation in Emacs -*- lexical-binding: t; -*-

;; All config that let me teleport across space (buffers, windows, frames, files)
;; and time (history, recent items).

;;; `ace-window'
;; Teleport to any window on the visible screen
(use-package ace-window
  :bind (("C-c t" . ace-window))
  :custom
  (aw-keys '(?t ?n ?e ?i ?o ?s ?r ?a)) ; Colemak-DH optimization
  (aw-background nil))

;;; `avy' --- Goku's Instant Transmission
;; Teleport to any text in the visible frame instantly
(use-package avy
  :bind (("C-,"   . avy-goto-char-timer)
         ("C-'"   . avy-goto-char-2)
         ("M-g f" . avy-goto-line))
  :custom
  (avy-background nil)
  (avy-style 'pre)
  (avy-all-windows t) ; Use all windows on the selected frame
  (avy-timeout-seconds 0.2) ; How long avy-goto-char-timer should wait
  (avy-keys '(?s ?t ?n ?e ?g ?m ?r ?i ?f ?u ?a ?o))) ; Colemak-DH optimization

;;; `bookmark' --- A variant of Minato's Flying Raijin
;; Set a marker, jump back instantly (markers persist across restarts)
;; Intergration with 'consult' via 'consult-bookmark'
(use-package bookmark
  :ensure nil
  :bind(("C-c B" . bookmark-set))
  :config
  (setq bookmark-save-flag 1)
  (setq bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory))
  (setq bookmark-bmenu-toggle-filenames t))

;;; `isearch'
;; Integration with avy
(use-package isearch
  :ensure nil
  :custom
  (isearch-allow-scroll 'unlimited)
  (isearch-lazy-count t)
  (isearch-repeat-on-direction-change t)
  (search-default-mode 'char-fold-to-regexp) ; matches accented letters too
  (search-whitespace-regexp ".*?")) ; search for "te n" matches "teleportation"

;;; `recentf'
;; Instant time travel, made better with `consult-recent-file'
(use-package recentf
  :ensure nil
  :init (recentf-mode 1)
  :custom
  (recentf-max-saved-items 200)
  (recentf-max-menu-items 25)
  (recentf-save-file (expand-file-name "recentf" user-emacs-directory))
  (recentf-auto-cleanup 'never)
  :config
  (run-at-time nil (* 5 60) #'recentf-save-list)) ; Save recentf list every 5 minutes

;;; `register' --- A super fast variant of Minato's Flying Raijin
;; Set markers, save regions, and jump back, yank saved regions instantly
(use-package register
  :ensure nil
  :bind (("C-|"   . point-to-register)
         ("C-M-|" . window-configuration-to-register)
         ("C-\\"  . jump-to-register))
  :custom
  (register-use-preview nil)) ; preview without delay

;;; `savehist'
;; Save minibuffer-history
(use-package savehist
  :demand t
  :ensure nil
  :init (savehist-mode 1)
  :custom
  (savehist-file (locate-user-emacs-file "history"))
  (history-length 2000)
  (savehist-additional-variables
   '(kill-ring
     register-alist
     search-ring
     regexp-search-ring)))

;;; `winner'
;; To restore or go back to previous window configurations
(use-package winner
  :ensure nil
  :init (winner-mode)
  :bind (("C-c w u" . winner-undo)
         ("C-c w r" . winner-redo)))

(provide 'knot-teleport)

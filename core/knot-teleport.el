;;; knot-teleport.el --- Spacetime teleportation in Emacs -*- lexical-binding: t; -*-

;; All config that let me teleport across space (buffers, windows, frames, files)
;; and time (history, recent items).

;;; `ace-window'
;; Teleport to any window on the visible screen
(use-package ace-window
  :bind (("C-c t" . ace-window)) ;; t for tabs?, no t for windows!
  :custom
  ;; Optimized for Colemak-DH
  (aw-keys '(?t ?n ?e ?i ?o ?s ?r ?a))
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
  ;; Use all windows on the selected frame
  (avy-all-windows t)
  ;; How long avy-goto-char-timer should wait
  (avy-timeout-seconds 0.2)
  ;; Optimized for Colemak-DH
  (avy-keys '(?s ?t ?n ?e ?g ?m ?r ?i ?f ?u ?a ?o)))

;;; `bookmark' --- A variant of Minato's Flying Raijin
;; Set a marker, jump back instantly (markers persist across restarts)
;; Intergration with 'consult' via 'consult-bookmark'
(use-package bookmark
  :ensure nil
  :config
  (setq bookmark-save-flag 1)
  (setq bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory))
  (setq bookmark-bmenu-toggle-filenames t))

;;; `register' --- A faster variant of Minato's Flying Raijin
;; Set markers, save regions, and jump back, yank saved regions instantly
(use-package register
  :ensure nil
  :custom
  (register-preview-delay 0.2))

;;; `recentf'
;; Time travel (teleport back to the past), made better with `consult-recent-file'
(use-package recentf
  :ensure nil
  :init (recentf-mode 1)
  :custom
  (recentf-max-saved-items 200)
  (recentf-max-menu-items 25)
  (recentf-save-file (expand-file-name "recentf" user-emacs-directory))
  (recentf-auto-cleanup 'never)
  :config
  ;; Save recentf list every 5 minutes
  (run-at-time nil (* 5 60) #'recentf-save-list))

;;; `savehist'
;; Save minibuffer-history
(use-package savehist
  :demand t
  :ensure nil
  :init (savehist-mode)
  :custom
  (savehist-file (locate-user-emacs-file "history"))
  (history-length 2000)
  (savehist-additional-variables
   '(kill-ring
     register-alist
     search-ring
     regexp-search-ring)))

(provide 'knot-teleport)

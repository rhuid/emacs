;;; knot-teleport.el --- Spacetime teleportation in Emacs -*- lexical-binding: t; -*-

;;;; Goku's Instant Transmission
(use-package avy
  :bind (("C-," . avy-goto-char-timer)
         :map isearch-mode-map
         ("C-," . avy-isearch))
  :custom
  (avy-timeout-seconds 0.2)
  (avy-keys '(?s ?t ?n ?e ?g ?m ?r ?i ?f ?u ?a ?o)))

;;;; A variant of Minato's Flying Raijin
(use-package bookmark
  :ensure nil
  :config
  (setq bookmark-save-flag 1)
  (setq bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory))
  (setq bookmark-bmenu-toggle-filenames t))

(use-package isearch
  :ensure nil
  :bind (:map isearch-mode-map
              ("C-h" . isearch-del-char))
  :custom
  (isearch-allow-scroll 'unlimited)
  (isearch-lazy-count t)
  (isearch-repeat-on-direction-change t)
  (search-default-mode 'char-fold-to-regexp) ; matches accented letters too
  (search-whitespace-regexp ".*?")) ; search for "t n" matches "teleportation"

(use-package project
  :custom
  (project-switch-commands
   '((magit-project-status "Magit"     ?m)
     (project-find-file    "Find file" ?f)
     (project-dired        "Dired"     ?d)
     (project-eshell       "Eshell"    ?e))))

;; Instant time travel, made better with `consult-recent-file'
(use-package recentf
  :ensure nil
  :init (recentf-mode)
  :custom
  (recentf-max-saved-items 200)
  (recentf-max-menu-items 25)
  (recentf-save-file (expand-file-name "recentf" user-emacs-directory))
  (recentf-auto-cleanup 'never)
  :config
  (run-at-time nil (* 5 60) #'recentf-save-list)) ; Save recentf list every 5 minutes

;;; A super fast variant of Minato's Flying Raijin
(use-package register
  :ensure nil
  :bind (("C-|"   . point-to-register)
         ("C-M-|" . window-configuration-to-register)
         ("C-\\"  . jump-to-register)
         ("C-M-m" . rh/set-point-to-register-1)
         ("C-M-j" . rh/jump-to-register-1)
         ("C-S-m" . rh/set-window-config-to-register-2)
         ("C-S-j" . rh/jump-to-window-config-2))
  :custom
  (register-use-preview nil) ; preview without delay
  :config
  (defun rh/set-point-to-register-1 ()
    "Store current point in register 1."
    (interactive)
    (point-to-register ?1)
    (message "Point marked."))

  (defun rh/set-window-config-to-register-2 ()
    "Store current window configuration in register 2."
    (interactive)
    (window-configuration-to-register ?2)
    (message "Window configuration marked."))

  (defun rh/jump-to-register-1 ()
    "Jump to position stored in register 1."
    (interactive)
    (jump-to-register ?1)
    (message "Teleported to the mark."))

  (defun rh/jump-to-window-config-2 ()
    "Jump to position stored in register 1."
    (interactive)
    (jump-to-register ?2)
    (message "Teleported to the window configuration.")))

;; Save minibuffer history
(use-package savehist
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

;; Teleport to any neighbor window
(use-package windmove
  :ensure nil
  :init
  (windmove-default-keybindings)
  :config
  (setq windmove-wrap-around t))

;; To restore or go back to previous window configurations
(use-package winner
  :ensure nil
  :init (winner-mode)
  :bind (("C-S-W" . winner-undo)
         ("C-c w r" . winner-redo))
  :config
  (defvar-keymap rh/winner-repeat-map
    :repeat t
    "u" #'winner-undo
    "r" #'winner-redo)
  :custom
  (winner-boring-buffers
   '("*Messages*" "*Completions*" "*Buffer List*" "*Async-native-compile-log*" "*scratch*")))

(provide 'knot-teleport)

;;; knot-built-ins.el --- tools which came built-in with emacs -*- lexical-binding: t; -*-

;; (require 'rh-capitalize)

(use-package recentf :straight nil :demand t 
  :init
  (recentf-mode 1)
  :custom
  (recentf-max-saved-items 100)
  (recentf-max-menu-items 25)
  (recentf-save-file (expand-file-name "recentf" user-emacs-directory))
  (recentf-auto-cleanup 'never)
  :config
  ;; Save recentf list every 5 minutes
  (run-at-time nil (* 5 60) #'recentf-save-list))

(use-package calc :straight nil
  :config
  (add-hook 'calc-trail-mode-hook 'evil-insert-state))

(use-package bookmark :straight nil :demand t
  :config
  (setq bookmark-save-flag 1)
  (setq bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory))
  (setq bookmark-bmenu-toggle-filenames t))

(provide 'knot-built-ins)

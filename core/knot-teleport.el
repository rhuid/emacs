;;; knot-teleport.el --- Spacetime teleportation in Emacs -*- lexical-binding: t; -*-

(use-package avy
  :bind (("C-," . avy-goto-char-timer)
         :map isearch-mode-map
         ("C-," . avy-isearch))
  :custom
  (avy-timeout-seconds 0.2)
  (avy-keys '(?s ?t ?n ?e ?g ?m ?r ?i ?f ?u ?a ?o)))

(use-package bookmark
  :ensure nil
  :config
  (setq bookmark-save-flag 1)
  (setq bookmark-bmenu-toggle-filenames t))

(use-package isearch
  :ensure nil
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

(use-package recentf
  :ensure nil
  :init (recentf-mode)
  :custom
  (recentf-max-saved-items 200)
  (recentf-max-menu-items 25)
  (recentf-auto-cleanup 'never)
  :config (run-at-time nil (* 5 60) #'recentf-save-list)) ; Save recentf list every 5 minutes

(use-package register
  :ensure nil
  :custom (register-use-preview nil)) ; preview without delay

(use-package savehist
  :ensure nil
  :init (savehist-mode)
  :custom
  (history-length 2000)
  (history-delete-duplicates t)
  (savehist-additional-variables '(kill-ring search-ring regexp-search-ring)))

;; (use-package winner-mode
;;   :init (winner-mode)
;;   :bind ("C-<backspace>" . winner-undo)
;;   :custom
;;   :ensure nil)

(provide 'knot-teleport)

;;; 90-tools.el --- just tools -*- lexical-binding: t; -*-

;; (require 'rh-capitalize)
(use-package centaur-tabs :disabled t
  :demand nil
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-style "chamfer")
  (setq centaur-tabs-height 24)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-icon-type 'all-the-icons)  ; or 'nerd-icons
  (setq centaur-tabs-set-close-button nil)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

;; (use-package auto-complete
;;  :config
;;  (ac-config-default))

(use-package magit :straight t :defer t
  :commands (magit-status magit-log)
  :config

  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-restore-window-configuration-after-quit nil)

  (defun rh/magit-quick-commit ()
    "Prompt for a commit message in minibuffer and commit immediately."
    (interactive)
    (let ((msg (read-string "Commit message: ")))
      (magit-commit-create `("-m" ,msg))))

  (define-key magit-status-mode-map (kbd "C-c q") #'rh/magit-quick-commit)

  (defun rh/magit-quick-amend ()
    "Quickly amend last commit with a new message via minibuffer."
    (interactive)
    (let ((msg (read-string "Amend message: ")))
      (magit-commit-create `("--amend" "-m" ,msg))))

  (define-key magit-status-mode-map (kbd "C-c a") #'rh/magit-quick-amend)

  (define-key magit-mode-map (kbd "C-c u") #'magit-unstage)
  (define-key magit-mode-map (kbd "C-c U") #'magit-unstage-all)
  )

(use-package aggressive-indent :straight t :defer t
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
         (lisp-mode . aggressive-indent-mode))
  :config
  (setq aggressive-indent-comments-too t))

;; (use-package captain)

;; (use-package chess)

(use-package rainbow-mode :straight t :defer t
  :hook (prog-mode . rainbow-mode))

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

(use-package sudo-edit :straight t :commands (sudo-edit))

(provide '90-tools)

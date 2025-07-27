;;; knot-extra-tools.el --- Some great tools including magit and more -*- lexical-binding: t; -*-

(use-package aggressive-indent :straight t :defer t
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
         (lisp-mode . aggressive-indent-mode))
  :config
  (setq aggressive-indent-comments-too t))

;; (use-package auto-complete
;;  :config
;;  (ac-config-default))

;; (use-package captain)

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

;; (use-package chess)

(use-package emms :straight t
  :config
  (setq emms-player-list '(emms-player-mpv)
        emms-source-file-default-directory "~/Downloads/DB Scores/")
  (require 'emms-setup)
  (emms-all) ;; or (emms-standard) for a lighter setup
  (require 'emms-player-mpv)
  (setq emms-mode-line-format " %s"
        emms-mode-line-titlebar-format "EMMS: %s")
  (emms-mode-line-mode 1))

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

  (defun rh/magit-quick-amend ()
    "Quickly amend last commit with a new message via minibuffer."
    (interactive)
    (let ((msg (read-string "Amend message: ")))
      (magit-commit-create `("--amend" "-m" ,msg)))))

(use-package rainbow-mode :straight t
  :hook (prog-mode . rainbow-mode))

(use-package sudo-edit :straight t
  :commands (sudo-edit))

(provide 'knot-extra-tools)

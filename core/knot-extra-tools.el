;;; knot-extra-tools.el --- Some great tools including magit and more -*- lexical-binding: t; -*-

;; (use-package auto-complete
;;  :config
;;  (ac-config-default))

(use-package avy
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2))
  :custom
  (avy-background t)
  )

(use-package emms
  :vc (:url "https://git.savannah.gnu.org/git/emms.git")
  :config
  (setq emms-player-list '(emms-player-mpv)
        emms-source-file-default-directory "~/Downloads/DB Scores/")
  (require 'emms-setup)
  (emms-all) ;; or (emms-standard) for a lighter setup
  (require 'emms-player-mpv)
  (setq emms-mode-line-format " %s"
        emms-mode-line-titlebar-format "EMMS: %s")
  (emms-mode-line-mode 1))

(use-package magit
  :commands (magit-status magit-log)
  :bind (:map magit-mode-map
              ("." . rh/magit-quick-commit)
              ("," . rh/magit-quick-amend))

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

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package sudo-edit
  :commands (sudo-edit))

(provide 'knot-extra-tools)

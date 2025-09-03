;;; knot-extra-tools.el --- Some great tools including magit and more -*- lexical-binding: t; -*-

;;; Things about windows

;; Make windows proportional while adding or deleting windows
(setq window-combination-resize t)

(use-package window
  :ensure nil
  :bind (("C-c k" . delete-window)
         ("C-c K" . kill-buffer-and-window)
         ("C-c n" . split-window-horizontally)
         ("C-c N" . split-window-vertically)))

;;; Play music with EMMS. I am using mpv as backend

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

;;; The modeline is expendable in some major modes

(use-package hide-mode-line
  :demand t
  :bind ("C-<f7>" . global-hide-mode-line-mode)
  :hook ((dired-mode org-mode eshell-mode) . hide-mode-line-mode))

;;; Magit is a super good interface for Git

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

;;; Rainbow mode: Colorize stings that represent colors

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

;;; Edit files as sudo user

(use-package sudo-edit
  :commands (sudo-edit))

(provide 'knot-extra-tools)

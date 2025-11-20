;;; knot-dired.el --- `dired' is the greatest file manager. Does anybody know that? -*- lexical-binding: t; -*-

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("b" . dired-up-directory)                  ; `b' for back
              ("r" . dired-toggle-read-only)              ;  enter `WDired' mode!
              ("f" . dired-display-file))
  :hook (dired-mode . rh/dired-init)
  :config
  (defun rh/dired-init ()
    (dired-hide-details-mode)
    (display-line-numbers-mode -1)
    (set-window-buffer (selected-window) (current-buffer))
    (hl-line-mode 1))
  (setq dired-listing-switches "-alh --group-directories-first")
  (setq dired-dwim-target t)
  (setq dired-mouse-drag-files t)                                                       ; drag and drop files with mouse
  (setq dired-recursive-copies 'always)
  (use-package diredfl :init (diredfl-global-mode))                                     ; make `dired' look a bit more colorful
  (use-package all-the-icons-dired :hook (dired-mode . all-the-icons-dired-mode))
  (use-package dired-git-info :bind (:map dired-mode-map (")" . dired-git-info-mode))))

(provide 'knot-dired)

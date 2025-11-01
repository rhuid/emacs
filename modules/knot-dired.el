;;; knot-dired.el --- Dired is the greatest file manager. Does anybody know that? -*- lexical-binding: t; -*-

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("b" . dired-up-directory)                  ; `b' for back
              ("r" . dired-toggle-read-only)              ;  enter `WDired' mode!
              ("f" . dired-display-file))                 ; `f' for display-`f'ile
  :hook ((dired-mode . rh/dired-setup)
	       (dired-mode . dired-hide-details-mode)
	       (dired-mode . dired-click-to-select-mode))
  :config
  (defun rh/dired-setup ()
    (display-line-numbers-mode -1)
    (set-window-buffer (selected-window) (current-buffer))
    (hl-line-mode 1))
  (setq dired-listing-switches "-alh --group-directories-first"
	      dired-dwim-target t
	      dired-mouse-drag-files t                                         ; drag and drop files with mouse
	      dired-recursive-copies 'always)
  (use-package diredfl :init (diredfl-global-mode))                      ; make dired look better
  (use-package all-the-icons-dired                                       ; icons for dired buffer
    :hook (dired-mode . all-the-icons-dired-mode))
  (use-package dired-git-info                                            ; show git info in dired
    :bind (:map dired-mode-map (")" . dired-git-info-mode))
    :config (setq dgi-auto-hide-details-p nil)))

(provide 'knot-dired)

;;; knot-dired.el --- Enchancements of the already great file manager -*- lexical-binding: t; -*-

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("g"    . dired-git-info-mode)
              ("r"    . dired-up-directory)
              ("i"    . rh/dired-open-file)
              ("u"    . dired-unmark)
              ("U"    . dired-unmark-all-marks)
              ("<f5>" . revert-buffer-quick))
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
	      dired-mouse-drag-files t
	      dired-recursive-copies 'always))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package diredfl
  :hook (dired-mode . diredfl-mode)
  :config (diredfl-global-mode 1))

(use-package dired-git-info
  :after dired
  :bind (:map dired-mode-map (")" . dired-git-info-mode))
  :config (setq dgi-auto-hide-details-p nil))

(use-package dired-preview :disabled t :after dired
  :hook (dired-mode . dired-preview-mode)
  :config
  (setq dired-preview-delay 0.2
        dired-preview-max-size 10                   ;; max 10 MB
	      dired-preview-use-timer t))

(use-package dired-du :disabled t
  :after dired
  :config (add-hook 'dired-mode-hook #'dired-du-mode))

(defun open-in-file-manager ()
  "Open the current directory in the system's GUI file manager."
  (interactive)
  (let ((path (shell-quote-argument (expand-file-name default-directory))))
    (cond ((eq system-type 'windows-nt)
	         (shell-command (concat "explorer " (replace-regexp-in-string "/" "\\" path t t))))
	        ((eq system-type 'darwin)
	         (shell-command (concat "open " path)))
	        ((eq system-type 'gnu/linux)
	         (shell-command (concat "xdg-open " path))))))

(defun rh/dired-open-file ()
  "Smart open for files in Dired:
- Directories: open in Dired.
- Known external files (e.g., media, PDF): open externally.
- Audio files: play with EMMS.
- All others: open in Emacs side window."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (cond
     ;; Directories: open in same buffer
     ((file-directory-p file)
      (dired-find-alternate-file))

     ;; Audio files: play with EMMS
     ((string-match-p
       (rx (or ".mp3" ".flac" ".wav" ".m4a" ".ogg" ".opus") eos)
       file)
      (emms-play-file file))

     ;; CBZ: open with yacreader
     ((string-match-p
       (rx (or ".cbz") eos)
       file)
      (start-process "com.yacreader.YACReader" nil "com.yacreader.YACReader" file))

     ;; External files: open with system default
     ((string-match-p
       (rx (or ".mp4" ".mkv" ".avi"
               ".jpg" ".jpeg" ".png" ".gif" ".svg"
               ".pdf" ".epub" ".cbr") eos)
       file)
      (cond ((eq system-type 'windows-nt)
             (shell-command (concat "start \"\"" (shell-quote-argument file))))
            ((eq system-type 'darwin)
             (shell-command (concat "open " (shell-quote-argument file))))
            ((eq system-type 'gnu/linux)
             (shell-command (concat "xdg-open " (shell-quote-argument file))))))

     ;; Everything else: open in Emacs side window
     (t
      ;; (display-buffer (find-file-noselect file))
      (dired-display-file)))))

(provide 'knot-dired)

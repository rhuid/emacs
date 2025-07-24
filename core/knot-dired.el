;;; knot-dired.el --- Enchancements of the already great file manager -*- lexical-binding: t; -*-

(use-package dired :straight nil :defer t
  :config
  (setq dired-listing-switches "-alh")
  (setq dired-dwim-target t))

(use-package dired-preview :straight t :disabled t :after dired
  :hook (dired-mode . dired-preview-mode)
  :config
  (setq dired-preview-delay 0.2
        dired-preview-max-size 10                   ;; max 10 MB
	dired-preview-use-timer t))

(use-package all-the-icons-dired :straight t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package peep-dired :straight t :disabled t :after dired
  ;; :hook (peep-dired-mode . evil-normalize-keymaps)
  :config
  (define-key dired-mode-map (kbd "P") #'peep-dired))

(use-package ranger :straight t :disabled t
  :config
  (ranger-override-dired-mode t))

(use-package dired-du :straight t :disabled t
  :after dired
  ;; This package really slows down dired
  :config
  (add-hook 'dired-mode-hook #'dired-du-mode))

(use-package dired-git-info :straight t :after dired
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode))            ;; press `)` to toggle git info
  :config
  (setq dgi-auto-hide-details-p nil))

;; ;; Create a new file
;; (defun my/dired-create-file (filename)
;;   "Create a new file named FILENAME in the current dired directory."
;;   (interactive (list (read-file-name "New file: " (dired-current-directory))))
;;   (let ((full-path (expand-file-name filename (dired-current-directory))))
;;     (if (file-exists-p full-path)
;; 	(message "File already exists")
;;       (write-region "" nil full-path)
;;       (dired-add-file full-path)
;;       (revert-buffer))))

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

     ;; External files: open with system default
     ((string-match-p
       (rx (or ".mp4" ".mkv" ".avi"
               ".jpg" ".jpeg" ".png" ".gif" ".svg"
               ".pdf" ".epub" ".cbz" ".cbr") eos)
       file)
      (cond ((eq system-type 'windows-nt)
             (shell-command (concat "start \"\"" (shell-quote-argument file))))
            ((eq system-type 'darwin)
             (shell-command (concat "open " (shell-quote-argument file))))
            ((eq system-type 'gnu/linux)
             (shell-command (concat "xdg-open " (shell-quote-argument file))))))

     ;; Everything else: open in Emacs side window
     (t
      (display-buffer (find-file-noselect file))))))

;; Default keybindings
;; d        Mark for delete
;; C        Copy

(provide 'knot-dired)

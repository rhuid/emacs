;;; knot-packages.el --- Some great tools including magit and more -*- lexical-binding: t; -*-

;;; Things about windows

;; Make windows proportional while adding or deleting windows
(setq window-combination-resize t)

(use-package window
  :ensure nil
  :bind
  ("C-S-f" . follow-mode)
  ("<f9>"  . balance-windows)
  ("C-c k" . delete-window)
  ("C-c K" . kill-buffer-and-window)
  ("C-c n" . split-window-horizontally)
  ("C-c N" . split-window-vertically))

(use-package achievements
  :init (achievements-mode))

;;; `dictionary'
(use-package dictionary
  :bind (("C-c d l" . dictionary-lookup-definition)
         ("C-c d s" . dictionary-search)))

;;; `dictrus' --- https://github.com/rhuid/dictrus
(use-package emacs
  :bind ("C-c d d" . rh/dictrus-lookup)
  :config
  (defun rh/dictrus-lookup (word)
    "Look up WORD using the dictrus CLI and display the result in a buffer."
    (interactive (list (read-string "Lookup word: " (thing-at-point 'word t))))
    (let* ((buf (get-buffer-create "*Dictrus*"))
           (result (with-temp-buffer
                     (call-process "dictrus" nil t nil word)
                     (buffer-string))))
      (with-current-buffer buf
        (read-only-mode -1)
        (erase-buffer)
        (insert (format "Word: %s\n\n%s" word result))
        (goto-char (point-min))
        (view-mode 1)) ;; view-mode for easy quit
      (pop-to-buffer buf))))

;;; `electric-pair-mode`
;; Automatically insert matching delimiters (parentheses, quotes, braces, etc)
(use-package elec-pair
  :demand t
  :config (electric-pair-mode)
  :hook (org-mode . rh/org-electric-pairs)
  :custom (electric-pair-pairs '((?\(.?\)) (?\{.?\}) (?\[.?\])
                                 (?\".?\") (?\<.?\>)))
  :config
  (defun rh/org-electric-pairs ()
    "Org pairs for electric-pair-mode."
    (setq-local electric-pair-pairs (append '((?/.?/) (?_.?_) (?~.?~))))))

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

;;; `magit'
(use-package magit
  :commands (magit-status magit-log)
  :bind ("C-x g" .  magit-status)
  (:map magit-mode-map
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

;;; `outline'
(use-package outline
  :bind
  ("C-S-t" . rh/outline-toggle-heading)
  ("C-S-o" . rh/outline-toggle-visibility)
  :hook
  (prog-mode . outline-minor-mode)
  (text-mode . outline-minor-mode)
  (outline-minor-mode . outline-show-all)
  (outline-minor-mode . outline-hide-body)
  :init
  ;; Set the keybinding prefix for built-in outline commands
  (setq outline-minor-mode-prefix (kbd "C-c @"))

  :config
  ;; Custom folding indicator (like +)
  (set-display-table-slot
   standard-display-table
   'selective-display
   (let ((face-offset (* (face-id 'shadow) (ash 1 22))))
     (vconcat (mapcar (lambda (c) (+ face-offset c)) " +"))))

  (defun rh/outline-toggle-heading ()
    "Toggle visibility of current outline heading."
    (interactive)
    (save-excursion
      (outline-back-to-heading)
      (if (outline-invisible-p (line-end-position))
          (outline-show-subtree)
        (outline-hide-subtree))))

  (defun rh/outline-toggle-visibility ()
    "Toggle between fully expanded and folded view of the outline buffer."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (if (cl-some (lambda (pos)
                     (goto-char pos)
                     (outline-invisible-p (line-end-position)))
                   (rh/outline-all-heading-positions))
          (outline-show-all)
        (outline-hide-body))))

  (defun rh/outline-all-heading-positions ()
    "Return a list of positions of all headings in the buffer."
    (let (positions)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward outline-regexp nil t)
          (push (point) positions)))
      positions)))

;;; `rainbow-delimiters'
;; Different color for each pair of parenthesis
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;; Edit files as sudo user
(use-package sudo-edit
  :commands (sudo-edit))

;;; `yasnippet'
;; Use them when abbrevs don't cut it
(use-package yasnippet
  :demand t
  :config
  (yas-global-mode)
  :custom
  (setq yas-snippet-dirs (list (concat user-emacs-directory "snippets"))))

(provide 'knot-packages)

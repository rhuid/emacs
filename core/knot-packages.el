;;; knot-packages.el --- Some great tools including magit and more -*- lexical-binding: t; -*-

(use-package visual-fill-column
  :hook ((org-mode emacs-lisp-mode) . visual-fill-column-mode)
  :custom
  (visual-fill-column-width 120)
  (visual-fill-column-center-text t))

(use-package achievements
  :init (achievements-mode))

;;; `dictrus' --- https://github.com/rhuid/dictrus
(use-package dictrus
  :load-path "~/.emacs.d/experimental/"
  :bind ("C-c d d" . rh/dictrus-lookup))

(use-package eldoc
  :ensure nil
  :init (global-eldoc-mode)
  :config (setq eldoc-idle-delay 0.2))

(use-package elec-pair
  :init (electric-pair-mode)
  :hook (org-mode . rh/org-electric-pairs)
  :custom (electric-pair-pairs '((?\(.?\)) (?\{.?\}) (?\[.?\]) (?\".?\") (?\<.?\>)))
  :config
  (defun rh/org-electric-pairs ()
    "Org pairs for electric-pair-mode."
    (setq-local electric-pair-pairs (append '((?/.?/) (?_.?_) (?~.?~))))))

(use-package expand-region
  :bind (("<Ci>" . er/expand-region)
         ("C-="  . er/expand-region)))

(use-package keyfreq
  :init (keyfreq-mode)
  :config (keyfreq-autosave-mode))

(use-package hippie-exp
  :bind ("C-S-e" . hippie-expand)
  :custom
  (hippie-expand-try-functions-list
   '(try-complete-file-name
     try-complete-file-name-partially
     try-expand-dabbrev-visible
     try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     try-expand-list
     try-expand-line
     try-expand-dabbrev-from-kill
     try-complete-lisp-symbol
     try-complete-lisp-symbol-partially
     try-expand-all-abbrevs)))

;; Requires enchant and dictionary backend
;; I am using `hunspell-en_us'
(use-package jinx
  :init (global-jinx-mode)
  :bind ("C-S-c" . jinx-correct)
  :custom (jinx-languages "en_US-large"))

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

(use-package multiple-cursors
  :bind (("C-S-l" . mc/edit-lines)
         ("C-S-a" . mc/mark-all-like-this)
         ("C-S-n" . mc/mark-next-like-this)
         ("C-S-p" . mc/mark-previous-like-this)
         ("C->"   . mc/skip-to-next-like-this)
         ("C-<"   . mc/skip-to-previous-like-this))
  :custom (mc/always-run-for-all t))

(use-package outline
  :hook ((prog-mode text-mode) . outline-minor-mode)
  :init (setq outline-minor-mode-prefix (kbd "C-c o")))

(use-package move-text
  :init (move-text-default-bindings))

(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config (pdf-tools-install))

(use-package puni
  :init (puni-global-mode)
  :bind (:map puni-mode-map
              ("C-S-h" . puni-backward-kill-word)
              ("C-w"   . nil))) ; for whole-line-or-region-kill-region

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package sudo-edit
  :commands (sudo-edit))

(use-package vundo
  :bind (("C-x u" . vundo)
         ("M-U"   . undo-redo))
  :custom (undo-limit (* 16 1024 1024)))

(use-package whole-line-or-region
  :init (whole-line-or-region-global-mode))

(use-package yasnippet
  :init (yas-global-mode)
  :custom (setq yas-snippet-dirs (list (concat user-emacs-directory "snippets"))))

(provide 'knot-packages)

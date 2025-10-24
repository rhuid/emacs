;;; knot-packages.el --- Tools, tools, tools! -*- lexical-binding: t; -*-

(use-package abbrev
  :ensure nil
  :init
  (defun rh/context-sensitive-abbrev-expand (fun &rest args)
    "Advice to prevent abbrev expansion inside comments and strings."
    (unless (nth 8 (syntax-ppss))
      (apply fun args)))
  (advice-add 'abbrev--default-expand :around #'rh/context-sensitive-abbrev-expand)
  :config
  (setq-default abbrev-mode t)
  (setq abbrev-file-name (expand-file-name "library/abbrevs.el" user-emacs-directory))
  (read-abbrev-file abbrev-file-name)
  (setq save-abbrevs 'silently))

(use-package avy
  :bind (("C-," . avy-goto-char-timer)
         :map isearch-mode-map
         ("C-," . avy-isearch))
  :custom
  (avy-timeout-seconds 0.2)
  (avy-keys '(?s ?t ?n ?e ?g ?m ?r ?i ?f ?u ?a ?o)))

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
    (setq-local electric-pair-pairs (append '((?_.?_) (?~.?~))))))

(use-package expand-region
  :bind ("C-&" . er/expand-region))

(use-package keyfreq
  :init (keyfreq-mode)
  :config (keyfreq-autosave-mode))

(use-package hippie-exp
  :bind ("<Ci>" . hippie-expand)
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

(use-package isearch
  :ensure nil
  :custom
  (isearch-allow-scroll 'unlimited)
  (isearch-lazy-count t)
  (isearch-repeat-on-direction-change t)
  (search-default-mode 'char-fold-to-regexp) ; matches accented letters too
  (search-whitespace-regexp ".*?")) ; search for "t n" matches "teleportation"

;; Requires enchant and dictionary backend. I am using `hunspell-en_us'
(use-package jinx
  :init (global-jinx-mode)
  :bind ("C-*" . jinx-correct) ("C-M-*" . jinx-correct-word)
  :custom (jinx-languages "en_US-large"))

(use-package magit
  :bind ("C-x g" .  magit-status)
  (:map magit-mode-map
        ("." . rh/magit-quick-commit)
        ("," . rh/magit-quick-amend))
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
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

;; My keybindings are a little weird but they work well for me.
(use-package multiple-cursors
  :bind (("C-M-S-a" . mc/mark-all-like-this-dwim)
         ("C-M-S-n" . mc/mark-next-like-this)
         ("C-M-S-p" . mc/mark-previous-like-this)
         ("C-M->"   . mc/skip-to-next-like-this)
         ("C-M-<"   . mc/skip-to-previous-like-this))
  :custom (mc/always-run-for-all t))

(use-package outline
  :hook ((prog-mode text-mode) . outline-minor-mode)
  :init (setq outline-minor-mode-prefix (kbd "C-c o")))

(use-package move-text
  :bind ("H-p" . move-text-up) ("H-n" . move-text-down)
  :init (move-text-default-bindings))

(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

;; Manage projects. Comes built-in, a good alternative to projectile.
(use-package project
  :custom (project-switch-commands
           '((magit-project-status "Magit"     ?m)
             (project-find-file    "Find file" ?f)
             (project-dired        "Dired"     ?d)
             (project-eshell       "Eshell"    ?e))))

;; Structural editing: lightweight, minimal and language-agnostic
(use-package puni
  :init (puni-global-mode)
  :bind (:map puni-mode-map
              ("C-w" . nil) ; for whole-line-or-region-kill-region
              ("H-r" . puni-raise)
              ("H-s" . puni-squeeze)
              ("H-k" . kill-paragraph)
              ("H-h" . backward-kill-paragraph)
              ("M-r" . puni-backward-kill-word)
              ("C-S-h" . puni-backward-kill-word)
              ("C-H-i" . puni-slurp-forward)
              ("C-H-n" . puni-barf-forward)
              ("M-H-n" . puni-slurp-backward)
              ("M-H-i" . puni-barf-backward)
              ("C-M-S-h" . backward-kill-sexp))
  :custom
  (puni-squeeze-flash nil) ; don't blink or flash, I find it distracting
  (puni-splash-flash nil) ; don't blink or flash, I find it distracting
  (puni-confirm-when-delete-unbalanced-active-region nil)) ; don't warn me, I know what I am doing

;; Highlight nested parentheses, brackets, and braces according to their depth.
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Quite handy to jump to recent files. Made better by `consult-recent-file'.
(use-package recentf
  :ensure nil
  :init (recentf-mode)
  :custom
  (recentf-max-saved-items 200)
  (recentf-max-menu-items 25)
  (recentf-auto-cleanup 'never)
  :config (run-at-time nil (* 5 60) #'recentf-save-list)) ; Save recentf list every 5 minutes

;; Save history across sessions including kill-ring!
(use-package savehist
  :ensure nil
  :init (savehist-mode)
  :custom
  (history-length 2000)
  (history-delete-duplicates t)
  (savehist-additional-variables '(kill-ring search-ring regexp-search-ring)))

;; Undo tree
(use-package vundo
  :bind ("C-x u" . vundo) ("C-?" . undo-redo)
  :custom (undo-limit (* 16 1024 1024)))

;; If there is no active region, kill/delete/copy the current line.
(use-package whole-line-or-region
  :init (whole-line-or-region-global-mode))

;; Snippets for quick text insertion.
(use-package yasnippet
  :init (yas-global-mode)
  :custom (yas-snippet-dirs (list (concat user-emacs-directory "snippets"))))

;; Some more packages
(use-package achievements :init (achievements-mode))
(use-package sudo-edit)
(use-package tree-sitter)
(use-package tree-sitter-langs)
(use-package wordel)

(provide 'knot-packages)

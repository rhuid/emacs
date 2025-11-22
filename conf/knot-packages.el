;;; knot-packages.el --- Tools, tools, tools! -*- lexical-binding: t; -*-

;; Sail through the visible screen at the speed of thought.
(use-package avy
  :bind (("C-," . avy-goto-char-timer)
         :map isearch-mode-map
         ("C-," . avy-isearch))
  :custom
  (avy-keys '(?t ?n ?s ?e ?r ?i ?a ?o))
  (avy-timeout-seconds 0.2))

;; Some modes look cleaner without it.
(use-package hide-mode-line
  :hook ((dired-mode org-mode eshell-mode) . hide-mode-line-mode))

;; Let Emacs whisper the rest of your words; completion for those who prefer serendipity over precision.
(use-package hippie-exp
  :bind ("M-/" . hippie-expand)
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

;; Type freely; Jinx has your back. A silent guardian of your spelling.
;; Requires enchant and dictionary backend (I use `hunspell-en_us').
(use-package jinx
  :init (global-jinx-mode)
  :bind ("C-*" . jinx-correct) ("C-M-*" . jinx-correct-word)
  :custom (jinx-languages "en_US-large"))

;; Git, without ever leaving home and without touching the terminal.
(use-package magit
  :bind ("C-x g" .  magit-status)
  (:map magit-mode-map ("." . rh/commit) ("," . rh/amend))
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (defun rh/commit ()
    "Prompt for a commit message in minibuffer and commit immediately."
    (interactive)
    (let ((msg (read-string "Commit message: ")))
      (magit-commit-create `("-m" ,msg))))
  (defun rh/amend ()
    "Quickly amend last commit with a new message via minibuffer."
    (interactive)
    (let ((msg (read-string "Amend message: ")))
      (magit-commit-create `("--amend" "-m" ,msg)))))

;; Edit everywhere at once; parallel editing.
(use-package multiple-cursors
  :bind (("C-M-S-a" . mc/mark-all-like-this-dwim)
         ("M-S-RET" . mc/edit-lines)                                                          ; `C-M-S-m'
         ("C-M-S-n" . mc/mark-next-like-this)
         ("C-M-S-p" . mc/mark-previous-like-this)
         ("C-M->"   . mc/skip-to-next-like-this)
         ("C-M-<"   . mc/skip-to-previous-like-this))
  :custom (mc/always-run-for-all t))

;; Move where I mean.
(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line-or-comment)
         ("C-e" . mwim-end-of-code-or-line)))

;; Structural editing: lightweight, and language-agnostic
(use-package puni
  :init (puni-global-mode)
  :bind (:map puni-mode-map
              ("C-w" . nil)                               ; for my own `rh/kill-region-dwim'
              ("M-o" . rh/puni-rewrap-sexp)
              ("M-K" . kill-paragraph)
              ("M-H" . backward-kill-paragraph)
              ("C-M-f" . puni-forward-sexp-or-up-list)
              ("C-M-b" . puni-backward-sexp-or-up-list)
              ("C-S-h" . puni-backward-kill-word)
              ("C-M-r" . puni-raise)
              ("C-M-s" . puni-squeeze)
              ("C-H-i" . puni-slurp-forward)
              ("C-H-n" . puni-barf-forward)
              ("M-H-n" . puni-slurp-backward)
              ("M-H-i" . puni-barf-backward)
              ("C-M-S-s" . puni-split)
              ("<backspace>" . puni-force-delete))
  :custom
  (puni-squeeze-flash nil)
  (puni-blink-for-sexp-manipulating nil)
  (puni-confirm-when-delete-unbalanced-active-region nil)
  :config
  (defun rh/puni-rewrap-sexp ()
    "Rewrap the current sexp."
    (interactive)
    (let ((delimiter (read-char "Opening delimiter: ")))
      (save-excursion
        (backward-up-list 1 t t)
        (mark-sexp 1 nil)
        (when (use-region-p)
          (let ((beg (region-beginning))
                (end (region-end)))
            (cond
             ((= delimiter ?\() (puni-wrap-round 1))
             ((= delimiter ?\[) (puni-wrap-square 1))
             ((= delimiter ?\{) (puni-wrap-curly 1))
             ((= delimiter ?\<) (puni-wrap-angle 1))
             ((= delimiter ?\") (puni--wrap-region beg end "\"" "\""))
             ((= delimiter ?\') (puni--wrap-region beg end "\'" "\'"))
             (t (deactivate-mark)
                (error "Invalid delimiter entered."))))
          (delete-pair 1))))))

;; Let's feel a bit more spacious.
(use-package spacious-padding
  :init (spacious-padding-mode)
  :custom (spacious-padding-widths '( :internal-border-width 12 :mode-line-width 3 )))

;; Like a presentation mode, much more readable and pleasant to the eyes.
(use-package visual-fill-column
  :disabled t
  :bind ("C-H-SPC" . visual-fill-column-mode)
  :hook ((org-mode text-mode magit-status-mode emacs-lisp-mode eshell-mode)
         . visual-fill-column-mode)
  :custom
  (visual-fill-column-width 130)
  (visual-fill-column-center-text t))

;; Undo tree: trace your edits as a living tree.
(use-package vundo
  :bind ("C-x u" . vundo) ("C-?" . undo-redo)
  :custom (undo-limit (* 16 1024 1024)))

;; Some more packages
(use-package expand-region :bind ("C->" . er/expand-region))                 ; select and expand regions by semantic units
(use-package move-text :init (move-text-default-bindings))                   ; move line or region vertically with ease
(use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode)) ; highlight nested parentheses cleanly
(use-package rainbow-mode :hook (prog-mode . rainbow-mode))                  ; colorize stings that represent colors
(use-package sudo-edit)
(use-package tree-sitter)
(use-package tree-sitter-langs)
(use-package yasnippet :init (yas-global-mode))                              ; expand templates

(provide 'knot-packages)

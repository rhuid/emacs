;;; knot-packages.el --- Tools, tools, tools! -*- lexical-binding: t; -*-

;; Let Emacs finish your phrases. Because typing is hard work.
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

;; Sail through the visible screen at the speed of thought.
(use-package avy
  :bind (("C-," . avy-goto-char-timer)
         :map isearch-mode-map
         ("C-," . avy-isearch))
  :custom
  (avy-keys '(?t ?n ?s ?e ?r ?i ?a ?o))
  (avy-timeout-seconds 0.2))

;; Type comfortably --- `eldoc' watches over you; a whisper of documentation as you type.
(use-package eldoc
  :ensure nil
  :init (global-eldoc-mode)
  :config (setq eldoc-idle-delay 0.2))

;; Electric pairs: Auto-insert the closing delimiter.
(use-package elec-pair
  :init (electric-pair-mode)
  :hook (org-mode . rh/org-electric-pairs)
  :custom (electric-pair-pairs '((?\(.?\)) (?\{.?\}) (?\[.?\]) (?\".?\") (?\<.?\>)))
  :config
  (defun rh/org-electric-pairs ()
    "Org pairs for electric-pair-mode."
    (setq-local electric-pair-pairs (append '((?_.?_) (?~.?~))))))

;; Select and expand regions by semantic units.
(use-package expand-region
  :bind ("C->" . er/expand-region))

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

;; Search as you type, and watch your buffer follow.
(use-package isearch
  :ensure nil
  :custom
  (isearch-allow-scroll 'unlimited)                                          ; scroll as much as you please
  (isearch-lazy-count t)                                                     ; show number of matches in the mode-line
  (isearch-repeat-on-direction-change t)                                     ; allow switching direction
  (search-default-mode 'char-fold-to-regexp)                                 ; match accented letters too
  (search-whitespace-regexp ".*?"))                                          ; type "t n" to match "teleportation"

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

;; Fold and bloom your buffer's hierarchy.
(use-package outline
  :hook ((prog-mode text-mode) . outline-minor-mode)
  :init (setq outline-minor-mode-prefix (kbd "C-c o")))

;; Move where I mean.
(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line-or-comment)
         ("C-e" . mwim-end-of-code-or-line)))

;; Highlight matching parentheses, braces and brackets.
(use-package paren
  :init (show-paren-mode)
  :custom (show-paren-delay 0))

;; Navigate your project with ease; lightweight and comes built-in.
(use-package project
  :custom (project-switch-commands
           '((magit-project-status "Magit"     ?m)
             (project-find-file    "Find file" ?f)
             (project-dired        "Dired"     ?d)
             (project-eshell       "Eshell"    ?e))))

;; Structural editing: lightweight, and language-agnostic
(use-package puni
  :init (puni-global-mode)
  :bind (:map puni-mode-map
              ("C-w" . nil)                                ; taken by whole-line-or-region-kill-region
              ("M-o" . rh/puni-rewrap-sexp)
              ("M-K" . kill-paragraph)
              ("M-H" . backward-kill-paragraph)
              ("C-M-f" . puni-forward-sexp-or-up-list)
              ("C-M-b" . puni-backward-sexp-or-up-list)
              ("C-S-h" . puni-backward-kill-word)
              ("C-S-i" . puni-backward-kill-word)
              ("C-M-r" . puni-raise)
              ("C-M-s" . puni-squeeze)
              ("C-H-i" . puni-slurp-forward)
              ("C-H-n" . puni-barf-forward)
              ("M-H-n" . puni-slurp-backward)
              ("M-H-i" . puni-barf-backward)
              ("C-M-c p" . puni-split)
              ("C-M-c s" . puni-splice))
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

;; If there is no active region, kill/copy the current line
(use-package whole-line-or-region
  :init (whole-line-or-region-global-mode))

;; Turn a few keystrokes into full templates.
(use-package yasnippet
  :init (yas-global-mode)
  :custom (yas-snippet-dirs (list (concat user-emacs-directory "snippets"))))

;; Some more packages
(use-package move-text :init (move-text-default-bindings))                     ; move line or region up and down with ease
(use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))   ; highlight nested parentheses cleanly
(use-package rainbow-mode :hook (prog-mode . rainbow-mode))                    ; colorize stings that represent colors
(use-package sudo-edit)
(use-package tree-sitter)
(use-package tree-sitter-langs)

(provide 'knot-packages)

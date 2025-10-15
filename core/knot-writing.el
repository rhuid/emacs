;;; knot-writing.el --- Writing tools including typesetting -*- lexical-binding: t; -*-

;; Basically Org, LaTeX, Typst, and Markdown

(use-package org
  :ensure nil
  :defer 2
  :hook (org-mode . (lambda () (display-line-numbers-mode -1)))
  :custom
  (org-hide-leading-stars t)
  (org-log-done 'time)
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-special-ctrl-a/e t)
  (org-pretty-entities t)
  (org-catch-invisible-edits 'show-and-error)
  (org-insert-heading-respect-content t)
  (org-ellipsis "…")
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-agenda-tags-column 0)
  (org-table-auto-align t)
  (org-startup-folded 'content)
  (org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)))
  (org-directory "~/org")
  (org-todo-keywords
   '((sequence "TODO" "WAITING" "IN-PROGRESS" "|" "DONE" "CANCELLED")))
  :config
  (add-to-list 'org-export-backends 'md)
  ;; Set faces for the headings
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.7 :weight bold :foreground "#50fa7b"))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.5 :weight bold :foreground "#8be9fd"))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.3 :weight bold :foreground "#ff79c6"))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.1 :weight bold :foreground "#f1fa8c")))))
  :bind (:map org-mode-map
              ("C-," . nil)
              ("C-'" . nil)
              ("C-j" . rh/join-line)
              ("C-S-o" . org-shifttab)))

(use-package org-modern
  :after org
  :init (global-org-modern-mode)
  :custom (org-modern-star t))

(use-package ox-reveal
  :after org
  :init (load-library "ox-reveal")
  :config
  (setq org-re-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
  (add-to-list 'org-export-backends 'reveal))

(use-package org-agenda
  :after org
  :ensure nil
  :commands (org-agenda org-todo-list))

(use-package latex
  :ensure auctex
  :hook
  (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . TeX-fold-mode)
  (LaTeX-mode . turn-on-reftex)
  (LaTeX-mode . rh/provide-keywords-math-face)
  (post-command . rh/toggle-latex-abbrev)
  :bind (:map LaTeX-mode-map ("C-c C-u" . rh/tex-fold-buffer))
  :config
  (setq TeX-auto-save t
        TeX-parse-self t)
  (setq TeX-view-program-selection '((output-pdf "Evince") (output-html "firefox")))
  (setq TeX-view-program-list '(("Evince" "flatpak run org.gnome.Evince --page-index=%(outpage) %o")))

  (defun rh/toggle-latex-abbrev ()
    "Disable abbrevs inside math mode in Latex."
    (if (and (derived-mode-p 'LaTeX-mode) (texmathp))
        (abbrev-mode -1)
      (abbrev-mode 1)))

  (defun rh/tex-fold-buffer (&optional prefix)
    "Fold the buffer but clears the fold with the universal argument."
    (interactive "P")
    (if prefix
        (TeX-fold-clearout-buffer)
      (TeX-fold-buffer)))

  (defface rh/math-delimiter-face
    '((t (:inherit font-lock-comment-face :weight thin :slant normal)))
    "Face used for math delimiters in Latex."
    :group 'latex)

  (defun rh/provide-keywords-math-face ()
    "Provide keywords that have `rh/math-delimiter-face'."
    (font-lock-add-keywords
     nil
     '(;; Math delimiters
       ("\\(\\\\[][()]\\|\\$\\)" 0 'rh/math-delimiter-face t)
       ))))

(use-package auctex-latexmk
  :after auctex
  :config (auctex-latexmk-setup))

(use-package cdlatex
  :hook
  (LaTeX-mode . turn-on-cdlatex)
  (org-mode . turn-on-org-cdlatex)
  :bind (:map cdlatex-mode-map ("TAB" . rh/latex-tab-action))
  :custom (cdlatex-paired-parens "$([{")
  :config
  (defun rh/latex-tab-action ()
    "Try `yasnippet' first, then fall back to `CDLaTeX'."
    (interactive)
    (unless (and (bound-and-true-p yas-minor-mode) (yas-expand))
      (cdlatex-tab)))

  (setq cdlatex-env-alist
        '(("axiom"        "\\begin{axiom}\nAUTOLABEL\n?\n\\end{axiom}\n" nil)
          ("defintion"    "\\begin{definition}\nAUTOLABEL\n?\n\\end{definition}\n" nil)
          ("lemma"        "\\begin{lemma}\nAUTOLABEL\n?\n\\end{lemma}\n" nil)
          ("proposition"  "\\begin{proposition}\nAUTOLABEL\n?\n\\end{proposition}\n" nil)
          ("theorem"      "\\begin{theorem}\nAUTOLABEL\n?\n\\end{theorem}\n" nil)
          ("corollary"    "\\begin{corollary}\nAUTOLABEL\n?\n\\end{corollary}\n" nil)))
  (setq cdlatex-command-alist
        '(("ax"   "Insert axiom env"       "" cdlatex-environment ("axiom") t nil)
          ("def"  "Insert definition env"  "" cdlatex-environment ("definition") t nil)
          ("lem"  "Insert lemma env"       "" cdlatex-environment ("lemma") t nil)
          ("prop" "Insert proposition env" "" cdlatex-environment ("proposition") t nil)
          ("th"   "Insert theorem env"     "" cdlatex-environment ("theorem") t nil)
          ("cor"  "Insert corollary env"   "" cdlatex-environment ("corollary") t nil)
          ("pr"   "Insert proof env"       "" cdlatex-environment ("proof") t nil)
          ("dp"   "Insert displaymath env" "" cdlatex-environment ("displaymath") t nil))))

(use-package latex-preview-pane
  :bind ("C-c C-p C-l" . latex-preview-pane-mode))

(use-package typst-ts-mode
  :hook (typst-ts-mode . typst-ts-watch-mode))

(provide 'knot-writing)

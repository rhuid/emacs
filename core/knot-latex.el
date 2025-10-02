;;; knot-latex.el --- For good old Latex. -*- lexical-binding: t; -*-

(use-package latex
  :ensure auctex
  :hook
  (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . TeX-fold-mode)
  (LaTeX-mode . turn-on-reftex)
  (LaTeX-mode . rh/provide-keywords-math-face)
  (post-command . rh/toggle-latex-abbrev)
  :bind (:map LaTeX-mode-map
              ("C-c C-u" . rh/tex-fold-buffer))
  :config
  (setq TeX-auto-save t
        TeX-parse-self t)
  (setq TeX-view-program-selection
        '((output-pdf "Evince")
          (output-html "firefox")))
  (setq TeX-view-program-list
        '(("Evince" "flatpak run org.gnome.Evince --page-index=%(outpage) %o")))

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
  :bind (:map cdlatex-mode-map
              ("TAB" . rh/latex-tab-action))
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

(provide 'knot-latex)

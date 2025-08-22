;;; knot-latex.el --- For all things LaTeX -*- lexical-binding: t; -*-

(use-package auctex
  :mode (( "\\.tex\\'" . LaTeX-mode)
	       ( "\\.cls\\'" . LaTeX-mode)
	       ( "\\.sty\\'" . LaTeX-mode))
  :hook ((post-command . rh/toggle-latex-abbrev)
         (LaTeX-mode   . rh/latex-add-math-abbrevs))
  :config
  (setq TeX-view-program-selection '((output-pdf "Sioyek")))
  ;; (setq TeX-view-program-list      '(("Sioyek" "sioyek --reuse-instance %o")))

  (defun rh/toggle-latex-abbrev ()
    "Enable abbrev only outside math mode in LaTeX."
    (if (and (derived-mode-p 'LaTeX-mode)
             (texmathp))
        (abbrev-mode -1)
      (abbrev-mode 1)))

  ;; Defining abbreviations for LaTeX here
  (defun rh/latex-add-math-abbrevs ()
    "Define abbrevs so single letters expand to math mode, e.g. x → $x$."
    (define-abbrev-table 'LaTeX-mode-abbrev-table ()) ;; ensure table exists
    (dolist (ch (append (number-sequence ?b ?z)
                        (number-sequence ?0 ?9)))
      (let* ((s (char-to-string ch))
             (expansion (format "$%s$" s)))
        (define-abbrev LaTeX-mode-abbrev-table s expansion nil :count 0)))))

(use-package auctex-latexmk
  :after auctex
  :config
  (auctex-latexmk-setup))

(use-package cdlatex
  :after auctex
  :hook ((LaTeX-mode . turn-on-cdlatex)
         (org-mode   . turn-on-org-cdlatex))
  :config
  (setq cdlatex-env-alist
	      '(("axiom"        "\\begin{axiom}\nAUTOLABEL\n?\n\\end{axiom}\n" nil)                 ; AUTOLABEL
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
	        ("pr"   "Insert proof env"       "" cdlatex-environment ("proof") t nil))))



(use-package latex-preview-pane)

(provide 'knot-latex)

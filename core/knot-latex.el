;;; knot-latex.el --- For all things LaTeX -*- lexical-binding: t; -*-

;;;; Auctex is the definitive LaTeX experience on Emacs

(use-package auctex
  :mode (( "\\.tex\\'" . LaTeX-mode)
	       ( "\\.cls\\'" . LaTeX-mode)
	       ( "\\.sty\\'" . LaTeX-mode))
  :hook
  (post-command . rh/toggle-latex-abbrev)
  ;; (LaTeX-mode   . rh/remap-local-keys)
  ;; (LaTeX-mode   . yas-minor-mode)
  (LaTeX-mode   . rh/setup-math-completion)

  :config
  (setq TeX-view-program-selection '((output-pdf "Sioyek")))
  ;; (setq TeX-view-program-list      '(("Sioyek" "sioyek --reuse-instance %o")))

  ;;; Abbrevs should work only outside math mode

  (defun rh/toggle-latex-abbrev ()
    "Enable abbrev only outside math mode in LaTeX."
    (if (and (derived-mode-p 'LaTeX-mode)
             (texmathp))
        (abbrev-mode -1)
      (abbrev-mode 1)))

  ;; Make math delimiters less visible (stolen from https://gitlab.com/slotThe/dotfiles/-/blob/master/emacs/lisp/hopf-latex.el)
  (defvar rh/latex-math-face (make-face 'rh/latex-math-face))
  (set-face-attribute 'rh/latex-math-face
                      nil
                      :inherit 'font-lock-comment-face :weight 'thin :slant 'normal)
  (font-lock-add-keywords
   'LaTeX-mode '(("\\\\[]()[]\\|\\$" 0 rh/latex-math-face t)))

  ;;; Math completions for corfu (wont work for now)

  ;; Create the math terms file if it doesn't exist
  (setq rh/math-dict-file (concat user-emacs-directory "math-dict"))

  (defun rh/load-math-completions ()
    "Load math completion terms from file."
    (when (file-exists-p rh/math-completion-file)
      (with-temp-buffer
        (insert-file-contents rh/math-dict-file)
        (split-string (buffer-string) "\n" t))))

  (defun rh/cape-math ()
    "Provide completion candidates from math terms."
    (let ((completions (rh/load-math-completions)))
      (when completions
        (cape-dict completions))))

  (defun rh/setup-math-completion ()
    "Setup math completion for the current buffer."
    (setq-local completion-at-point-functions
                (append completion-at-point-functions
                        '(rh/cape-math)))))

(use-package auctex-latexmk
  :after auctex
  :config
  (auctex-latexmk-setup))

;;;; CDLatex for quick typing (it's all about speed)

(use-package cdlatex
  :after auctex
  :hook
  (LaTeX-mode . turn-on-cdlatex)
  (org-mode   . turn-on-org-cdlatex)
  :bind (:map cdlatex-mode-map
              ("TAB" . rh/latex-tab-action))
  :custom
  (cdlatex-paired-parens "$([{")
  :config

  ;;; Use CDLaTeX along with yasnippets without conflict

  (defun rh/latex-tab-action ()
    "Try yasnippet first, then fall back to CDLaTeX."
    (interactive)
    (if (and (bound-and-true-p yas-minor-mode)
             (yas-expand)) t
      (cdlatex-tab)))

  ;;; Quickly drop latex environments

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

(use-package latex-preview-pane)

(provide 'knot-latex)

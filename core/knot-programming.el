;;; knot-programming.el --- Things related to writing source code -*- lexical-binding: t; -*-

(use-package prog-mode
  :ensure nil
  :config
  (global-font-lock-mode 1)
  (setq-default indent-tabs-mode nil
		            tab-width 2)
  (setq standard-indent 2))

(use-package yasnippet
  :demand t
  :config
  (yas-global-mode)
  :custom
  (setq yas-snippet-dirs (list (concat user-emacs-directory "snippets"))))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (setq lsp-completion-provider :none)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-signature-auto-activate nil)
  (lsp-log-io nil))

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable t))

(use-package haskell-mode
  :commands haskell-mode
  :mode "\\.hs\\'" )

(use-package lean4-mode
  :vc (:url "https://github.com/leanprover-community/lean4-mode.git" :rev :last-release)
  :commands lean4-mode
  :mode "\\.lean\\'"

  :bind (:map lean4-mode-map
              ("<f5>" . rh/lean4-minimal-mode-toggle)
              ("<f7>" . lean4-toggle-info)
              ("F"    . flycheck-mode))

  :hook ((lean4-mode . lsp-mode)
	       (lean4-mode . rh/lean-highlight-types)
	       (lean4-mode . rh/lean-highlight-values)
	       (lean4-mode . rh/lean-highlight-typeclasses)
	       (lean4-mode . rh/outline-lean)
	       (lean4-mode . (lambda () (require 'rh-lean))))

  :config
  (defvar rh/lean4-minimal-mode-enabled nil
    "If non-nil, Lean 4 is in minimal UI mode.")

  (defun rh/lean4-minimal-mode-toggle ()
    "Toggle between full Lean UI and minimal UI. Minimal UI keeps goal feedback and inline evaluation"
    (interactive)
    (if rh/lean4-minimal-mode-enabled
        ;; Switch to full UI mode
        (progn
          (setq rh/lean4-minimal-mode-enabled nil)
          (flycheck-mode +1)
          (setq-local flycheck-highlighting-mode 'symbols) ; underline symbols
          (lsp-ui-sideline-toggle-symbols-info) ; (the sideline pop up info of variables and stuff)
          (message "Full UI mode."))
      ;; Switch to minimal UI mode
      (setq rh/lean4-minimal-mode-enabled t)
      (flycheck-mode -1)
      (setq-local flycheck-highlighting-mode nil) ; never underline (it's annoying)
      (company-mode -1)
      (lsp-ui-sideline-toggle-symbols-info) ; turning it off
      (message "Minimal UI mode.")))

  (defun rh/outline-lean ()
    "Set outline regex for top-level declarations in Lean."
    (setq-local outline-regexp
		            (rx line-start
		                (* space)
		                (or  "structure" "inductive" "class"
			                   "theorem" "axiom" "lemma" "def"
		                     "instance" "example" "opaque"
			                   "namespace")))
    (outline-hide-body)))

(use-package sh-script
  :ensure nil
  :mode ("\\.sh\\'" . sh-mode)
  :hook ((sh-mode . rh/sh-tab-hook)
	       (sh-mode . rh/sh-highlight-custom-keywords)
	       (sh-mode . (lambda ()
		                  (require 'rh-shell)))))

(use-package outline
  :demand t
  :hook ((prog-mode . outline-minor-mode)
         (text-mode . outline-minor-mode)
         (outline-minor-mode . outline-show-all)
	       (outline-minor-mode . outline-hide-body))
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

(use-package lisp-mode
  :ensure nil
  :mode ("\\.el\\'" . emacs-lisp-mode)
  :hook ((emacs-lisp-mode . eldoc-mode)
	       (emacs-lisp-mode . rh/elisp-tab-hook)
	       (emacs-lisp-mode . rh/elisp-highlight-custom-keywords)
	       (emacs-lisp-mode . rh/outline-elisp)
	       (lisp-interaction-mode . rh/elisp-tab-hook)
	       (lisp-interaction-mode . rh/elisp-highlight-custom-keywords)
	       (emacs-lisp-mode . (lambda () (require 'rh-elisp))))
  :config
  (defun rh/outline-elisp ()
    "Set outline regex for top-level declarations in Emacs Lisp."
    (setq-local outline-regexp
		            (rx line-start
                    (* space)
                    "("
                    (or  "use-package" "require" "provide" "defun"
			                   "with-eval-after-load" "setq" "defvar"
			                   "add-to-list" "add-hook")))
    (outline-hide-body)))

(use-package rust-mode
  :mode "\\.rs\\'"
  :hook ((rust-mode . outline-minor-mode)
	       (rust-mode . rh/outline-rust)
	       (rust-mode . (lambda () (require 'rh-rust))))
  :config
  (setq rust-format-on-save t)

  (defun rh/outline-rust ()
    (setq-local outline-regexp
		            (rx line-start (* space)
                    (or "fn" "pub" "struct" "enum" "impl")))
    (outline-hide-body)))

(use-package flycheck-rust
  :after rust
  (rust-mode . flycheck-rust-setup))

(use-package nix-mode
  :mode "\\.nix\\'"
  :config
  (setq nix-indent-function 'nix-indent-line))

(use-package julia-mode
  :mode "\\.jl\\'")

(use-package kbd-mode
  :vc (:url "https://github.com/kmonad/kbd-mode" :rev :newest)
  :mode "\\.kbd\\'")

(use-package systemd
  :mode (("\\.service\\'" . systemd-mode)
         ("\\.timer\\'"   . systemd-mode)
         ("\\.mount\\'"   . systemd-mode)
         ("\\.target\\'"  . systemd-mode)
         ("\\.conf\\'"    . conf-unix-mode)
	       ("\\.ini\\'"     . conf-unix-mode)))

(use-package csv-mode
  :mode ("\\.csv\\'" . csv-mode)
  :hook (csv-mode . csv-align-mode))

;;;; Eglot (in-built)

;; (use-package emacs :ensure nil
;;   :hook ((sh-mode . eglot-ensure)
;; 	 (rust-mode . eglot-ensure))
;;   )

(add-hook 'sh-mode-hook #'eglot-ensure)

;; markdown live preview
(use-package flymd)

(provide 'knot-programming)

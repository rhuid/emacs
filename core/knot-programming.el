;;; knot-programming.el --- All things related to writing source code -*- lexical-binding: t; -*-

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
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

(use-package company
  :commands company-mode
  :hook (lean4-mode . company-mode)
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
	company-insertion-on-trigger nil
	company-insertion-triggers nil)

  (define-key company-active-map (kbd "TAB") nil)
  (define-key company-active-map (kbd "<tab>") nil)
  (define-key company-active-map (kbd "RET") #'company-abort)
  (define-key company-active-map (kbd "<ret>") #'company-abort)

  (define-key company-active-map (kbd "C-l") #'company-complete-selection)
  (define-key company-mode-map (kbd "C-l") #'company-complete))

(use-package haskell-mode
  :commands haskell-mode
  :mode "\\.hs\\'" )

(use-package haskell-tng-mode
  ;; :mode "\\.hs\\"
  )

(use-package lean4-mode
  :vc (:url "https://github.com/leanprover-community/lean4-mode.git" :rev :last-release)
  :commands lean4-mode
  :mode "\\.lean\\'"
  :hook ((lean4-mode . lsp-mode)
	 (lean4-mode . rh/lean4-tab-hook)
	 (lean4-mode . rh/lean-highlight-types)
	 (lean4-mode . rh/lean-highlight-values)
	 (lean4-mode . rh/lean-highlight-typeclasses)
	 (lean4-mode . rh/outline-lean)
	 (lean4-mode . rh/lean4-corfu-off-company-on)
	 (lean4-mode . (lambda ()
			 (require 'lean4-mode)
			 (require 'rh-lean))))
  :init
  (defun rh/lean4-corfu-off-company-on ()
    "Disable corfu and enable company only in Lean4 buffers."
    (interactive)
    (corfu-mode -1)
    (company-mode +1))

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
	 (emacs-lisp-mode . (lambda ()
			      (require 'rh-elisp))))
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
	 (rust-mode . flycheck-rust-setup)
	 (rust-mode . (lambda ()
			(require 'rh-rust))))
  :config
  (setq rust-format-on-save t)

  ;; Currently not working, will check
  (defun rh/outline-rust ()
    (setq-local outline-regexp
		(rx line-start (* space)
                    (or "fn" "pub" "struct" "enum" "impl")))
    (outline-hide-body)))

(use-package flycheck-rust
  :after rust)

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

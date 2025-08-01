;;; knot-programming.el --- All things related to writing source code -*- lexical-binding: t; -*-

(use-package lsp-mode :ensure t :defer t
  :commands (lsp lsp-deferred)
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-signature-auto-activate nil)
  (lsp-log-io nil))

(use-package lsp-ui :ensure t :defer t :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable t))

(use-package company :ensure t :defer t
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

;; Haskell
(use-package haskell-mode :ensure t :defer t
  :mode "\\.hs\\'"
  )

(use-package haskell-tng-mode :ensure t :defer t
  ;; :mode "\\.hs\\"
  )

(use-package lean4-mode :ensure nil :defer t
  :commands lean4-mode
  :load-path "~/.emacs.d/lean4-mode"

  ;; :ensure (lean4-mode :type git :host github
  ;;                       :repo "leanprover-community/lean4-mode"
  ;;                       :files ("*.el" "data"))

  :mode "\\.lean\\'"
  :hook ((lean4-mode . lsp-mode)
	 (lean4-mode . rh/lean4-tab-hook)
         (lean4-mode . rh/lean-highlight-types)
         (lean4-mode . rh/lean-highlight-values)
         (lean4-mode . rh/lean-highlight-typeclasses)
	 (lean4-mode . rh/lean4-corfu-off-company-on)
	 (lean4-mode . (lambda ()
                         (require 'lean4-mode)
                         (require 'rh-lean))))
  :config
  (defun rh/lean4-corfu-off-company-on ()
    "Disable corfu and enable company only in Lean4 buffers."
    (interactive)
    (corfu-mode -1)
    (company-mode +1))
  )

(use-package sh-script :ensure nil :defer t
  :mode ("\\.sh\\'" . sh-mode)
  :hook ((sh-mode . rh/sh-tab-hook)
	 (sh-mode . rh/sh-highlight-custom-keywords)
	 (sh-mode . (lambda ()
		      (require 'rh-shell))))
  )

(use-package outline :ensure t :demand t
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
      positions))
  )

(use-package lisp-mode :ensure nil :defer t
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
                    (or ";;;" "use-package" "require" "provide" "defun"
			"add-to-list" "add-hook")))
    (outline-hide-body))
  )

(use-package rust-mode :ensure t :defer t
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
    (outline-hide-body))
  )

(use-package flycheck-rust :ensure t :defer t :after rust)

(use-package nix-mode :ensure t :defer t
  :mode "\\.nix\\'"
  :config
  (setq nix-indent-function 'nix-indent-line))

(use-package julia-mode :ensure t :defer t
  :mode "\\.jl\\'")

(use-package kbd-mode :defer t
  :ensure (kbd-mode :type git :host github :repo "kmonad/kbd-mode")
  :mode "\\.kbd\\'")

(use-package systemd :ensure t :defer t
  :mode (("\\.service\\'" . systemd-mode)
         ("\\.timer\\'"   . systemd-mode)
         ("\\.mount\\'"   . systemd-mode)
         ("\\.target\\'"  . systemd-mode)
	 ("\\.conf\\'"    . conf-unix-mode)
	 ("\\.ini\\'"     . conf-unix-mode)))

(use-package csv-mode :ensure t
  :mode ("\\.csv\\'" . csv-mode)
  :hook (csv-mode . csv-align-mode))

;;;; Eglot (in-built)

;; (use-package emacs :ensure nil
;;   :hook ((sh-mode . eglot-ensure)
;; 	 (rust-mode . eglot-ensure))
;;   )

(add-hook 'sh-mode-hook #'eglot-ensure)

;; markdown live preview
(use-package flymd :ensure t :defer t)

(provide 'knot-programming)

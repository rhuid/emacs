;;; knot-programming.el --- Things related to writing source code -*- lexical-binding: t; -*-

;;; `prog-mode'
;; For all programming modes
(use-package prog-mode
  :ensure nil
  :config
  (global-font-lock-mode 1)
  ;; Always use spaces, never tabs
  ;; By default, 1 TAB = 2 spaces
  (setq-default indent-tabs-mode nil
		            tab-width 2)
  ;; Default indentation is 2 spaces, but can be overridden by major modes
  (setq standard-indent 2))

;;; `electric-pair-mode`
;; Automatically insert matching delimiters (parentheses, quotes, braces, etc)
(use-package elec-pair
  :demand t
  :config (electric-pair-mode)
  :hook (org-mode . rh/org-electric-pairs)
  :custom (electric-pair-pairs '((?\(.?\)) (?\{.?\}) (?\[.?\])
                                 (?\".?\") (?\<.?\>)))
  :config
  (defun rh/org-electric-pairs ()
    "Org pairs for electric-pair-mode."
    (setq-local electric-pair-pairs (append '((?/.?/) (?_.?_) (?~.?~))))))

;;; `aggressive-indent'
;; Indent aggressively for Lisp and its derivatives
(use-package aggressive-indent
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
         (lisp-interaction-mode . aggressive-indent-mode)
         (lisp-mode . aggressive-indent-mode))
  :config
  (setq aggressive-indent-comments-too t))

;;; `rainbow-delimiters'
;; Different color for each pair of parenthesis
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;; `yasnippets'
;; Use them when abbrevs don't cut it
(use-package yasnippet
  :demand t
  :config
  (yas-global-mode)
  :custom
  (setq yas-snippet-dirs (list (concat user-emacs-directory "snippets"))))

;;; `lsp'
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (read-process-output-max (* 1024 1024))
  ;; Don't use company or any completion backend provider (important for Lean 4, as LSP always pulls in company)
  (lsp-completion-provider :none)
  ;; For better performance (delay after typing before requesting LSP)
  (lsp-idle-delay 0.6)
  (lsp-enable-folding nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-text-document-color nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-modeline-code-actions-mode nil)
  (lsp-modeline-diagnostics-mode nil)
  (lsp-signature-auto-activate nil)
  ;; For faster communication with LSP, disable logging
  (lsp-log-io nil))

;;; `lsp-ui'
;; Extra nice UI for LSP
(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-ignore-duplicate t)
  ;; Show inline diagnostics and evaluations (important for Lean 4)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-diagnostic-max-lines 3)
  ;; Disable doc/info of symbols and variables on the sideline
  (lsp-ui-sideline-show-hover nil)
  ;; Disable the pop-up doc over mouse hover
  (lsp-ui-doc-enable t))

;;; Syntax checking
(use-package flycheck
  :commands (flycheck-mode)
  :config
  (setq flycheck-display-errors-delay 0.2
        ;; Don't underline (it's mildly annoying)
        flycheck-highlighting-mode 'nil
        flycheck-indication-mode 'left-fringe))

(use-package haskell-mode
  :commands haskell-mode
  :mode "\\.hs\\'" )

;;; `lean4-mode'
;; The ultimate theorem prover and function programming language
(use-package lean4-mode
  :defer 1

  :vc (:url "https://github.com/leanprover-community/lean4-mode.git" :rev :last-release)
  :commands lean4-mode
  :mode "\\.lean\\'"

  :bind (:map lean4-mode-map
              ("<f5>" . rh/lean4-lsp-toggle)
              ("<f7>" . lean4-toggle-info))

  :hook
  (lean4-mode . rh/lean-highlight-types)
	(lean4-mode . rh/lean-highlight-values)
	(lean4-mode . rh/lean-highlight-typeclasses)
	(lean4-mode . rh/outline-lean)
	(lean4-mode . (lambda () (require 'rh-lean)))

  :config
  (defvar rh/lean4-lsp-enabled nil
    "If non-nil, Lean 4 is in minimal UI mode.")

  (defun rh/lean4-lsp-toggle ()
    "Toggle between LSP and minimal UI.
  LSP keeps goal feedback, inline evaluation and eldoc. Minimal UI has only syntax highlighting"
    (interactive)
    (if rh/lean4-lsp-enabled
        ;; Turn LSP off
        (progn
          (setq rh/lean4-lsp-enabled nil)
          (lsp-disconnect)
          (message "LSP off"))
      ;; Turn LSP on
      (setq rh/lean4-lsp-enabled t)
      (lsp)
      (message "LSP on")))

  (defun rh/outline-lean ()
    "Set outline regex for top-level declarations in Lean."
    (setq-local outline-regexp
		            (rx line-start
		                (* space)
		                (or  "structure" "inductive" "class"
			                   "theorem" "axiom" "lemma" "def"
		                     "instance" "example" "opaque"
			                   "namespace")))
    (outline-hide-body))

  ;; Make abbrevs case sensitive (important)
  (abbrev-table-put lean4-abbrev-table :case-fixed t))

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
  :after rust-mode
  :hook (rust-mode . flycheck-rust-setup))

(use-package nix-mode
  :mode "\\.nix\\'"
  :config
  (setq nix-indent-function 'nix-indent-line))

(use-package julia-mode
  :mode "\\.jl\\'")

;;; Mode for KMonad config files
(use-package kbd-mode
  :vc (:url "https://github.com/kmonad/kbd-mode" :rev :newest)
  :mode "\\.kbd\\'")

;;; Mode for systemd and other config files
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

;; Markdown live preview
(use-package flymd)

(provide 'knot-programming)

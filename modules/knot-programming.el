;;; knot-programming.el --- Things related to writing source code -*- lexical-binding: t; -*-

;; Geneal things about programming mode.
(use-package prog-mode
  :ensure nil
  :hook ((prog-mode . glyphless-display-mode)                                ; display all glyphless characters as boxes
         (after-save . executable-make-buffer-file-executable-if-script-p))  ; make a shell script executable upon saving
  :config
  (global-font-lock-mode)
  (setq-default indent-tabs-mode nil)                                        ; always use spaces, never tabs
  (setq-default tab-width 2)                                                 ; set default tab width to 2 spaces
  (setq standard-indent 2))                                                  ; set default indent to 2 spaces

;; Indent aggressively as you type.
(use-package aggressive-indent
  :hook ((emacs-lisp-mode lisp-interaction-mode) . aggressive-indent-mode)
  :config (setq aggressive-indent-comments-too t))

;; A necessary evil for my Lean 4 theorem proving... disabling extra features I don't use
(use-package lsp-mode
  :custom
  (read-process-output-max (* 2 1024 1024))
  (lsp-completion-provider :none)                                            ; to avoid interference from company-mode
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-text-document-color nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-signature-auto-activate nil))

;; Extra UI and documentation.
(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t)                                       ; inline diagnostics and evaluations
  (lsp-ui-sideline-show-hover nil)                                           ; disable doc/info of symbols on the sideline
  (lsp-ui-doc-enable t))                                                     ; enable pop up of documentation on mouse hover

(use-package flycheck
  :commands (flycheck-mode)
  :config
  (setq flycheck-display-errors-delay 0.2
        flycheck-highlighting-mode 'nil                                      ; don't underline
        flycheck-indication-mode 'left-fringe))

;; Lean 4: Prove theorems in Emacs!
;; (setq lean4-lsp-server-executable-name "lean")
(use-package lean4-mode
  :vc (:url "https://github.com/leanprover-community/lean4-mode.git")
  :bind (:map lean4-mode-map ("C-m" . electric-newline-and-maybe-indent))
  :hook (lean4-mode . lsp)
  :config (abbrev-table-put lean4-abbrev-table :case-fixed t))               ; case-sensitive abbrev expansion

;; (add-to-list 'load-path "~/.emacs.d/lean4-mode/")
;; (require 'lean4-mode)

(use-package flycheck-rust
  :after rust-mode
  :hook (rust-mode . flycheck-rust-setup))

(use-package haskell-mode)
(use-package julia-mode)
(use-package nix-mode)
(use-package rust-mode)
(use-package systemd)
(use-package csv-mode :hook (csv-mode . csv-align-mode))

(provide 'knot-programming)

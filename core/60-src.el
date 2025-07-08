;; Coding

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

;; (use-package company
;;   :hook (after-init . global-company-mode)
;;   :config
;;   (setq company-idle-delay 0.2
;;         company-minimum-prefix-length 2))

;; Haskell
(use-package haskell-mode
  :mode "\\.hs\\'"
  )

(use-package haskell-tng-mode
  ;; :mode "\\.hs\\'"
  )


;; Lean
(add-to-list 'load-path "~/.emacs.d/lean4-mode")
(require 'lean4-mode)
(add-to-list 'auto-mode-alist '("\\.lean\\'" . lean4-mode))

(require 'rh-lean)
(add-hook 'lean4-mode-hook #'rh/lean4-tab-hook)
(add-hook 'lean4-mode-hook #'rh/lean-highlight-types)
(add-hook 'lean4-mode-hook #'rh/lean-highlight-values)
(add-hook 'lean4-mode-hook #'rh/lean-highlight-typeclasses)

(require 'rh-shell)
(add-hook 'sh-mode-hook #'rh/sh-tab-hook)
(add-hook 'sh-mode-hook #'rh/sh-highlight-custom-keywords)

(require 'rh-elisp)
(add-hook 'emacs-lisp-mode-hook #'rh/elisp-tab-hook)
(add-hook 'emacs-lisp-mode-hook #'rh/elisp-highlight-custom-keywords)

(add-hook 'lisp-interaction-mode-hook #'rh/elisp-tab-hook)
(add-hook 'lisp-interaction-mode-hook #'rh/elisp-highlight-custom-keywords)

;; Rust
(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (setq rust-format-on-save t))

(use-package flycheck-rust
  :hook (rust-mode . flycheck-rust-setup))

(require 'rh-rust)

;; For .kbd kmonad files
(use-package kbd-mode
  :straight (kbd-mode :type git :host github :repo "kmonad/kbd-mode"))

(use-package nix-mode
  :straight t
  :mode "\\.nix\\'"
  :config
  (setq nix-indent-function 'nix-indent-line))

(use-package julia-mode
  :mode "\\.jl\\'")

(provide '60-src)

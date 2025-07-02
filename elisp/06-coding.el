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
  :hook (lsp-mode . lsp-ui-mode))

;; Lean
(add-to-list 'load-path "~/.emacs.d/lean4-mode")
(require 'lean4-mode)
(add-to-list 'auto-mode-alist '("\\.lean\\'" . lean4-mode))

(require 'rh-lean)

(defun rh/lean4-setup ()
  (abbrev-mode 1)
  (setq lean4-info-auto-open t)    ;; auto open goal/context panel...not working?
)

(add-hook 'lean4-mode-hook #'rh/lean4-setup)

(require 'rh-shell)
(require 'rh-elisp)

;; Rust
(use-package flycheck-rust
  :hook (rust-mode . flycheck-rust-setup))

(require 'rh-rust)

;; For .kbd kmonad files
(use-package kbd-mode
  :vc (:url "https://github.com/kmonad/kbd-mode" :rev :newest))

(provide '06-coding)

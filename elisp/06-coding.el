;; Coding

;; Lean
(add-to-list 'load-path "~/.emacs.d/lean4-mode")              ; lean4-mode is installed manually in this path
(require 'lean4-mode)
(add-to-list 'auto-mode-alist '("\\.lean\\'" . lean4-mode))   ; associate .lean files with lean4-mode

(defun custom/lean4-setup ()
   "Custom setup for Lean 4 editing."
   (abbrev-mode 1)                                             ; enable abbrev-mode
   (eglot-ensure)                                              ; enable LSP via eglot 
)

(add-hook 'lean4-mode-hook #'custom/lean4-setup)

(with-eval-after-load 'eglot                                  ; tell Eglot how to start Lean Lsp
  (add-to-list 'eglot-server-programs
               '(lean4-mode . ("lake" "serve"))))             ; invoke lake server in lean4-mode

(require 'rh-lean)
;;(require 'rh-faces)

(require 'rh-shell)
(require 'rh-elisp)

;; Rust
(use-package flycheck-rust
  :hook (rust-mode . flycheck-rust-setup))

(require 'rh-rust)

;; For .kbd kmonad files
(use-package kbd-mode
  :vc (:url "https://github.com/kmonad/kbd-mode" :rev :newest))

;; (setq lsp-log-io nil)                                         ; Suppress LSP warnings

(provide '06-coding)

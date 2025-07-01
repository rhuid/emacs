;; Coding

(require 'rh-custom-keywords)

;; Lean
(add-to-list 'load-path "~/.emacs.d/lean4-mode")              ; lean4-mode is installed manually in this path
(require 'lean4-mode)
(add-to-list 'auto-mode-alist '("\\.lean\\'" . lean4-mode))   ; associate .lean files with lean4-mode
(setq eglot-stay-out-of '(flymake))                           ; prevent eglot to prevent loading flymake syntax checking

(define-abbrev-table 'lean4-abbrev-table                      ; define abbrevs
  '(("ex" "example" nil 0)
    ("ax" "axiom" nil 0)))

(defun custom/lean4-setup ()
  "Custom setup for Lean 4 editing."
  (abbrev-mode 1)                                             ; enable abbrev-mode
  (eglot-ensure)                                              ; enable LSP via eglot 
  (flymake-mode -1)                                           ; disable flymake-mode
  (setq flymake-error-bitmap nil)
  (setq flymake-warning-bitmap nil)
  (setq flymake-note-bitmap nil))

(add-hook 'lean4-mode-hook #'custom/lean4-setup)

(with-eval-after-load 'eglot                                  ; tell Eglot how to start Lean Lsp
  (add-to-list 'eglot-server-programs
               '(lean4-mode . ("lake" "serve"))))             ; invoke lake server in lean4-mode

(require 'rh-lean)

;; Rust
(use-package flycheck-rust
  :hook (rust-mode . flycheck-rust-setup))

(require 'rh-rust)

;; For .kbd kmonad files
(use-package kbd-mode
  :vc (:url "https://github.com/kmonad/kbd-mode" :rev :newest))

(setq lsp-log-io nil)                                         ; Suppress LSP warnings

(provide '06-coding)

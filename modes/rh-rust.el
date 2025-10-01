;;; rh-rust.el --- description -*- lexical-binding: t; -*-

(defun rh/outline-rust ()
  (setq-local outline-regexp
              (rx line-start (* space)
                  (or "fn" "pub" "struct" "enum" "impl")))
  (outline-hide-body))

;; :hook (rust-mode . rh/outline-rust)

(require 'rh-snip)

(defvar rh/rust-snippet-alist
  '(("c" . "/* ? */?")                                     ; multi-line comment
    ("fn" . "fn ?(?) {\n  ?\n}")
    ("let" . "let ? = ?;")
    ("st" . "#[derive(Debug)]\nstruct ? {\n    ?\n}")
    ("impl" . "impl ? {\n    ?\n}")
    ("mt" . "match ? {\n    ? => ?,?\n}")
    ("a" . "? => ?,?")
    ;; ... add more
    ))

(defun rh/rust-tab-hook ()
  (local-set-key (kbd "TAB")
                 (lambda ()
                   (interactive)
                   (setq rh/snippet-placeholder-positions
                         (rh/jump-or-indent
                          rh/rust-snippet-alist
                          rh/snippet-placeholder-positions)))))

(add-hook 'rust-mode-hook #'rh/rust-tab-hook)

(provide 'rh-rust)

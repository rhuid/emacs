;;; rh-elisp.el --- description -*- lexical-binding: t; -*-

(defun rh/outline-elisp ()
  "Set outline regex for top-level declarations in Emacs Lisp."
  (setq-local outline-regexp
              (rx line-start (* space) "("
                  (or  "use-package" "require" "provide" "defun"
                       "with-eval-after-load" "setq" "defvar"
                       "add-to-list" "add-hook")))
  (outline-hide-body))

;; (emacs-lisp-mode . rh/outline-elisp)
;; (emacs-lisp-mode . (lambda () (require 'rh-elisp)))

(require 'rh-snip)
(require 'rh-faces)

(defvar rh/elisp-snippet-alist
  '(("req"  . "(require '?)")
    ("pro"  . "(provide '?)")
    ("ahk"  . "(add-hook '?-mode-hook #'?)")
    ("use"  . "(use-package ?)")
    ("cf"   . ":config")))

(defun rh/elisp-tab-hook ()
  "Setup Elisp snippet and placeholder support on TAB."
  (local-set-key (kbd "TAB")
                 (lambda ()
                   (interactive)
                   (setq rh/snippet-placeholder-positions
                         (rh/jump-or-indent
                          rh/elisp-snippet-alist
                          rh/snippet-placeholder-positions)))))

(provide 'rh-elisp)

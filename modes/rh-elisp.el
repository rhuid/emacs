(require 'rh-faces)

;; emacs-lisp-mode (for .el files such as this one)
(defun rh/elisp-highlight-custom-keywords ()
  "Highlight custom emacs lisp keywords in `emacs-lisp-mode`."
  (let ((rh/elisp-custom-keywords
         '("add-hook"
	   "remove-hook"
	   "add-to-list"
	   "global-set-key"
	   )))
    (font-lock-add-keywords
     nil
     `((,(concat "\\<" (regexp-opt rh/elisp-custom-keywords t) "\\>")
        . 'rh/custom-keyword-face)))))

(add-hook 'emacs-lisp-mode-hook #'rh/elisp-highlight-custom-keywords)
(add-hook 'lisp-interaction-mode-hook #'rh/elisp-highlight-custom-keywords)

(provide 'rh-elisp)

(require 'rh-snip)
(require 'rh-faces)

(defvar rh/elisp-snippet-alist
  '(("req"  . "(require '?)")
    ("pro"  . "(provide '?)")
    ("ahk"  . "(add-hook '?-mode-hook #'?)")
    ("use"  . "(use-package ?)")
    ("cf"   . ":config") 
    ;; ... add more when needed
    ))
 
(defun rh/elisp-tab-hook ()
  "Setup Elisp snippet and placeholder support on TAB."
  (local-set-key (kbd "TAB")
                 (lambda ()
                   (interactive)
                   (setq rh/snippet-placeholder-positions
                         (rh/jump-or-indent
                          rh/elisp-snippet-alist
                          rh/snippet-placeholder-positions)))))

(defun rh/elisp-highlight-custom-keywords ()
  "Highlight some custom emacs lisp words in `emacs-lisp-mode`."
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

(provide 'rh-elisp)

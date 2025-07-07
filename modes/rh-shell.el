(require 'rh-snip)
(require 'rh-faces)

(defvar rh/shell-snippet-alist
  '(("she"   . "#!/bin/bash")
    ("src"   . "source ~/scripts/lib/?")
   ;; ... add more when needed
    ))

(defun rh/sh-tab-hook ()
  "Setup Shell snippet and placeholder support on TAB."
  (local-set-key (kbd "TAB")
                 (lambda ()
                   (interactive)
                   (setq rh/snippet-placeholder-positions
                         (rh/jump-or-indent
                          rh/shell-snippet-alist
                          rh/snippet-placeholder-positions)))))

;; sh-mode (Shell)
(defun rh/sh-highlight-custom-keywords ()
  "Highlight custom shell keywords in `sh-mode`."
  (let ((rh/sh-custom-keywords
         '("print"
	   "file_exists"
	   "dir_exists"
	   "is_executable"
	   )))
    (font-lock-add-keywords
     nil
     `((,(concat "\\<" (regexp-opt rh/sh-custom-keywords t) "\\>")
        . 'rh/custom-keyword-face)))))

(provide 'rh-shell)

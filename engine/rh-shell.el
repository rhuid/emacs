(require 'rh-faces)

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

(add-hook 'sh-mode-hook #'rh/sh-highlight-custom-keywords)

(provide 'rh-shell)

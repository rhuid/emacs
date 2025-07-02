;; Highlighting custom keywords (currently implemented for sh-mode, emacs-lisp-mode, lisp-interaction-mode)

(defface rh/custom-keyword-face
  '((t (:foreground "LightGoldenrod1" :weight normal)))
  "Face for custom keywords.")

(defface rh/types-face
  '((t (:foreground "HotPink" :weight normal)))
  "Face for types.")

(defface rh/values-face
  '((t (:foreground "LightSalmon2" :weight normal)))
  "Face for values.")

(defface rh/custom-face-1
  '((t (:foreground "DarkOliveGreen2" :weight normal)))
  "A custom face.")

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

(provide 'rh-custom-faces)

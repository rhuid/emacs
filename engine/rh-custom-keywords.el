(defface rh/custom-keyword-face
  '((t (:foreground "MediumPurple1" :weight normal)))
  "Face for user-defined keywords.")

(defun rh/sh-highlight-user-defined ()
  "Highlight user-defined shell keywords in `sh-mode`."
  (font-lock-add-keywords
   nil
   '(("\\<\\(print\\|log\\|debug\\|trace\\)\\>" . 'rh/custom-keyword-face))))

(add-hook 'sh-mode-hook #'rh/sh-highlight-user-defined)

;; (defun rh/elisp-highlight-user-defined ()
;;   "Highlight user-defined elisp keywords in `elisp-mode`."
;;   (font-lock-add-keywords
;;    nil
;;    '(("\\<\\(show\\|log\\|debug\\|trace\\)\\>" . 'rh/custom-keyword-face))))

;; (add-hook 'emacs-lisp-mode-hook #'rh/elisp-highlight-user-defined)

(provide 'rh-custom-keywords)

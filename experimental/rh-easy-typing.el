;;; rh-easy-typing.el --- Some personal definitions for quick and easy typing -*- lexical-binding: t; -*-

;; Type "n1" and get "n₁"
;; Hook into post-self-insert and then to the major mode
(defun rh/replace-subscript-number ()
  "Replace trailing digits after an identifier with Unicode subscripts.
It avoids changes inside strings/comments and respects buffer-read-only."
  (interactive)
  (when (and (not buffer-read-only)
             (looking-back "\\b\\([A-Za-zα-ωΑ-Ω_]+\\)\\([0-9]+\\)" (line-beginning-position) t))
    ;; avoid doing this inside strings or comments
    (let ((ppss (syntax-ppss)))
      (unless (or (nth 3 ppss) (nth 4 ppss)) ; in-string or in-comment
        (let* ((word (match-string 1))
               (digits (match-string 2))
               (sub (mapconcat
                     (lambda (ch)
                       (cdr (assoc (char-to-string ch)
                                   '(("0" . "₀") ("1" . "₁") ("2" . "₂")
                                     ("3" . "₃") ("4" . "₄") ("5" . "₅")
                                     ("6" . "₆") ("7" . "₇") ("8" . "₈")
                                     ("9" . "₉")))))
                     (string-to-list digits) "")))
          ;; replace the whole match (word + digits) with word + subscripted digits
          (replace-match (concat word sub) t t))))))

(defun rh/easy-typing-mode ()
  (interactive)
  (add-hook 'post-self-insert-hook 'rh/replace-subscript-number))

(provide 'rh-easy-typing)

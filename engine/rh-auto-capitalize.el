;;; rh-auto-capitalize.el --- Automatically capitalize sentences in text modes -*- lexical-binding: t; -*-

(defun rh/capitalize-after-punctuation ()
  "Capitalize the first letter of a sentence or paragraph when appropriate."
  (when (and (not (nth 4 (syntax-ppss)))           ;; not inside comment
             (not (derived-mode-p 'prog-mode)))    ;; not a programming mode
    (save-excursion
      (backward-char 1)
      ;; Case 1: After punctuation
      (when (looking-back "\\([.!?]\\)[ \n]+\\([a-z]\\)" (- (point) 5) t)
        (replace-match
         (concat (match-string 1) " " (capitalize (match-string 2)))
         t))
      ;; Case 2: After newline (paragraph start)
      (when (looking-back "^[ \t]*\\([a-z]\\)" (line-beginning-position) t)
        (replace-match (capitalize (match-string 1)) t)))))

;;;###autoload
(define-minor-mode rh/auto-capitalize-mode
  "Global minor mode to automatically capitalize the start of sentences or paragraphs.
Skips programming modes and comment blocks."
  :global t
  :lighter " ↑A"
  (if rh/auto-capitalize-mode
      (add-hook 'post-self-insert-hook #'rh/capitalize-after-punctuation)
    (remove-hook 'post-self-insert-hook #'rh/capitalize-after-punctuation)))

;;;###autoload
(defun rh/toggle-auto-capitalize ()
  "Toggle automatic sentence capitalization mode."
  (interactive)
  (if rh/auto-capitalize-mode
      (progn
        (rh/auto-capitalize-mode -1)
        (message "Auto-capitalize OFF"))
    (rh/auto-capitalize-mode 1)
    (message "Auto-capitalize ON")))

;; (defvar rh/auto-capitalize-modes
;;   '(fundamental-mode text-mode org-mode markdown-mode latex-mode LaTeX-mode)
;;   "Modes in which automatic sentence capitalization should be enabled.")

;; ;; Enable it automatically in supported modes
;; (dolist (mode rh/auto-capitalize-modes)
;;   (add-hook (intern (concat (symbol-name mode) "-hook"))
;;             #'rh/auto-capitalize-mode))

(global-set-key (kbd "C-c C-S-a") #'rh/toggle-auto-capitalize)

(add-hook 'text-mode-hook #'rh/auto-capitalize-mode)

(provide 'rh-auto-capitalize)

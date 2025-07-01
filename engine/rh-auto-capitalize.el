;;; rh-auto-capitalize.el --- Automatically capitalize sentences in text modes -*- lexical-binding: t; -*-

(defun rh/capitalize-after-punctuation ()
  "Capitalize the just-typed lowercase letter if it starts a new sentence or paragraph."
  (when (and (eq this-command 'self-insert-command)
             (not (nth 4 (syntax-ppss))) ; not inside comment
             (not (derived-mode-p 'prog-mode))) ; not in code
    (let* ((pt (point))
           (char (char-before pt)))
      ;; Only proceed if just inserted a lowercase letter
      (when (and char
                 (<= ?a char ?z))
        (cond
         ;; Case 1: after punctuation + space
         ((save-excursion
            (backward-char 2)
            (and (looking-at "\\([.?!]\\)\\s-")
                 (not (nth 8 (syntax-ppss))))) ; ensure not in string/comment
          (save-excursion
            (let ((post-self-insert-hook nil))
              (backward-delete-char 1)
              (insert-char (upcase char)))))
         
         ;; Case 2: at paragraph start (fixed parentheses)
         ((save-excursion
            (backward-char)   ; move to the inserted char
            (skip-chars-backward " \t")
            (and (bolp)   ; at beginning of line?
                 (or (bobp)   ; beginning of buffer?
                     (progn   ; use progn instead of save-excursion
                       (forward-line -1)
                       (looking-at-p "^\\s-*$"))))) ; previous line blank?
          (save-excursion
            (let ((post-self-insert-hook nil))
              (backward-delete-char 1)
              (insert-char (upcase char)))))))))

;;;###autoload
(define-minor-mode rh/auto-capitalize-mode
  "Global auto-capitalization mode."
  :global t
  :lighter " ↑A"
  (if rh/auto-capitalize-mode
      (add-hook 'post-self-insert-hook #'rh/capitalize-after-punctuation)
    (remove-hook 'post-self-insert-hook #'rh/capitalize-after-punctuation)))

(defun rh/toggle-auto-capitalize ()
  "Toggle auto-capitalization mode."
  (interactive)
  (rh/auto-capitalize-mode (if rh/auto-capitalize-mode -1 1))
  (message "Auto-capitalize %s" (if rh/auto-capitalize-mode "ON" "OFF")))

(global-set-key (kbd "C-c C-S-a") #'rh/toggle-auto-capitalize)

;; Fixed hook syntax
(add-hook 'text-mode-hook #'rh/auto-capitalize-mode)

;; ;;;###autoload
;; (define-minor-mode rh/auto-capitalize-mode
;;   "Global minor mode to automatically capitalize the start of sentences or paragraphs.
;; Skips programming modes and comment blocks."
;;   :global t
;;   :lighter " ↑A"
;;   (if rh/auto-capitalize-mode
;;       (add-hook 'post-self-insert-hook #'rh/capitalize-after-punctuation)
;;     (remove-hook 'post-self-insert-hook #'rh/capitalize-after-punctuation)))

;; ;;;###autoload
;; (defun rh/toggle-auto-capitalize ()
;;   "Toggle automatic sentence capitalization mode."
;;   (interactive)
;;   (if rh/auto-capitalize-mode
;;       (progn
;;         (rh/auto-capitalize-mode -1)
;;         (message "Auto-capitalize OFF"))
;;     (rh/auto-capitalize-mode 1)
;;     (message "Auto-capitalize ON")))

;; ;; (defvar rh/auto-capitalize-modes
;; ;;   '(fundamental-mode text-mode org-mode markdown-mode latex-mode LaTeX-mode)
;; ;;   "Modes in which automatic sentence capitalization should be enabled.")

;; ;; ;; Enable it automatically in supported modes
;; ;; (dolist (mode rh/auto-capitalize-modes)
;; ;;   (add-hook (intern (concat (symbol-name mode) "-hook"))
;; ;;             #'rh/auto-capitalize-mode))

;; (global-set-key (kbd "C-c C-S-a") #'rh/toggle-auto-capitalize)

;; (add-hook 'text-mode-hook #'rh/auto-capitalize-mode)

(provide 'rh-auto-capitalize)

;;; knot-welcome-home --- A minimal and somewhat dry startup page -*- lexical-binding: t; -*-

(defun rh/welcome-home ()
  "Show custom startup screen with ASCII art and welcome message."
  (let ((buf (get-buffer-create "*Home*"))
        (banner (expand-file-name "banner.txt" user-emacs-directory)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)

        ;; Insert top text
        (insert "\n\n\n\n\n")
        (insert (make-string 132 ?\s))
        (insert (propertize "Welcome home, Ronald" 'face '(:height 2.2 :weight normal)))
        (insert "\n")
        (insert "\n\n")

        ;; Insert ASCII art
        (when (file-exists-p banner)
          (insert-file-contents banner))

        ;; Insert bottom text
        (goto-char (point-max))
        (insert "\n")
        (insert (make-string 134 ?\s))
        (insert (propertize "The Editor & Operating System" 'face '(:height 1.3 :slant italic)))
        (insert "\n\n")

	;; Quote
        (insert (make-string 123 ?\s))
        (insert (propertize "> A mathematician is a machine for turning coffee into theorems" 'face '(:height 1.0 :slant italic)))
        (insert "\n\n")

        ;; Final touches
        (goto-char (point-min))
        (read-only-mode 1)
        (fundamental-mode)
        (setq-local cursor-type nil)
        (setq-local display-line-numbers nil)
        (setq-local display-line-numbers-mode nil)))
    (switch-to-buffer buf)))

(add-hook 'emacs-startup-hook #'rh/welcome-home)
(add-hook 'server-after-make-frame-hook #'rh/welcome-home)

(provide 'knot-welcome-home)

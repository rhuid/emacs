;;; knot-startup-page.el --- A minimal no-bullshit startup page -*- lexical-binding: t; -*-

(defun rh/random-excerpt (file)
  "Return a random quote or poem from FILE, separated by lines with ---."
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((entries (split-string (buffer-string) "^---$" t "[ \t\n]*"))
           (count (length entries)))
      (when (> count 0)
        (string-trim (nth (random count) entries))))))

(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message
      (with-temp-buffer
	(insert "\nWelcome home, Ronald.\n")
        (insert-file-contents (concat user-emacs-directory "logo/Emacs-ANSI-Shadow.txt"))
	(goto-char (point-max))
	(insert "\n")
	(insert (rh/random-excerpt (concat user-emacs-directory "excerpts.txt")))
        (insert "\n")
        (buffer-string)))

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (with-current-buffer "*scratch*"
	      (setq-local buffer-read-only   t
			  left-margin-width  2
			  right-margin-width 2
			  display-line-numbers nil
			  meow-cursor-type-normal '(bar . 0)
			  meow-cursor-type-keypad '(bar . 0))
	      (set-window-buffer (selected-window) (current-buffer)))))

(provide 'knot-startup-page)

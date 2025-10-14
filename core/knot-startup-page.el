;;; knot-startup-page.el --- A minimal no-bullshit startup page -*- lexical-binding: t; -*-

;; A random line or poem every time you start Emacs!
(defun rh/random-excerpt (file)
  "Return a random quote or poem from FILE, separated by lines with ---."
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((entries (split-string (buffer-string) "^---$" t "[ \t\n]*"))
           (count (length entries)))
      (when (> count 0)
        (string-trim (nth (random count) entries))))))

;; Customize the scratch buffer
(setq initial-major-mode 'fundamental-mode
      initial-scratch-message
      (with-temp-buffer
	      (insert "\nWelcome home, Ronald.\n")
	      (insert-file-contents (concat user-emacs-directory "library/logo/Emacs-ANSI-Shadow.txt"))
	      (goto-char (point-max))
	      (insert "\n")
	      (insert (rh/random-excerpt (concat user-emacs-directory "library/excerpts.txt")))
	      (insert "\n")
	      (buffer-string)))

(defun rh/startup-setup ()
  "Some visual customizations for the startup page."
  (with-current-buffer "*scratch*"
    (visual-fill-column-mode)
    (hide-mode-line-mode)
    (setq-local buffer-read-only   t
                left-margin-width  2
                right-margin-width 2
                cursor-type nil
                display-line-numbers nil)
    (set-window-buffer (selected-window) (current-buffer))))

(add-hook 'emacs-startup-hook 'rh/startup-setup)

(defun rh/return-home ()
  "Delete other windows and return to the startup screen."
  (interactive)
  (delete-other-windows)
  (switch-to-buffer "*scratch*"))

(global-set-key (kbd "C-x r h") 'rh/return-home)

(provide 'knot-startup-page)

;; Keybindings

(global-set-key (kbd "C-c r") #'replace-string)
(global-set-key (kbd "C-c w") #'delete-trailing-whitespace)
(global-set-key (kbd "C-c t c") #'company-mode)

(defun toggle-header-line ()
  "Toggle the header-line-format in the current buffer."
  (interactive)
  (setq header-line-format
        (if header-line-format
            nil
          ""))
  (redraw-display))  ;; optional: forces immediate refresh

(global-set-key (kbd "C-c t h") #'toggle-header-line)

(provide '02-keybindings)

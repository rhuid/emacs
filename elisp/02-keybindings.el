;; Keybindings

(global-set-key (kbd "C-c r") #'replace-string)
(global-set-key (kbd "C-c w") #'delete-trailing-whitespace)
(global-set-key (kbd "C-c t c") #'company-mode)
(global-set-key (kbd "C-c t a") #'abbrev-mode)

(defun toggle-header-line ()
  "Toggle the header-line-format in the current buffer."
  (interactive)
  (setq header-line-format
        (if header-line-format
            nil
          ""))
  (redraw-display))  ;; optional: forces immediate refresh

(global-set-key (kbd "C-c t h") #'toggle-header-line)

(defun rh/clean-mode ()
  "Toggle clean-mode: header line and company mode."
  (interactive)
  (toggle-header-line)
  (company-mode 'toggle)
  (flymake-mode 'toggle))

(global-set-key (kbd "C-c c") #'rh/clean-mode)

(provide '02-keybindings)

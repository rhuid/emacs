;; Keybindings

(use-package evil
  :config
  (evil-mode 1)
  (setq evil-visual-state-cursor 'hollow))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :config
  (evil-commentary-mode))

(defun toggle-evil ()
  "Toggle Evil mode."
  (interactive)
  (if (bound-and-true-p evil-mode)
      (progn
	(evil-mode -1)
	(when (featurep 'evil-surround)
	  (global-evil-surround-mode -1))
	(message "Evil mode disabled."))
    (progn
      (evil-mode 1)
      (require 'evil-surround)
      (global-evil-surround-mode 1)
      (message "Evil mode enabled."))))

(global-set-key (kbd "C-c t e") #'toggle-evil)

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

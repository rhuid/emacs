;; Keybindings

(use-package evil
  :commands (evil-mode)
  :init
  (setq evil-visual-state-cursor 'hollow))

(use-package evil-surround
  :commands (global-evil-surround-mode))

(use-package evil-commentary
  :commands (evil-commentary-mode))

(defun toggle-evil ()
  "Toggle Evil mode."
  (interactive)
  (if (bound-and-true-p evil-mode)
      (progn
        (evil-mode -1)
        (when (fboundp 'global-evil-surround-mode)
          (global-evil-surround-mode -1))
        (setq cursor-type 'bar)
        (message "Evil mode disabled."))
    (progn
      (require 'evil)
      (require 'evil-surround)
      (evil-mode 1)
      (global-evil-surround-mode 1)
      (setq evil-visual-state-cursor 'hollow)
      (setq cursor-type 'box)
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

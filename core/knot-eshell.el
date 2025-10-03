;;; knot-eshell.el --- Eshell things -*- lexical-binding: t; -*-

(use-package eshell
  :ensure nil
  :hook (eshell-first-time-mode . rh/eshell-initialization)
  :custom
  (eshell-prompt-regexp "[$#] ")
  (eshell-prompt-function #'rh/prompt-function)
  (eshell-history-size 100000)
  (eshell-stringify-t nil)
  :config
  (defun rh/eshell-initialization ()
    (display-line-numbers-mode -1)
    (with-current-buffer "*eshell*"
      (setq-local left-margin-width  4
                  right-margin-width 4)
      (set-window-buffer (selected-window) (current-buffer))))

  (defun rh/prompt-function ()
    "A minimalist prompt of the form "
    (let ((promptface `(:foreground "violet"))
          (pwd (abbreviate-file-name (eshell/pwd))))
      (concat
       (propertize pwd 'face promptface) "\n"
       (if (zerop (user-uid)) "# " "→ ")))))

(use-package eshell-toggle
  :bind ("C-'" . eshell-toggle)
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-project-root t))

(use-package eshell-syntax-highlighting
  :demand t
  :after eshell
  :config (eshell-syntax-highlighting-global-mode +1))

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

;; Emulate a terminal
(use-package eat
  :after eshell
  :hook (eshell-mode . eat-eshell-mode))

(provide 'knot-eshell)

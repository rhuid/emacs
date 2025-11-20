;;; knot-eshell.el --- `eshell' supremacy -*- lexical-binding: t; -*-

(use-package eshell
  :ensure nil
  :hook (eshell-mode . rh/eshell-initialization)
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
       (if (zerop (user-uid)) "# " "→ "))))

  (use-package eat :after eshell :init (eat-eshell-mode)) ; emulate a terminal
  (use-package esh-autosuggest :after eshell :init (esh-autosuggest-mode))
  (use-package eshell-syntax-highlighting :after eshell :init (eshell-syntax-highlighting-global-mode)))

(use-package eshell-toggle
  :bind ("M-S" . eshell-toggle)
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-project-root t))

(provide 'knot-eshell)

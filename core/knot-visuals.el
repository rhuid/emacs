;;; knot-visuals.el --- UI, themes, modeline, pretty symbols, icons and all that -*- lexical-binding: t; -*-

;; Highlight matching parentheses, braces and brackets
(use-package paren
  :init (show-paren-mode)
  :custom
  (show-paren-delay 0)
  (show-paren-when-point-inside-paren t))

;; A minimalist mode-line
(setq-default mode-line-format
              '(" " mode-line-buffer-identification " | " mode-name " | "
                (:eval (format-time-string "%b %-d %a %-I:%M %p")) " | " (vc-mode vc-mode)))

(use-package hide-mode-line
  :hook ((dired-mode org-mode eshell-mode) . hide-mode-line-mode))

;;;; Colorize stings that represent colors
(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

;; Adjust font size globally
(global-set-key [remap text-scale-adjust] 'global-text-scale-adjust)

(defun rh/toggle-light-dark-theme-mode ()
  "Toggle between light and dark variants of the current theme."
  (interactive)
  (if-let ((theme (car custom-enabled-themes)))
      (let* ((name (symbol-name theme)))
        (cond
         ((string-suffix-p "-dark" name)
          (disable-theme theme)
          (load-theme (intern (concat (string-remove-suffix "-dark" name) "-light")) t))
         ((string-suffix-p "-light" name)
          (disable-theme theme)
          (load-theme (intern (concat (string-remove-suffix "-light" name) "-dark")) t))
         (t (message "The current theme (%s) does not have a dark/light mode." theme))))
    (message "No theme is currently set.")))

(global-set-key (kbd "C-c t t") 'rh/toggle-light-dark-theme-mode)

;; Don't prompt me for loading themes, warning it could run Lisp code.
(setq custom-safe-themes t)

;; Clean up all debris (before loading a new theme).
(mapc #'disable-theme custom-enabled-themes)

(use-package modus-themes
  :demand t
  :config
  (setq modus-themes-italic-constructs nil
        modus-themes-bold-constructs t)
  (load-theme 'modus-vivendi))

;; Although we preach minimalism, we shall allow some pretty math symbols.
(global-prettify-symbols-mode)

(defun rh/provide-pretty-symbols ()
  "Provide some pretty symbols."
  (setq prettify-symbols-alist
        '(("->" . ?→) ("=>" . ?⇒) (">=" . ?≥) ("<=" . ?≤) ("!=" . ?≠))))

(add-hook 'prog-mode-hook 'rh/provide-pretty-symbols)

(use-package spacious-padding
  :init (spacious-padding-mode))

(use-package visual-fill-column
  :hook ((org-mode emacs-lisp-mode) . visual-fill-column-mode)
  :custom
  (visual-fill-column-width 120)
  (visual-fill-column-center-text t))

(provide 'knot-visuals)

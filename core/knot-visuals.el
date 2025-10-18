;;; knot-visuals.el --- UI, themes, mode-line, pretty symbols, icons and all that -*- lexical-binding: t; -*-

;; Don't prompt me for loading themes, warning it could run Lisp code.
(setq custom-safe-themes t)

;; Clean up all debris (before loading a new theme).
(mapc #'disable-theme custom-enabled-themes)

;; A great collection of cool looking themes. Also try: modus themes
(use-package ef-themes
  :init (ef-themes-select 'ef-tritanopia-dark))

;; A minimalist mode-line
(use-package mood-line
  :init (mood-line-mode))

;; Some modes don't need the mode-line and looks cleaner without it
(use-package hide-mode-line
  :bind (:map toggle-minor-mode-map ("h" . hide-mode-line-mode))
  :hook ((dired-mode org-mode eshell-mode) . hide-mode-line-mode))

;; Highlight matching parentheses, braces and brackets
(use-package paren
  :init (show-paren-mode)
  :custom (show-paren-delay 0))

;; Colorize stings that represent colors
(use-package rainbow-mode
  :bind (:map toggle-minor-mode-map ("r" . rainbow-mode))
  :hook (prog-mode . rainbow-mode))

;; Adjust font size globally
(global-set-key [remap text-scale-adjust] 'global-text-scale-adjust)

(defun rh/toggle-light-dark-theme-mode ()
  "Toggle between -light and -dark variants of the current theme."
  (interactive)
  (if-let* ((theme (car custom-enabled-themes))
            (name (symbol-name theme)))
      (pcase name
        ((pred (string-suffix-p "-dark"))
         (disable-theme theme)
         (load-theme (intern (concat (substring name 0 -5) "-light")) t))
        ((pred (string-suffix-p "-light"))
         (disable-theme theme)
         (load-theme (intern (concat (substring name 0 -6) "-dark")) t))
        (_ (message "Theme %s has no -light/-dark pair." theme)))
    (message "No theme enabled.")))

(global-set-key (kbd "C-c t t") 'rh/toggle-light-dark-theme-mode)

;; Although we preach minimalism, we shall allow some pretty math symbols.
(global-prettify-symbols-mode)

(defun rh/provide-pretty-symbols ()
  "Provide some pretty symbols."
  (setq prettify-symbols-alist
        '(("->" . ?→) ("=>" . ?⇒) (">=" . ?≥) ("<=" . ?≤) ("!=" . ?≠))))

(add-hook 'prog-mode-hook 'rh/provide-pretty-symbols)

;; Makes you feel spacious.
(use-package spacious-padding
  :init (spacious-padding-mode)
  :custom (spacious-padding-widths '( :internal-border-width 12 :mode-line-width 3 )))

;; Like a presentation mode, much more readable and pleasant to the eyes
(use-package visual-fill-column
  :bind (:map toggle-minor-mode-map ("v" . visual-fill-column-mode))
  :hook ((org-mode text-mode magit-status-mode emacs-lisp-mode minibuffer-mode)
         . visual-fill-column-mode)
  :custom
  (visual-fill-column-width 120)
  (visual-fill-column-center-text t))

(provide 'knot-visuals)

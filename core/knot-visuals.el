;;; knot-visuals.el --- UI, themes, modeline, pretty symbols, icons and all that -*- lexical-binding: t; -*-

;; Highlight matching parentheses, braces and brackets
(use-package paren
  :init (show-paren-mode)
  :custom
  (show-paren-delay 0)
  (show-paren-when-point-inside-paren t))

;; A minimalist mode-line
(setq-default mode-line-format
              '(" " mode-line-buffer-identification " | "
                mode-name " | "
                (:eval (format-time-string "%b %-d %a %-I:%M %p")) " | "
                (vc-mode vc-mode)))

(use-package hide-mode-line
  :bind ("C-<f7>" . global-hide-mode-line-mode)
  :hook ((dired-mode org-mode eshell-mode) . hide-mode-line-mode))

;;;; Colorize stings that represent colors
(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

;; Adjust font size globally
(setq global-text-scale-adjust-size 2)
(global-set-key (kbd "C-x C-=") 'global-text-scale-adjust)
(global-set-key (kbd "C-x C--") 'global-text-scale-adjust)
(global-set-key (kbd "C-x C-0") 'global-text-scale-adjust)

;;;; General things about themes
(use-package emacs
  :bind ("C-c t t" . rh/toggle-light-dark-theme-mode)
  :config
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
      (message "No theme is currently enabled."))))

(use-package ef-themes
  :disabled t
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (ef-themes-select 'ef-dream)
  ;; Set face for (selected) regions
  (set-face-attribute 'region nil
                      :background "#353237"))

(use-package nano-theme
  :demand t
  :vc (:url "https://github.com/rougier/nano-theme")
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'nano-dark t))

;;;; Prettify symbols
(use-package emacs
  :hook ((prog-mode . rh/provide-pretty-symbols))
  :config
  (global-prettify-symbols-mode)
  (defun rh/provide-pretty-symbols ()
    "Provide some pretty symbols."
    (setq prettify-symbols-alist
          '(("->" . ?→)
            ("=>" . ?⇒)
            (">=" . ?≥)
            ("<=" . ?≤)
            ("!=" . ?≠)))))

(use-package spacious-padding
  :init (spacious-padding-mode))

;;;; Icons
(when (display-graphic-p)
  (all-the-icons-install-fonts t)
  (nerd-icons-install-fonts t))

(provide 'knot-visuals)

;;; knot-visuals.el --- UI, themes, modeline, pretty symbols, icons and all that -*- lexical-binding: t; -*-

;;; `paren'
;; Highlight matching parentheses, braces and brackets
(use-package paren
  :init (show-paren-mode)
  :custom
  (show-paren-delay 0)
  (show-paren-when-point-inside-paren t))

;;; A minimal modeline
(setq-default mode-line-format
              '(" " mode-line-buffer-identification " | "
                mode-name " | "
                (:eval (format-time-string "%b %-d %a %-I:%M %p")) " | "
                (vc-mode vc-mode)))

;;; `hide-mode-line'
;; The modeline is expendable in some major modes
(use-package hide-mode-line
  :demand t
  :bind ("C-<f7>" . global-hide-mode-line-mode)
  :hook ((dired-mode org-mode eshell-mode) . hide-mode-line-mode))

;;; `rainbow-mode'
;; Colorize stings that represent colors
(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

;;; Toggle font size
;; I usually toggle between two font sizes: one for most work and
;; the other for presentations using a projector
(use-package emacs
  :bind
  ("<f10>"   . rh/toggle-global-font-size)
  ("C-<f10>" . global-text-scale-adjust)
  :config
  (defvar rh/current-font-size 15
    "This is the default font size at startup.")

  (defun rh/toggle-global-font-size ()
    "Toggle font size between edit mode and presentation mode."
    (interactive)
    (setq rh/current-font-size (if (= rh/current-font-size 15) 20 15))
    (set-frame-font (format "Iosevka-%s" rh/current-font-size) t t)))

;;; `themes'
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

;;; `ef-themes'
;; A beautiful collection of themes but I am using nano (below) now.
(use-package ef-themes
  :disabled t
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (ef-themes-select 'ef-dream)
  ;; Set face for (selected) regions
  (set-face-attribute 'region nil
                      :background "#353237"))

;;; `nano-theme'
(use-package nano-theme
  :demand t
  :vc (:url "https://github.com/rougier/nano-theme")
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'nano-dark t)
  ;; (nano-mode)
  )

;;; `prettify-symbols'
(use-package emacs
  :ensure nil
  :hook ((prog-mode . rh/provide-pretty-symbols))
  :config
  (global-prettify-symbols-mode 1)
  (defun rh/provide-pretty-symbols ()
    "Provide some pretty symbols."
    (interactive)
    (setq prettify-symbols-alist
          '(("->" . ?→)
            ("=>" . ?⇒)
            (">=" . ?≥)
            ("<=" . ?≤)
            ("!=" . ?≠)))))

;;; `all-the-icons'
(use-package all-the-icons
  :config
  (when (display-graphic-p)
    (unless (member "all-the-icons" (font-family-list))
      (all-the-icons-install-fonts t))))

;;; `nerd-icons'
(use-package nerd-icons
  :config
  (when (display-graphic-p)
    (unless (member "Symbols Nerd Font Mono" (font-family-list))
      (nerd-icons-install-fonts t))))

(provide 'knot-visuals)

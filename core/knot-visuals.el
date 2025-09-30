;;; knot-visuals.el --- UI, themes, modeline, pretty symbols, icons and all that -*- lexical-binding: t; -*-

;; Highlight matching parentheses, braces and brackets
(use-package paren
  :init (show-paren-mode)
  :custom
  (show-paren-delay 0)
  (show-paren-when-point-inside-paren t))

(use-package mode-line
  :ensure nil
  :init
  (setq-default mode-line-format
                '(" " mode-line-buffer-identification " | "
                  mode-name " | "
                  (:eval (format-time-string "%b %-d %a %-I:%M %p")) " | "
                  (vc-mode vc-mode))))

(use-package hide-mode-line
  :bind ("C-<f7>" . global-hide-mode-line-mode)
  :hook ((dired-mode org-mode eshell-mode) . hide-mode-line-mode))

;;;; Colorize stings that represent colors
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
  (defvar rh/current-font-size rh/default-font-size
    "This is the default font size at startup.")

  (defvar rh/bigger-font-size 20
    "This is the font size for presentations.")

  (defun rh/toggle-global-font-size ()
    "Toggle font size between edit mode and presentation mode."
    (interactive)
    (setq rh/current-font-size
          (if (= rh/current-font-size rh/default-font-size)
              rh/bigger-font-size
            rh/default-font-size))
    (set-frame-font (format "Iosevka-%s" rh/current-font-size) t t)))

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

;;; `ef-themes'
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
  (load-theme 'nano-dark t)
  ;; (nano-mode)
  )

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

(use-package all-the-icons
  :config
  (when (display-graphic-p)
    (unless (member "all-the-icons" (font-family-list))
      (all-the-icons-install-fonts t))))

(use-package nerd-icons
  :config
  (when (display-graphic-p)
    (unless (member "Symbols Nerd Font Mono" (font-family-list))
      (nerd-icons-install-fonts t))))

(provide 'knot-visuals)

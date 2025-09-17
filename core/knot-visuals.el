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

;;; Toggle font size: maximum lines per window vs. comfortable reading

(defvar rh/current-font-size 15
  "This is the default font size at startup.")

(defun rh/toggle-global-font-size ()
  "Toggle font size between edit mode and presentation mode."
  (interactive)
  (setq rh/current-font-size (if (= rh/current-font-size 15) 20 15))
  (set-frame-font (format "Iosevka-%s" rh/current-font-size) t t))

;;; `ef-themes'
(use-package ef-themes
  :demand t
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (ef-themes-select 'ef-dream)
  ;; Set face for (selected) regions
  (set-face-attribute 'region nil
                      :background "#353237"))

;;; Nano theme?

;; (use-package nano-theme
;;   :demand t
;;   :vc (:url "https://github.com/rougier/nano-theme")
;;   :config
;;   (map #'disable-theme custom-enabled-themes)
;;   (nano-mode)
;;   )

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

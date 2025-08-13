;;; knot-theme-stuff.el --- Modeline, themes, icons, etc -*- lexical-binding: t; -*-

;;; A minimal modeline
(setq-default mode-line-format
              '(" " mode-line-buffer-identification " | "
                mode-name " | "
                (:eval (format-time-string "%b %-d %a %-I:%M %p")) " | "
                (vc-mode vc-mode)))

(use-package ef-themes
  :demand t
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (ef-themes-select 'ef-dream)
  ;; Set face for (selected) regions
  (set-face-attribute 'region nil
                      :background "#353237"))

(use-package all-the-icons :disabled
  :config
  (when (display-graphic-p)
    (unless (member "all-the-icons" (font-family-list))
      (all-the-icons-install-fonts t))))

(use-package nerd-icons :disabled t
  :config
  (when (display-graphic-p)
    (unless (member "Symbols Nerd Font Mono" (font-family-list))
      (nerd-icons-install-fonts t))))

(provide 'knot-theme-stuff)

;;; knot-theme-stuff.el --- Modeline, themes, icons, etc -*- lexical-binding: t; -*-

;;; A minimal modeline
(setq-default mode-line-format
              '(" " mode-line-buffer-identification " | "
                mode-name " | "
                (:eval (format-time-string "%b %-d %a %-I:%M %p")) " | "
                (vc-mode vc-mode)))

;;; Cycle through font sizes: maximum lines per window vs. comfortable reading

(defvar rh/font-sizes '(13 14 16)
  "List of font sizes to cycle through.")

(defvar rh/current-font-size-index 0
  "Index of the current font size in `rh/font-sizes`.")

(defun rh/cycle-font-size ()
  "Cycle through predefined font sizes in all frames."
  (interactive)
  (setq rh/current-font-size-index
        (mod (1+ rh/current-font-size-index) (length rh/font-sizes)))
  (let ((size (nth rh/current-font-size-index rh/font-sizes)))
    (dolist (frame (frame-list))
      (set-frame-font (format "Iosevka-%d" size) nil (list frame)))
    (message "Font size set to %d" size)))

(use-package ef-themes
  :demand t
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (ef-themes-select 'ef-dream)
  ;; Set face for (selected) regions
  (set-face-attribute 'region nil
                      :background "#353237"
                      ))

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

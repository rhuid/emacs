;; Looks and theme

;; To set the size and positon of Emacs window at startup (may be required in Emacs client mode)
(setq default-frame-alist
      '((width . 120)                                        ; in characters
        (height . 90)                                        ; in lines
	(left . 0)                                           ; position
	(top . 0)
	(font . "Iosevka-12")))

(menu-bar-mode -1)                                           ; hide menu bar
(tool-bar-mode -1)                                           ; hide tool bar
(scroll-bar-mode -1)                                         ; hide scroll bar
(set-fringe-mode 0)                                          ; try other values too
(global-visual-line-mode t)                                  ; automatic line wrapping
(global-hl-line-mode 1)
(setq-default cursor-type 'bar)                              ; cursor: thin vertical line
(blink-cursor-mode 1)
(global-prettify-symbols-mode 1) 
(global-display-line-numbers-mode t)                         ; show line numbers
(setq make-backup-files nil)                                 ; don't generate backup files

(use-package all-the-icons
  :ensure t
  :config
  (when (display-graphic-p)
    (unless (member "all-the-icons" (font-family-list))
      (all-the-icons-install-fonts t))))

(use-package nerd-icons
  :ensure t
  :config
  (when (display-graphic-p)
    (unless (member "Symbols Nerd Font Mono" (font-family-list))
      (nerd-icons-install-fonts t))))

;; (use-package doom-modeline                                   ; modeline customization
;;   :ensure t
;;   :init
;;   (doom-modeline-mode 1)
;;   :custom
;;   (doom-modeline-height 25)
;;   (doom-modeline-bar-width 6)
;;   (doom-modeline-icon t)
;;   (doom-modeline-major-mode-color-icon t)
;;   (doom-modeline-buffer-state-icon t)
;;   (doom-modeline-lsp-icon t)
;;   (doom-modeline-time-icon t)
;;   (doom-modeline-time-live-icon t)
;;   (doom-modeline-buffer-file-name-style 'truncate-upto-project)
;;   (doom-modeline-project-detection 'project)
;;   (doom-modeline-env-version t)
;;   (doom-modeline-lsp t)
;;   (doom-modeline-github nil)
;;   (setq doom-modeline-time-analogue-clock t)
;;   (setq doom-modeline-enable-word-count t)
;;   )

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (setq moody-mode-line-height 25)

  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package nyan-mode                                       ; nyan cat in the modeline representing buffer position
  :ensure t
  :config
  (nyan-mode 1))                                             ; enabling it globally

(use-package rainbow-delimiters                              ; different color for each pair of parenthesis
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Themes

(use-package catppuccin-theme)

(defun rh/set-catppuccin-theme ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'catppuccin t)
  
  (set-face-attribute 'font-lock-comment-face nil
		      :slant 'oblique                       ; make comments oblique
		      :foreground "#999999"                 ; comment color (grayish)
		      :weight 'light)                       ; make comments lighter

  ;; line numbers
  (set-face-attribute 'line-number nil
                      :foreground "#555555")
  (set-face-attribute 'line-number-current-line nil
                      :foreground "#c678dd"
                      :weight 'semi-bold)

  ;; fringe
  (set-face-background 'fringe "#1a1a1a"))

(global-set-key (kbd "C-c C-M-c") #'rh/set-catppuccin-theme)

(use-package doom-themes)

(defun rh/set-doom-vibrant-theme ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'doom-vibrant t)
  
  ;; main background
  (set-face-background 'default "#1a1a1a")
  
  ;; comments
  (set-face-attribute 'font-lock-comment-face nil
		      :slant 'oblique
		      :weight 'light)
  
  ;; line numbers
  (set-face-attribute 'line-number nil
                      :foreground "#555555")
  (set-face-attribute 'line-number-current-line nil
                      :foreground "#c678dd"
                      :weight 'semi-bold)

  ;; highlighted line and fringe
  (set-face-background 'hl-line "#222222")
  (set-face-background 'fringe "#1a1a1a"))

(global-set-key (kbd "C-c C-M-d") #'rh/set-doom-vibrant-theme)

(use-package gotham-theme)

(defun rh/set-gotham-theme ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'gotham t)
  (set-face-attribute 'font-lock-comment-face nil
		      :slant 'oblique                   
		      :foreground "#999999" 
		      :weight 'normal) 

  ;; line numbers
  (set-face-attribute 'line-number nil
                      :foreground "#555555")
  (set-face-attribute 'line-number-current-line nil
                      :foreground "#c678dd"
                      :weight 'semi-bold)

  ;; fringe
  (set-face-background 'fringe "#1a1a1a"))

(global-set-key (kbd "C-c C-M-g") #'rh/set-gotham-theme)

(defun rh/set-theme-based-on-time ()
  "Automatically set Emacs theme based on time of day.
Doom during the day, Gotham after 6 PM."
  (let* ((hour (string-to-number (format-time-string "%H")))
         (night? (or (>= hour 18) (< hour 6))))
    (mapc #'disable-theme custom-enabled-themes)
    (if night?
        (rh/set-gotham-theme)
      (rh/set-doom-vibrant-theme))))

;; Run on Emacs startup			
(add-hook 'emacs-startup-hook #'rh/set-theme-based-on-time)

;; Run every hour
(run-at-time "00:00" 3600 #'rh/set-theme-based-on-time)

(provide '01-looks)

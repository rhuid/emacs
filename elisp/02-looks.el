;; Looks

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
(setq-default cursor-type 'bar)                              ; cursor: thin vertical line
(blink-cursor-mode 1)
(global-prettify-symbols-mode 1) 
(global-display-line-numbers-mode t)                         ; show line numbers
(setq make-backup-files nil)                                 ; don't generate backup files

(set-frame-font "Iosevka-12" t t)                            ; set font (font installed separately)

(use-package all-the-icons                                   ; some pretty icons
  :ensure t)                                             

(use-package doom-modeline                                   ; modeline customization
  :ensure t
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 6)
  (doom-modeline-ico t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-project-detection 'project)
  (doom-modeline-env-version t)
  (doom-modeline-lsp t)
  (doom-modeline-github nil))

(use-package nyan-mode                                       ; nyan cat in the modeline representing buffer position
  :ensure t
  :config
  (nyan-mode 1))                                             ; enabling it globally

(use-package nerd-icons
  :ensure t)

(use-package rainbow-delimiters                              ; different color for each pair of parenthesis
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(provide '02-looks)

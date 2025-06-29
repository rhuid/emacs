;;(use-package catppuccin-theme                      
;;  :ensure t
;;  :config
;;  (load-theme 'catppuccin t))

(use-package gotham-theme                    
  :ensure t
  :config
  (load-theme 'gotham t)
  (set-face-attribute 'font-lock-comment-face nil
		      :slant 'oblique))                     ; make comments oblique
;;		      :foreground "#999999"))               ; comment color (grayish)
;;		      :weight 'light))                      ; make comments lighter

;;(use-package gandalf-theme
;;  :ensure t
;;  :config
;;  (load-theme 'gandalf t))

;;(use-package adwaita-dark-theme
;;  :ensure t
;;  :config
;;  (load-theme 'adwaita-dark))

;;(use-package ample-theme
;;  :ensure t
;;  :config
;;  (load-theme 'ample))

;;(use-package ef-themes
;;  :ensure t)
;;(load-theme 'ef-spring t)

;;(use-package doom-themes
;;  :ensure t
;;  :init                                 ; Runs before the package is loaded
;;  (setq doom-themes-enable-bold t
;;        doom-themes-enable-italic t)
;;  :config                               ; Runs after the package is loaded
;;  (load-theme 'doom-one t)
;;  (doom-themes-org-config))             ; Improved org-mode styling

(provide '03-theme)

;; This is my custom file (anyone else can ignore this)
;; I use this for two purposes
;; One, to scrap some DIY stuff that are specific to my computer without polluting the rest of my configuration
;; Two, as a traditional custom file for Emacs to infringe upon (although I almost never use interactive customization)

(load-library "~/.emacs.d/elpa/puni-20241007.1609/puni.el")          ; for some reason, `puni' won't fully load without this
(load-library "~/.emacs.d/.archives/dictrus.el")                     ; `dictrus' --- https://github.com/rhuid/dictrus
(bind-key "C-c d d" 'rh/dictrus-lookup)

;; Because `M-r' is on home row in my keyboard
(define-key key-translation-map (kbd "M-r M-b") (kbd "C-x b"))
(define-key key-translation-map (kbd "M-r M-p") (kbd "C-x p f"))

;; Silently (without output) invoke external programs from within Emacs
(bind-key "C-s-n" (lamb (start-process-shell-command "Nemo" nil "nemo .")))  ; GUI file manager in the current directory
(bind-key "C-s-g" (lamb (start-process-shell-command "GIMP" nil "gimp")))    ; GIMP

(use-package achievements :init (achievements-mode))

;; Here comes the mess!
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-fold-catch-invisible-edits 'show-and-error nil nil "Customized with use-package org")
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

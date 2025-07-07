;; (require 'rh-capitalize)

;; (use-package centaur-tabs
;;   :demand
;;   :config
;;   (centaur-tabs-mode t)
;;   (setq centaur-tabs-style "chamfer")
;;   (setq centaur-tabs-height 24)
;;   (setq centaur-tabs-set-icons t)
;;   (setq centaur-tabs-icon-type 'all-the-icons)  ; or 'nerd-icons
;;   (setq centaur-tabs-set-close-button nil)
;;   :bind
;;   ("C-<prior>" . centaur-tabs-backward)
;;   ("C-<next>" . centaur-tabs-forward))

;; (use-package aggresive-indent
;;   :ensure t)

;; (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)

;; (use-package auto-complete
;;  :config
;;  (ac-config-default))

;; (defun rhuid/open-dashboard-if-scratch ()
;;   "Show dashboard if the current buffer is *scratch*."
;;   (when (and (equal (buffer-name) "*scratch*")
;;              (not (buffer-file-name)))
;;     (dashboard-open)))

;; (add-hook 'emacs-startup-hook #'rhuid/open-dashboard-if-scratch)
;; (add-hook 'server-after-make-frame-hook #'rhuid/open-dashboard-if-scratch)


(use-package eshell
  :hook (eshell-first-time-mode . rh/eshell-init)
  :config
  (defun rh/eshell-init ()
    ;; Set prompt
    (setq eshell-prompt-function
          (lambda ()
            (concat
             (propertize (user-login-name) 'face `(:foreground "orange"))
             "@"
             (propertize (system-name) 'face `(:foreground "green"))
             ":"
             (propertize (eshell/pwd) 'face `(:foreground "blue"))
             (if (= (user-uid) 0) " # " " $ "))))
    (setq eshell-prompt-regexp "^[^#$\n]*[#$] ")))

(use-package eshell-syntax-highlighting
  :after eshell
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

;; (use-package eshell-hist-mode
;;   :hook (eshell-mode . eshell-hist-mode))

(provide '09-extras)

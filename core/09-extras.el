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




;; UI for minibuffer candidates
(use-package vertico
  :init
  (vertico-mode 1))

;; Type multiple words in any order to match candidates. Fuzzy, regex, initialism, flex
(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil))

;; Add extra info to candidates in the minibuffer, such as docstring summaries and more
(use-package marginalia
  :init
  (marginalia-mode))





(use-package eshell
  :hook (eshell-first-time-mode . rh/eshell-init)
  :config
  (defun rh/eshell-init ()
    ;; Set prompt
    (setq eshell-prompt-function
          (lambda ()
            (concat
             (propertize (user-login-name) 'face `(:foreground "cyan"))
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

;; (set-face-attribute 'eshell-prompt nil :foreground "#00ffcc" :weight 'bold)







(use-package magit
  :defer t
  :commands (magit-status magit-log)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))





(provide '09-extras)

;;; 90-tools.el --- just tools -*- lexical-binding: t; -*-

;; (require 'rh-capitalize)
(use-package centaur-tabs :disabled t
  :demand nil
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-style "chamfer")
  (setq centaur-tabs-height 24)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-icon-type 'all-the-icons)  ; or 'nerd-icons
  (setq centaur-tabs-set-close-button nil)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

;; (use-package auto-complete
;;  :config
;;  (ac-config-default))

;; ESHELL

(use-package eshell :straight nil :demand t 
  :commands eshell
  :hook ((eshell-first-time-mode . rh/eshell-init)
	 (eshell-mode . esh-autosuggest-mode))
  :config
  (defun rh/eshell-toggle ()
    "Toggle the most recent eshell buffer."
    (interactive)
    (let ((eshell-buffer
           (seq-find (lambda (buf)
                       (with-current-buffer buf
			 (derived-mode-p 'eshell-mode)))
                     (buffer-list))))
      (if (eq (current-buffer) eshell-buffer)
          (switch-to-buffer (other-buffer))
	(if eshell-buffer
            (switch-to-buffer eshell-buffer)
          (eshell)))))

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
    (display-line-numbers-mode -1)
    )
  )

(use-package eshell-syntax-highlighting :straight t :demand t :after eshell
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package esh-autosuggest :straight t :demand t :after eshell)
(use-package eat :straight t :defer t :after eshell
  ;; Emulate A Terminal
  :commands (eat eat-eshell-mode)
  :hook (eshell-mode . eat-eshell-mode))

;; (use-package eshell-hist-mode
;;   :hook (eshell-mode . eshell-hist-mode))

;; (set-face-attribute 'eshell-prompt nil :foreground "#00ffcc" :weight 'bold)

(use-package vterm :straight t :defer t
  :commands vterm
  :init
  (defun rh/vterm-toggle ()
    "Toggle the most recent vterm buffer."
    (interactive)
    (let ((vterm-buffer
           (seq-find (lambda (buf)
                       (with-current-buffer buf
			 (derived-mode-p 'vterm-mode)))
                     (buffer-list))))
      (if (eq (current-buffer) vterm-buffer)
          (switch-to-buffer (other-buffer))
	(if vterm-buffer
            (switch-to-buffer vterm-buffer)
          (vterm)))))
  :config
  (setq vterm-shell "/sbin/zsh")
  )

(use-package magit :straight t :defer t
  :commands (magit-status magit-log)
  :init
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-restore-window-configuration-after-quit nil)
  :config
  (with-eval-after-load 'evil-collection
    (evil-collection-init 'magit))
  )

(use-package aggressive-indent :straight t :defer t
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
         (lisp-mode . aggressive-indent-mode))
  :config
  (setq aggressive-indent-comments-too t))



;; (use-package captain)

;; (use-package chess)


(use-package rainbow-mode :straight t :defer t
  :hook (prog-mode . rainbow-mode))

(use-package recentf :straight nil :demand t 
  :init
  (recentf-mode 1)
  :custom
  (recentf-max-saved-items 100)
  (recentf-max-menu-items 25)
  (recentf-save-file (expand-file-name "recentf" user-emacs-directory))
  (recentf-auto-cleanup 'never)
  :config
  ;; Save recentf list every 5 minutes
  (run-at-time nil (* 5 60) #'recentf-save-list))

(use-package calc :straight nil
  :config
  (add-hook 'calc-trail-mode-hook 'evil-insert-state))

(use-package sudo-edit :straight t :commands (sudo-edit))

(provide '90-tools)

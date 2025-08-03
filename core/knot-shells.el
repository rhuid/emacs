;;; knot-shells.el --- Mainly eshell and vterm -*- lexical-binding: t; -*-

;;;; Eshell

(use-package eshell :ensure nil :demand t
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

(use-package eshell-syntax-highlighting :demand t :after eshell
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package esh-autosuggest :demand t :after eshell)
(use-package eat :after eshell
  ;; Emulate A Terminal
  :commands (eat eat-eshell-mode)
  :hook (eshell-mode . eat-eshell-mode))

;; (use-package eshell-hist-mode
;;   :hook (eshell-mode . eshell-hist-mode))

;; (set-face-attribute 'eshell-prompt nil :foreground "#00ffcc" :weight 'bold)

;;;; vterm

(use-package vterm
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

(provide 'knot-shells)

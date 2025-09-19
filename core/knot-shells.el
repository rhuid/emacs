;;; knot-shells.el --- Mainly eshell and vterm -*- lexical-binding: t; -*-

;;; `eshell'
(use-package eshell
  :ensure nil
  :demand t
  :commands eshell
  :hook
  (eshell-first-time-mode . rh/eshell-init)
  (eshell-mode . esh-autosuggest-mode)
  :bind (("C-c u s" . rh/eshell-toggle)
         ("C-c u v" . rh/vterm-toggle))
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

  ;; Initialization
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

    ;; Extra visual touches
    (display-line-numbers-mode -1)
    (with-current-buffer "*eshell*"
      (setq-local left-margin-width  1
                  right-margin-width 1)
      (set-window-buffer (selected-window) (current-buffer))))
  :custom
  (eshell-history-size 100000)
  (eshell-prompt-regexp "[$#] "))

(use-package eshell-syntax-highlighting
  :demand t
  :after eshell
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package esh-autosuggest :demand t :after eshell)

;; Emulate A Terminal
(use-package eat
  :after eshell
  :commands (eat eat-eshell-mode)
  :hook (eshell-mode . eat-eshell-mode))

;; (use-package eshell-hist-mode
;;   :hook (eshell-mode . eshell-hist-mode))

;; (set-face-attribute 'eshell-prompt nil :foreground "#00ffcc" :weight 'bold)

;;; `vterm'
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
  (setq vterm-shell "/sbin/zsh"))

(provide 'knot-shells)

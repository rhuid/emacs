;;; 90-tools.el --- just tools -*- lexical-binding: t; -*-

;; (require 'rh-capitalize)

(use-package centaur-tabs
  :disabled t
  :demand
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

;; MINIBUFFER 

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

;; Adds modern alternatives to core Emacs commands
(use-package consult
  :bind
  (("C-s" . consult-line)
   ("C-x b" . consult-buffer)
   ("M-y" . consult-yank-pop)
   ("C-c f" . consult-find)
   ("C-c l" . consult-locate)
   )
  :config
  
  ;; To always start searching from home directory
  (advice-add 'consult-find :around
              (lambda (orig &rest args)
		(let ((default-directory (expand-file-name "~")))
                  (apply orig args))))
  )

;; (setq consult-preview-key "M-.") ; preview only when you press M-

;; Live popup of possible key combinations
(use-package which-key
  :config
  (which-key-mode))

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  ;; Add Embark to the mouse context menu. Also enable `context-menu-mode'.
  ;; (context-menu-mode 1)
  ;; (add-hook 'context-menu-functions #'embark-context-menu 100)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))





;; ESHELL

(defun rh/eshell-toggle ()
  "Toggle eshell buffer."
  (interactive)
  (if (string= (buffer-name) "*eshell*")
      (switch-to-prev-buffer)
    (eshell)))

(use-package eshell
  :hook ((eshell-first-time-mode . rh/eshell-init)
	 (eshell-mode . esh-autosuggest-mode))
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
	     (if (= (user-uid) 0) " # " " $ ")))))
  
  (use-package eshell-syntax-highlighting
    :after eshell
    :config
    (eshell-syntax-highlighting-global-mode +1))

  (use-package esh-autosuggest)
  )

;; Emulate A Terminal
(use-package eat
  :hook (eshell-mode . eat-eshell-mode))

;; (use-package eshell-hist-mode
;;   :hook (eshell-mode . eshell-hist-mode))

;; (set-face-attribute 'eshell-prompt nil :foreground "#00ffcc" :weight 'bold)




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

;; (defun rh/temp-vterm-toggle ()
;;   "Toggle vterm buffer."
;;   (interactive)
;;   (if (string= (buffer-name) "*vterm*")
;;       (switch-to-prev-buffer)
;;     (vterm)))


(use-package vterm
  :straight t
  :commands vterm
  :config
  (setq vterm-shell "/sbin/zsh"))



(use-package magit
  :defer t
  :commands (magit-status magit-log)
  :init
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1))




(use-package aggressive-indent
  :ensure t
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
         (lisp-mode . aggressive-indent-mode))
  :config
  (setq aggressive-indent-comments-too t))



;; (use-package captain)

;; (use-package chess)



(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package general
  :after outline
  :config
  (general-create-definer rh/leader-keys
    :states '(normal visual)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (rh/leader-keys
    ;; eshell
    "e t"     'rh/eshell-toggle

    ;; vterm
    "t t"     'rh/vterm-toggle

    ;; Outline Minor Mode
    "o t"     'rh/outline-toggle-heading
    "o <tab>" 'rh/outline-toggle-heading
    "o a"     'rh/outline-toggle-visibility
    ))

(provide '90-tools)

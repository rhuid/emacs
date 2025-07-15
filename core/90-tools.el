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

;; MINIBUFFER

;; UI for minibuffer candidates
(use-package vertico :straight t :demand t :init (vertico-mode 1))
(use-package orderless :straight t :demand t 
  ;; Type multiple words in any order to match candidates. Fuzzy, regex, initialism, fle
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil))

;; Add extra info to candidates in the minibuffer, such as docstring summaries and more
(use-package marginalia :straight t  :demand t :init (marginalia-mode))
(use-package consult :straight t :demand t 
  ;; Adds modern alternatives to core Emacs commands
  :bind
  (("C-c f" . consult-find)
   ("C-c l" . consult-locate)
   ("C-c r" . consult-recent-file)
   ("C-s" . consult-line)
   ("C-x b" . consult-buffer)
   ("M-y" . consult-yank-pop)
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
(use-package which-key :straight t  :demand t :config (which-key-mode))
(use-package embark :straight t :demand t 
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
(use-package embark-consult :straight t :demand t 
  ;; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; ESHELL

(use-package eshell :straight nil :demand t 
  :commands eshell
  :hook ((eshell-first-time-mode . rh/eshell-init)
	 (eshell-first-time-mode . rh/sync-eshell-PATH-with-exec-path)
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
	     (if (= (user-uid) 0) " # " " $ ")))))
  (defun rh/sync-eshell-PATH-with-exec-path ()
    "Sync Eshell's $PATH with Emacs's `exec-path`."
    (setenv "PATH" (string-join exec-path path-separator)))
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
        #'magit-display-buffer-same-window-except-diff-v1))


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



(use-package general :straight t :demand t :after outline
  :config
  (general-create-definer rh/leader-keys
    :states '(normal visual)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (rh/leader-keys
    ;; eshell
    "e t"     'rh/eshell-toggle

    ;; eval
    "e e"     'eval-expression

    ;; outline minor mode
    "o t"     'rh/outline-toggle-heading
    "o <tab>" 'rh/outline-toggle-heading
    "o a"     'rh/outline-toggle-visibility

    ;; string manipulation
    "s r"     'replace-string
    "s w"     'delete-trailing-whitespace
    "s a"     'abbrev-mode

    ;; vterm
    "t t"     'rh/vterm-toggle
    
    ;; utilities
    "u r"     'recentf-open-files
    ))

;; All custom keybindings

(global-set-key (kbd "<f5>") #'rh/cycle-modus-themes)


(provide '90-tools)

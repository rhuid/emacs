;;; knot-completion.el --- Enhancements of the minibuffer -*- lexical-binding: t; -*-

;;; Mostly completion frameworks. There are lots of individual packages which all work together.

(use-package vertico :straight t :demand t
  :config (vertico-mode)
  :bind (("C-x f" . find-file)
	 :map vertico-map
	 ("C-j"   . vertico-exit-input)
	 ("C-M-p" . vertico-prev-group)
	 ("C-M-n" . vertico-next-group))
  :custom
  (vertico-resize t)
  (vertico-cycle t))

(use-package orderless :straight t :demand t
  ;; Type multiple words in any order to match candidates
  :init
  (setq completion-styles '(orderless partial-completion)
        completion-category-defaults nil))

(use-package marginalia :straight t  :demand t
  ;; Add extra info to candidates in the minibuffer, such as docstring summaries and more
  :init (marginalia-mode))

(use-package consult :straight t :demand t
  ;; Adds modern alternatives to core Emacs commands
  :bind
  (("C-c f"  . consult-find)
   ("C-c l"  . consult-locate)
   ("C-c r"  . consult-recent-file)
   ("C-s"    . consult-line)
   ("C-M-s"  . consult-line-multi)
   ("C-M-g"  . consult-ripgrep)
   ("C-x b"  . consult-buffer)
   ("C-M-e"  . consult-buffer)
   ("M-y"    . consult-yank-pop)
   ("C-M-b"  . consult-bookmark)
   ("C-c t"  . consult-theme)
   ("M-m"    . consult-imenu)
   ("M-p"    . consult-project-buffer)
   ("M-o"    . consult-outline)
   )
  :config
  (setq consult-preview-key 'any)

  ;; To always start searching from home directory
  (advice-add 'consult-find :around
              (lambda (orig &rest args)
		(let ((default-directory (expand-file-name "~")))
                  (apply orig args)))))

(use-package embark :straight t :demand t
  :bind
  (("C-."   . embark-act)
   ("C-;"   . embark-dwim)
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
                 ;; nil
		 (display-buffer-in-side-window)
		 (side . bottom)
		 (window-height . 0.3 )
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult :straight t :demand t :after consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package which-key :straight t :demand t
  ;; Live popup of possible key combinations
  :config
  (setq which-key-idle-delay 0.5)
  (setq which-key-popup-type 'minibuffer)
  (which-key-mode))

(use-package corfu :straight t :demand t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)                        ;; Enable auto popup
  (corfu-auto-delay 0.1)
  (corfu-minimum-prefix-length 2)
  (corfu-preview-current t)
  (corfu-on-exact-match t)              ;; Auto-select exact match
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match nil)
  (corfu-preselect 'prompt)             ;; Preselect candidate
  (corfu-cycle t))                      ;; Cycle through candidates

(with-eval-after-load 'corfu

  ;; Disable some annoying stuffs like inserting on TAB or RET
  (define-key corfu-map (kbd "RET") nil)
  (define-key corfu-map (kbd "<return>") nil)
  (define-key corfu-map (kbd "TAB") nil)
  (define-key corfu-map (kbd "<tab>") nil)
  (define-key corfu-map (kbd "C-j") nil)
  (define-key corfu-map (kbd "C-m") nil)

  ;; Use C-p and C-n to navigate through the corfu suggestions, and C-SPC to insert the selection
  (define-key corfu-map (kbd "C-n") #'corfu-next)
  (define-key corfu-map (kbd "C-p") #'corfu-previous)
  (define-key corfu-map (kbd "C-SPC") #'corfu-insert))

(use-package cape :straight t :after corfu
  :init
  (setq completion-at-point-functions
	(list #'cape-symbol
	      #'cape-dabbrev
	      #'cape-file
	      #'cape-history
	      #'cape-tex)))

(provide 'knot-completion)

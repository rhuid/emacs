;;; knot-completion.el --- Minibuffer completions and at-point completions -*- lexical-binding: t; -*-

(use-package vertico
  :init (vertico-mode)
  :bind (("C-x f" . find-file)
         :map vertico-map
         ("C-j"   . vertico-exit-input)
	       ("C-M-p" . vertico-prev-group)
	       ("C-M-n" . vertico-next-group))
  :custom
  (vertico-resize t)
  (vertico-cycle t))

(use-package vertico-directory
  :after vertico
  :load-path "~/.emacs.d/elpa/vertico/extensions/"
  :ensure nil
  :bind (:map vertico-map
	            ("C-h" . vertico-directory-delete-char)
	            ("C-w" . vertico-directory-delete-word)))

;;;; Type words in any order to match candidates
(use-package orderless
  :init (setq completion-styles '(orderless partial-completion)
              completion-category-defaults nil))

;;;; Add documentation to minibuffer results
(use-package marginalia
  :init (marginalia-mode))

(use-package consult
  :demand t
  :bind
  (("C-c f"   . consult-find)
   ("C-c L"   . consult-locate)
   ("C-x C-f" . consult-recent-file)
   ("C-M-s"   . consult-line)
   ("C-M-g"   . consult-ripgrep)
   ("C-x b"   . consult-buffer)
   ("C-M-e"   . consult-buffer)
   ("M-y"     . consult-yank-pop)
   ("C-c b"   . consult-bookmark)
   ("C-M-b"   . consult-bookmark)
   ("C-<f5>"  . consult-theme)
   ("M-m"     . consult-imenu)
   ("M-p"     . consult-project-buffer)
   ("M-O"     . consult-outline))
  :config
  (setq consult-preview-key nil)
  ;; To always start searching from home directory
  ;; (advice-add 'consult-find :around
  ;;             (lambda (orig &rest args)
	;; 	            (let ((default-directory (expand-file-name "~")))
  ;;                 (apply orig args))))
  )

;;;; Jump to recent directories
(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
	       ("C-x C-j" . consult-dir-jump-file)))

(use-package embark
  :demand t
  :bind
  (("C-."    . embark-act)
   ("C-;"    . embark-dwim)
   ("<f1>-B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
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

(use-package embark-consult
  :demand t
  :after consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.5
        which-key-popup-type 'minibuffer)
  (dolist (binding '(("C-c e" . "emms")
                     ("C-c s" . "string-manipulation")
                     ("C-c u" . "utilities")))
    (which-key-add-key-based-replacements (car binding) (cdr binding))))

(use-package corfu
  :init (global-corfu-mode)
  :after orderless
  :hook ((corfu-mode . corfu-history-mode)
         (corfu-mode . corfu-indexed-mode)
         (corfu-mode . corfu-popupinfo-mode))
  :custom
  (corfu-auto t)                    ;; Enable auto popup
  (corfu-auto-delay 0.2)
  (corfu-minimum-prefix-length 2)
  (corfu-preview-current t)
  (corfu-on-exact-match t)          ;; Auto-select exact match
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
  (corfu-preselect 'prompt)         ;; Preselect candidate
  (corfu-cycle t)
  :config
  (define-key corfu-map (kbd "C-n") 'corfu-next)
  (define-key corfu-map (kbd "C-p") 'corfu-previous)
  (define-key corfu-map (kbd "C-e") 'corfu-insert)
  (define-key corfu-map (kbd "RET") nil)
  (define-key corfu-map (kbd "TAB") nil)
  (define-key corfu-map (kbd "<return>") nil)
  (define-key corfu-map (kbd "<tab>") nil)
  (define-key corfu-map (kbd "C-j") nil)
  (define-key corfu-map (kbd "C-m") nil))

(use-package cape
  :after corfu
  :demand t
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)

  ;; :init
  ;; (setq completion-at-point-functions
	;;       (list #'cape-symbol
	;;             #'cape-dabbrev
	;;             #'cape-file
	;;             #'cape-history
	;;             #'cape-tex
  ;;             #'cape-keyword
  ;;             #'cape-dict
  ;;             ))

  )

(provide 'knot-completion)

;;; knot-completion.el --- Enhancements of the minibuffer -*- lexical-binding: t; -*-

;;;; Mostly completion frameworks. There are lots of individual packages which all work together well.

(use-package vertico
  :demand t
  :vc (:url "https://github.com/minad/vertico")
  :config (vertico-mode)
  :bind (("C-x f" . find-file)
         :map vertico-map
         ("C-j"   . vertico-exit-input)
	       ("C-M-p" . vertico-prev-group)
	       ("C-M-n" . vertico-next-group))
  :custom
  (vertico-resize t)
  (vertico-cycle t))

(use-package vertico-directory
  :demand t
  :after vertico
  :load-path "~/.emacs.d/elpa/vertico/extensions/"
  :ensure nil
  :bind (:map vertico-map
	            ("DEL" . vertico-directory-delete-char)
	            ("C-w" . vertico-directory-delete-word)))

;;;; Type multiple words in any order to match candidates

(use-package orderless
  :demand t
  :vc (:url "https://github.com/oantolin/orderless")
  :init
  (setq completion-styles '(orderless partial-completion)
        completion-category-defaults nil))

;;;; Add extra info to candidates in the minibuffer, such as docstring summaries and more

(use-package marginalia
  :demand t
  :vc (:url "https://github.com/minad/marginalia")
  :init (marginalia-mode))

(use-package consult
  :demand t
  :vc (:url "https://github.com/minad/consult")
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
  (setq consult-preview-key 'any)

  ;; To always start searching from home directory
  (advice-add 'consult-find :around
              (lambda (orig &rest args)
		            (let ((default-directory (expand-file-name "~")))
                  (apply orig args)))))

;;;; Jump to recent directories

(use-package consult-dir
  :demand t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
	       ("C-x C-j" . consult-dir-jump-file)))

(use-package embark
  :demand t
  :vc (:url "https://github.com/oantolin/embark")

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
  :vc (:url "https://github.com/oantolin/embark")
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; Pop up available keybindings as you type the keys
(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.5
        which-key-popup-type 'minibuffer)
  (dolist (binding '(("C-c e" . "emms")
                     ("C-c s" . "string-manipulation")
                     ("C-c u" . "utilities")))
    (which-key-add-key-based-replacements (car binding) (cdr binding))))

;;;; I prefer corfu over company for completion

(use-package corfu
  :demand t
  :after orderless
  :hook ((corfu-mode . corfu-history-mode)
         (corfu-mode . corfu-indexed-mode)
         (corfu-mode . corfu-popupinfo-mode))
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)                        ;; Enable auto popup
  (corfu-auto-delay 0.2)
  (corfu-minimum-prefix-length 2)
  (corfu-preview-current t)
  (corfu-on-exact-match t)              ;; Auto-select exact match
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
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

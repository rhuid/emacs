;;; 60-src.el --- All things related to writing source code -*- lexical-binding: t; -*-

;; Coding

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-signature-auto-activate nil)
  (lsp-log-io nil))

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable t))

(use-package company
  :straight t
  :commands company-mode
  :hook (lean4-mode . company-mode)
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
	company-auto-commit nil
	company-auto-commit-chars nil)

  (define-key company-active-map (kbd "TAB") nil)
  (define-key company-active-map (kbd "<tab>") nil)
  (define-key company-active-map (kbd "RET") #'company-abort)
  (define-key company-active-map (kbd "<ret>") #'company-abort)

  (define-key company-active-map (kbd "C-l") #'company-complete-selection)
  (define-key company-mode-map (kbd "C-l") #'company-complete))

(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)                        ;; Enable auto popup
  (corfu-auto-delay 0.2)
  (corfu-minimum-prefix-length 2)
  (corfu-preview-current nil)           ;; Don't preview current candidate inline
  (corfu-on-exact-match nil)            ;; Don’t auto-select exact match
  (corfu-quit-at-boundary t)            ;; Stop completion at word boundaries
  (corfu-quit-no-match t)               ;; Quit if no match
  (corfu-preselect 'prompt)             ;; Don't preselect any candidate
  (corfu-cycle t))                      ;; Cycle through candidates

(with-eval-after-load 'corfu
  (define-key corfu-map (kbd "RET") nil)
  (define-key corfu-map (kbd "<return>") nil)
  (define-key corfu-map (kbd "TAB") nil)
  (define-key corfu-map (kbd "<tab>") nil)
  (define-key corfu-map (kbd "C-j") nil)
  (define-key corfu-map (kbd "C-m") nil)

  (define-key corfu-map (kbd "C-SPC") #'corfu-insert))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

;; Haskell
(use-package haskell-mode
  :defer t
  :mode "\\.hs\\'"
  )

(use-package haskell-tng-mode
  :defer t
  ;; :mode "\\.hs\\"
  )

(defun rh/lean4-corfu-off-company-on ()
  (interactive)
  "Disable corfu and enable company only in Lean4 buffers."
  (corfu-mode -1)
  (company-mode +1))
(use-package lean4-mode
  :commands lean4-mode
  :straight (lean4-mode :type git :host github
                        :repo "leanprover-community/lean4-mode"
                        :files ("*.el" "data"))
  :defer t
  :mode "\\.lean\\'"
  :hook ((lean4-mode . rh/lean4-tab-hook)
         (lean4-mode . rh/lean-highlight-types)
         (lean4-mode . rh/lean-highlight-values)
         (lean4-mode . rh/lean-highlight-typeclasses)
	 (lean4-mode . rh/lean4-corfu-off-company-on))
  :init
  (require 'lean4-mode)
  (require 'rh-lean)
  )

(use-package sh-script
  :defer t
  :mode ("\\.sh\\'" . sh-mode)
  :init
  (require 'rh-shell)
  :hook (sh-mode . rh/setup-sh-mode)
  :config
  (defun rh/setup-sh-mode ()
    (rh/sh-tab-hook)
    (rh/sh-highlight-custom-keywords))
  )

(use-package outline
  :hook ((prog-mode . outline-minor-mode)
         (text-mode . outline-minor-mode)
         (outline-minor-mode . outline-show-all)
	 (outline-minor-mode . outline-hide-body))
  :init
  ;; Set the keybinding prefix for built-in outline commands
  (setq outline-minor-mode-prefix (kbd "C-c @"))

  :config
  ;; Custom folding indicator (like +)
  (set-display-table-slot
   standard-display-table
   'selective-display
   (let ((face-offset (* (face-id 'shadow) (lsh 1 22))))
     (vconcat (mapcar (lambda (c) (+ face-offset c)) " +"))))

  ;; Global toggle function for heading subtree
  (defun rh/outline-toggle ()
    "Toggle visibility of current outline heading."
    (interactive)
    (save-excursion
      (outline-back-to-heading)
      (if (outline-invisible-p (line-end-position))
          (outline-show-subtree)
        (outline-hide-subtree))))

  ;; Toggle keybinding (in both Evil and non-Evil)
  (define-key outline-minor-mode-map (kbd "C-c @ <tab>") #'rh/outline-toggle)
  (evil-global-set-key 'normal (kbd "C-c @ <tab>") #'rh/outline-toggle)
  (evil-global-set-key 'normal (kbd "C-c @ <backtab>") #'outline-hide-body))

(use-package lisp-mode
  :straight nil
  :mode ("\\.el\\'" . emacs-lisp-mode)
  :hook ((emacs-lisp-mode . rh/elisp-tab-hook)
	 (emacs-lisp-mode . rh/elisp-highlight-custom-keywords)
	 (emacs-lisp-mode . rh/outline-elisp)
	 (lisp-interaction-mode . rh/elisp-tab-hook)
	 (lisp-interaction-mode . rh/elisp-highlight-custom-keywords))
  :init
  (require 'rh-elisp)

  :config
  (defun rh/outline-elisp ()
    "Set outline regex for top-level declarations in Emacs Lisp."
    (setq-local outline-regexp
		(rx line-start
                    (* space)
                    "("
                    (or ";;;" "use-package" "require" "provide" "defun"
			"add-to-list" "add-hook")))
    (outline-hide-body))
  )

(use-package rust-mode
  :defer t
  :mode "\\.rs\\'"
  :hook ((rust-mode . outline-minor-mode)
	 (rust-mode . rh/outline-rust)
	 (rust-mode . flycheck-rust-setup))
  :init
  (require 'rh-rust)
  :config
  (setq rust-format-on-save t)

  ;; Currently not working, will check
  (defun rh/outline-rust ()
    (setq-local outline-regexp
		(rx line-start (* space)
                    (or "fn" "pub" "struct" "enum" "impl")))
    (outline-hide-body))

  (use-package flycheck-rust)
  )

(use-package nix-mode
  :straight t
  :mode "\\.nix\\'"
  :config
  (setq nix-indent-function 'nix-indent-line))

(use-package julia-mode
  :mode "\\.jl\\'")

(use-package kbd-mode
  :straight (kbd-mode :type git :host github :repo "kmonad/kbd-mode")
  :mode "\\.kbd\\'")

(use-package systemd
  :mode (("\\.service\\'" . systemd-mode)
         ("\\.timer\\'"   . systemd-mode)
         ("\\.mount\\'"   . systemd-mode)
         ("\\.target\\'"  . systemd-mode)
	 ("\\.conf\\'"    . conf-unix-mode)
	 ("\\.ini\\'"     . conf-unix-mode)))

(provide '60-src)

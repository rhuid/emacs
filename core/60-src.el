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

;; (use-package company
;;   :hook (after-init . global-company-mode)
;;   :config
;;   (setq company-idle-delay 0.2
;;         company-minimum-prefix-length 2
;; 	company-auto-commit nil
;; 	company-auto-commit-chars nil)

;; (define-key company-active-map (kbd "TAB") nil)
;; (define-key company-active-map (kbd "<tab>") nil)
;; (define-key company-active-map (kbd "RET") #'company-abort)
;; (define-key company-active-map (kbd "<ret>") #'company-abort)

;; (define-key company-active-map (kbd "C-l") #'company-complete-selection)
;; (define-key company-mode-map (kbd "C-l") #'company-complete))

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
  :mode "\\.hs\\'"
  )

(use-package haskell-tng-mode
  ;; :mode "\\.hs\\"
  )

(use-package lean4-mode
  :straight nil
  :load-path "~/.emacs.d/lean4-mode"
  :mode "\\.lean\\'"
  :init
  (require 'lean4-mode)
  (require 'rh-lean)
  :hook ((lean4-mode . rh/lean4-tab-hook)
         (lean4-mode . rh/lean-highlight-types)
         (lean4-mode . rh/lean-highlight-values)
         (lean4-mode . rh/lean-highlight-typeclasses)))

(use-package sh-script
  :mode "\\.sh\\'"
  :init
  (require 'rh-shell)
  :hook (sh-mode . rh/setup-sh-mode))

(defun rh/setup-sh-mode ()
  (rh/sh-tab-hook)
  (rh/sh-highlight-custom-keywords))

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

  ;; ;; Overview function: show headings only
  ;; (defun rh/outline-overview ()
  ;;   "Show only outline headings."
  ;;   (outline-show-all)
  ;;   (outline-hide-body))

  ;; Toggle keybinding (in both Evil and non-Evil)
  (define-key outline-minor-mode-map (kbd "C-c @ <tab>") #'rh/outline-toggle)
  (evil-global-set-key 'normal (kbd "C-c @ <tab>") #'rh/outline-toggle)
  (evil-global-set-key 'normal (kbd "C-c @ <backtab>") #'outline-hide-body))

;; ;; Specialized config for Emacs Lisp mode
;; (use-package emacs
;;   :ensure nil
;;   :hook (emacs-lisp-mode . rh/outline-elisp)
;;   :config
;;   (defun rh/outline-elisp ()
;;     "Set outline regex for top-level declarations in Emacs Lisp."
;;     (setq-local outline-regexp
;;                 (rx line-start
;;                     (* space)
;;                     "("
;;                     (or ";;;" "use-package" "require" "provide" "defun"
;;                         "add-to-list" "add-hook")))
;;     (outline-hide-body)))

(require 'rh-elisp)
(defun rh/outline-elisp ()
  "Set outline regex for top-level declarations in Emacs Lisp."
  (setq-local outline-regexp
              (rx line-start
                  (* space)
                  "("
                  (or ";;;" "use-package" "require" "provide" "defun"
                      "add-to-list" "add-hook")))
  (outline-hide-body))

(add-hook 'emacs-lisp-mode-hook #'rh/elisp-tab-hook)
(add-hook 'emacs-lisp-mode-hook #'rh/elisp-highlight-custom-keywords)
(add-hook 'emacs-lisp-mode-hook #'rh/outline-elisp)
(add-hook 'lisp-interaction-mode-hook #'rh/elisp-tab-hook)
(add-hook 'lisp-interaction-mode-hook #'rh/elisp-highlight-custom-keywords)

(use-package rust-mode
  :mode "\\.rs\\'"
  :hook ((rust-mode . outline-minor-mode)
	 (rust-mode . rh/outline-rust))
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
  )

(use-package flycheck-rust
  :hook (rust-mode . flycheck-rust-setup))

(use-package nix-mode
  :straight t
  :mode "\\.nix\\'"
  :config
  (setq nix-indent-function 'nix-indent-line))

(use-package julia-mode
  :mode "\\.jl\\'")

(use-package kbd-mode
  :straight (kbd-mode :type git :host github :repo "kmonad/kbd-mode"))

(use-package systemd
  :mode (("\\.service\\'" . systemd-mode)
         ("\\.timer\\'" . systemd-mode)
         ("\\.mount\\'" . systemd-mode)
         ("\\.target\\'" . systemd-mode)))

(provide '60-src)

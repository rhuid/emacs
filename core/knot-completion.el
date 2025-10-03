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
  :bind (:map vertico-map
	            ("C-h" . vertico-directory-delete-char)
	            ("C-w" . vertico-directory-delete-word)))

(use-package orderless
  :init (setq completion-styles '(orderless partial-completion)))

(use-package marginalia
  :init (marginalia-mode))

(use-package consult
  :bind (("C-x C-f" . consult-recent-file)
         ("C-x b"   . consult-buffer)
         ("C-M-s"   . consult-line)
         ("C-M-g"   . consult-ripgrep)
         ("C-x C-y" . consult-yank-pop)
         ("M-m"     . consult-imenu)
         ("M-O"     . consult-outline))
  :config (setq consult-preview-key nil))

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
	       ("C-x C-j" . consult-dir-jump-file)))

(use-package embark
  :bind (("C-."    . embark-act)
         ("C-;"    . embark-dwim)
         ("<f1>-B" . embark-bindings))
  :init (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package which-key
  :init (which-key-mode))

(use-package corfu
  :init (global-corfu-mode)
  :hook ((corfu-mode . corfu-history-mode)
         (corfu-mode . corfu-indexed-mode)
         (corfu-mode . corfu-popupinfo-mode))
  :custom (corfu-auto t) ; Enable auto popup
  :config
  (define-key corfu-map (kbd "TAB") nil)
  (define-key corfu-map (kbd "RET") nil)
  (define-key corfu-map (kbd "C-t") 'corfu-insert))

(use-package cape
  :after corfu
  :demand t
  :hook ((LaTeX-mode org-mode) . rh/cape-dict-completion)
  :config
  (setq cape-dict-file '())
  (setq rh/dict-file (concat user-emacs-directory "library/personal-dictionary"))
  (add-to-list 'cape-dict-file rh/dict-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (defun rh/cape-dict-completion ()
    "Setup completion using my personal dictionary file."
    (setq-local completion-at-point-functions
                (cons (cape-capf-super #'cape-dict)
                      completion-at-point-functions))))

(provide 'knot-completion)

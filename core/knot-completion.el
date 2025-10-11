;;; knot-completion.el --- Minibuffer completions and at-point completions -*- lexical-binding: t; -*-

(use-package vertico
  :init (vertico-mode)
  :custom
  (vertico-resize t)
  (vertico-cycle t))

(use-package vertico-directory
  :after vertico
  :load-path "~/.emacs.d/elpa/vertico/extensions/"
  :bind (:map vertico-map ("C-w" . vertico-directory-delete-word)))

(use-package orderless
  :init (setq completion-styles '(orderless partial-completion)))

(use-package marginalia
  :init (marginalia-mode))

(use-package consult
  :bind (("C-x f" . consult-recent-file)
         ("C-x b" . consult-buffer)
         ("C-M-s" . consult-line)
         ("C-M-g" . consult-ripgrep)
         ("M-O"   . consult-outline))
  :config (setq consult-preview-key "C-,"))

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir))
  (:map vertico-map ("C-x C-j" . consult-dir-jump-file)))

(use-package embark
  :bind (("C-." . embark-act) ("C-;" . embark-dwim))
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
  :bind (:map corfu-map ("TAB" . nil) ("RET" . nil) ("C-t" . 'corfu-insert))
  :custom (corfu-auto t)) ; Enable auto popup

(use-package cape
  :after corfu
  :demand t
  :config
  (setq cape-dict-file '())
  (add-to-list 'cape-dict-file (concat user-emacs-directory "library/personal-dictionary"))
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-dict))

(provide 'knot-completion)

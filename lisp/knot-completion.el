;;; knot-completion.el --- Minibuffer completions and at-point completions -*- lexical-binding: t; -*-

;; Vertical completion UI for the minibuffer.
(use-package vertico
  :init (vertico-mode)
  :custom
  (vertico-resize t)
  (vertico-cycle t))

;; Directory traversal made swift.
(use-package vertico-directory
  :after vertico
  :load-path "~/.emacs.d/elpa/vertico/extensions/"
  :bind (:map vertico-map ("C-w" . vertico-directory-delete-word)))

;; Find what you seek, no matter the order.
(use-package orderless
  :init (setq completion-styles '(orderless partial-completion)))

;; See the story behind every completion candidate.
(use-package marginalia
  :init (marginalia-mode))

;; Consult is your compass for Emacs; a command center for the thermonuclear editor.
(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-x f" . consult-recent-file)
         ("C-S-l" . consult-line)
         ("C-M-g" . consult-ripgrep)
         ("M-g g" . consult-grep)
         ("H-g"   . consult-git-grep)
         ("H-l"   . consult-focus-lines)
         ("M-y"   . consult-yank-pop)
         ("M-g m" . consult-mark)
         ("H-x f" . consult-find)
         ("M-O"   . consult-outline)
         ("H-x m" . consult-minor-mode-menu))
  :config (setq consult-preview-key "C-,")
  :custom (register-use-preview nil))

;; Teleport to any directory with a keystroke.
(use-package consult-dir
  :bind (("C-x C-d" . consult-dir))
  (:map vertico-map ("C-x C-j" . consult-dir-jump-file)))

;; Actions appear where your cursor lingers.
(use-package embark
  :bind (("C-." . embark-act) ("M-." . embark-dwim))
  :init (setq prefix-help-command #'embark-prefix-help-command))

;; Preview actions before they take flight.
(use-package embark-consult
  :after consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Get hints for your next move. Display available keybindings in context after a prefix.
(use-package which-key
  :init (which-key-mode))

;; Completion floats directly under your cursor.
(use-package corfu
  :init (global-corfu-mode)
  :hook ((corfu-mode . corfu-history-mode)
         (corfu-mode . corfu-indexed-mode)
         (corfu-mode . corfu-popupinfo-mode))
  :bind (:map corfu-map ("TAB" . nil) ("RET" . nil) ("C-t" . 'corfu-insert))
  :custom (corfu-auto t)) ; Enable auto popup

;; Add sources for completion.
(use-package cape
  :after corfu
  :hook ((org-mode LaTeX-mode) . rh/cape-dict)
  :demand t
  :config
  (setq cape-dict-file (list (concat user-emacs-directory "library/personal-dictionary")))
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (defun rh/cape-dict ()
    (add-to-list (make-local-variable 'completion-at-point-functions) #'cape-dict)))

(provide 'knot-completion)

;;; knot-editor.el --- description -*- lexical-binding: t; -*-

(use-package evil			:straight t :demand t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-visual-state-cursor 'hollow)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-disable-insert-state-bindings t)     ;; If set to t, Emacs keybindings are available in insert state
  (setq evil-undo-system 'undo-redo)              ;; Use the undo-redo system available in Emacs >= 28
  (setq evil-mode-line-format 'before)

  :config
  (evil-mode 1)
  (setq cursor-type 'box))

(use-package evil-collection		:straight t :demand t :after evil
  :config
  (evil-collection-init))

(use-package evil-surround		:straight t :demand t :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary		:straight t :demand t :after evil
  :config
  (evil-commentary-mode))

(use-package evil-nerd-commenter	:straight t :demand t :after evil
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

(use-package evil-snipe			:straight t :demand t  :after evil
  :init
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

(use-package evil-colemak-basics	:straight t :demand t :after evil
  :init
  (setq evil-colemak-basics-layout-mod 'mod-dh)
  :config
  (global-evil-colemak-basics-mode))

;; Toggle Evil (optional: re-init evil-collection)
;; (defun toggle-evil ()
;;   "Toggle Evil mode and related extensions globally."
;;   (interactive)
;;   (if (bound-and-true-p evil-mode)
;;       (progn
;;         (evil-mode -1)
;;         (global-evil-surround-mode -1)
;;         (evil-commentary-mode -1)
;;         (setq cursor-type 'bar)
;;         (message "Evil mode disabled."))
;;     (progn
;;       (evil-mode 1)
;;       (global-evil-surround-mode 1)
;;       (evil-commentary-mode 1)
;;       (evil-collection-init) ; optional: re-init collection
;;       (setq cursor-type 'box)
;;       (message "Evil mode enabled."))))

(provide 'knot-editor)

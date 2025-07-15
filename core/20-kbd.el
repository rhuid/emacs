;;; 20-kbd.el --- description -*- lexical-binding: t; -*-

(use-package evil			:straight t
  :init
  (setq evil-want-integration t)                  ;; This is optional since it's already set to t by default
  (setq evil-want-keybinding nil)
  (setq evil-visual-state-cursor 'hollow)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-disable-insert-state-bindings t)     ;; If set to t, Emacs keybindings are available in insert state
  (setq evil-undo-system 'undo-redo)              ;; Use the undo-redo system available in Emacs >= 28

  :config
  (evil-mode 1)
  (setq cursor-type 'box)
  )

(use-package evil-collection		:straight t :after evil
  :config
  (evil-collection-init))

(use-package evil-surround		:straight t :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary		:straight t :after evil
  :config
  (evil-commentary-mode))

(use-package evil-nerd-commenter	:straight t :after evil
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

(use-package evil-snipe			:straight t :after evil
  :init
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

(use-package evil-goggles		:straight t :disabled t
  :config
  (evil-goggles-mode)

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))

(use-package evil-colemak-basics	:straight t :after evil
  :init
  (setq evil-colemak-basics-layout-mod 'mod-dh)
  :config
  (global-evil-colemak-basics-mode))


;; ;; Toggle Evil (optional: re-init evil-collection)
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

;; (global-set-key (kbd "C-c e") #'toggle-evil)

;; (global-set-key (kbd "C-c r") #'replace-string)
;; (global-set-key (kbd "C-c w") #'delete-trailing-whitespace)
;; (global-set-key (kbd "C-c t c") #'company-mode)
;; (global-set-key (kbd "C-c t a") #'abbrev-mode)

;; (defun toggle-header-line ()
;;   "Toggle the header-line-format in the current buffer."
;;   (interactive)
;;   (setq header-line-format
;;         (if header-line-format
;;             nil
;;           ""))
;;   (redraw-display))  ;; optional: forces immediate refresh

;; (global-set-key (kbd "C-c t h") #'toggle-header-line)

;; (defun rh/clean-mode ()
;;   "Toggle clean-mode: header line and company mode."
;;   (interactive)
;;   (toggle-header-line)
;;   (company-mode 'toggle)
;;   (flymake-mode 'toggle))

;; (global-set-key (kbd "C-c c") #'rh/clean-mode)

(provide '20-kbd)

;;; knot-keybindings.el --- All keybindings with or without general.el -*- lexical-binding: t; -*-

;;;; General

(use-package general :straight t :demand t :after outline
  :config
  (general-create-definer rh/leader-keys
    :states '(normal visual)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (rh/leader-keys
    ;; the best and most used ones
    "SPC o"   'rh/toggle-org-scratch
    "SPC l"   'rh/toggle-lean-scratch

    ;; eshell
    "e t"     'rh/eshell-toggle

    ;; eval
    "e e"     'eval-expression

    ;; git (magit)
    "g s"     'magit-status

    ;; outline minor mode
    "o t"     'rh/outline-toggle-heading
    "o <tab>" 'rh/outline-toggle-heading
    "o a"     'rh/outline-toggle-visibility

    ;; string manipulation
    "s r"     'replace-string
    "s w"     'delete-trailing-whitespace
    "s a"     'abbrev-mode

    ;; vterm
    "t t"     'rh/vterm-toggle
    
    ;; utilities
    "u r"     'recentf-open-files
    ))

;; All custom keybindings

(define-key evil-normal-state-map (kbd "C-c b m") #'bookmark-set)
(define-key evil-normal-state-map (kbd "C-c b j") #'bookmark-jump)
(define-key evil-normal-state-map (kbd "C-c b l") #'list-bookmarks)

(define-key evil-normal-state-map (kbd "C-c e p") #'emms-pause)
(define-key evil-normal-state-map (kbd "C-c e s") #'emms-stop)
(define-key evil-normal-state-map (kbd "C-c e n") #'emms-next)
(define-key evil-normal-state-map (kbd "C-c e b") #'emms-previous)

(define-key evil-normal-state-map (kbd "C-x C-b") #'ibuffer)
(define-key evil-normal-state-map (kbd "C-x g")   #'magit-status)

(with-eval-after-load 'dired
  (defun rh/dired-evil-keys ()
    (define-key evil-normal-state-local-map
		(kbd "l") #'dired-display-file)

    (define-key evil-normal-state-local-map
		(kbd "m") #'dired-up-directory)

    (define-key evil-normal-state-local-map
		(kbd "i") #'rh/dired-open-file)

    (define-key evil-normal-state-local-map
		(kbd "C-c o") #'open-in-file-manager))
  
  (add-hook 'dired-mode-hook #'rh/dired-evil-keys))

(with-eval-after-load 'magit
  (defun rh/magit-evil-keys ()
    (define-key evil-normal-state-local-map
		(kbd "u") #'magit-unstage)

    (define-key evil-normal-state-local-map
		(kbd "U") #'magit-unstage-all))
  
  (add-hook 'magit-status-mode-hook #'rh/magit-evil-keys))

(provide 'knot-keybindings)

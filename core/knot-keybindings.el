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

(dolist (binding
	 '(("C-c b m" . bookmark-set)
	   ("C-c b j" . bookmark-jump)
	   ("C-c b l" . list-bookmarks)

	   ("C-c e p" . emms-pause)
	   ("C-c e s" . emms-stop)
	   ("C-c e n" . emms-next)
	   ("C-c e b" . emms-previous)

	   ("C-x C-b" . ibuffer)
	   ("C-x g"   . magit-status)))
  (define-key evil-normal-state-map (kbd (car binding)) (cdr binding)))

(with-eval-after-load 'dired
  (defun rh/dired-evil-keys ()
    (dolist (binding
	     '(("l" . dired-display-file)
	       ("m" . dired-up-directory)
	       ("i" . rh/dired-open-file)
	       ("C-c o" . open-in-file-manager)))
      (define-key evil-normal-state-local-map (kbd (car binding)) (cdr binding))))
  
  (add-hook 'dired-mode-hook #'rh/dired-evil-keys))

(with-eval-after-load 'magit
  (defun rh/magit-evil-keys ()
    (dolist (binding
	     '(("u" . magit-unstage)
	       ("U" . magit-unstage-all)))
      (define-key evil-normal-state-local-map (kbd (car binding)) (cdr binding))))
  
  (add-hook 'magit-status-mode-hook #'rh/magit-evil-keys))

(provide 'knot-keybindings)

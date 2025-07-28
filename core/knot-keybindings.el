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
    "o"       '(:ignore t :which-key "outline")
    "o t"     'rh/outline-toggle-heading
    "o <tab>" 'rh/outline-toggle-heading
    "o a"     'rh/outline-toggle-visibility

    ;; string manipulation
    "s"       '(:ignore t :which-key "string manipulation")
    "s r"     'replace-string
    "s w"     'delete-trailing-whitespace
    "s a"     'abbrev-mode

    ;; vterm
    "t t"     'rh/vterm-toggle
    
    ;; utilities
    "u"       '(:ignore t :which-key "utilities")
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
  (global-set-key (kbd (car binding)) (cdr binding)))

(with-eval-after-load 'dired
  (defun rh/dired-keys ()
    (dolist (binding
	     '(("l"     . dired-display-file)
	       ("m"     . dired-up-directory)
	       ("i"     . rh/dired-open-file)
	       ("d"     . dired-mark)
	       ("u"     . dired-unmark)
	       ("U"     . dired-unmark-all-marks)
	       ("C-c o" . open-in-file-manager)))
      (define-key evil-normal-state-local-map (kbd (car binding)) (cdr binding))))
  
  (add-hook 'dired-mode-hook #'rh/dired-keys))

(with-eval-after-load 'ibuffer
  (defun rh/ibuffer-keys ()
    (dolist (binding
	     '(("d" . ibuffer-mark-forward)
	       ("u" . ibuffer-unmark-forward)
	       ("U" . ibuffer-unmark-all-marks)
	       ("D" . ibuffer-do-kill-lines)
	       ))
      (define-key evil-normal-state-local-map (kbd (car binding)) (cdr binding))))
  
  (add-hook 'ibuffer-mode-hook #'rh/ibuffer-keys))

(with-eval-after-load 'magit
  (defun rh/magit-keys ()
    (dolist (binding
	     '(("u"     . magit-unstage)
	       ("U"     . magit-unstage-all)
	       ("C-c q" . rh/magit-quick-commit)
	       ("C-c a" . rh/magit-quick-amend)))
      (define-key evil-normal-state-local-map (kbd (car binding)) (cdr binding))))

  (add-hook 'magit-status-mode-hook #'rh/magit-keys))

(provide 'knot-keybindings)

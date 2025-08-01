;;; knot-keybindings.el --- All keybindings with or without general.el -*- lexical-binding: t; -*-

;;;; General

;; (use-package general :straight t :demand t :after outline
;;   :config
;;   (general-create-definer rh/leader-keys
;;     :states '(normal visual)
;;     :prefix "SPC"
;;     :global-prefix "C-SPC")

;;   (rh/leader-keys
;;     ;; the best and most used ones
;;     "SPC o"   'rh/toggle-org-scratch
;;     "SPC l"   'rh/toggle-lean-scratch

;;     ;; eshell
;;     "e t"     'rh/eshell-toggle

;;     ;; eval
;;     "e e"     'eval-expression

;;     ;; git (magit)
;;     "g s"     'magit-status

;;     ;; outline minor mode
;;     "o"       '(:ignore t :which-key "outline")
;;     "o t"     'rh/outline-toggle-heading
;;     "o <tab>" 'rh/outline-toggle-heading
;;     "o a"     'rh/outline-toggle-visibility

;;     ;; string manipulation
;;     "s"       '(:ignore t :which-key "string manipulation")
;;     "s r"     'replace-string
;;     "s w"     'delete-trailing-whitespace
;;     "s a"     'abbrev-mode

;;     ;; vterm
;;     "t t"     'rh/vterm-toggle

;;     ;; utilities
;;     "u"       '(:ignore t :which-key "utilities")
;;     "u r"     'recentf-open-files
;;     ))

;; All custom keybindings

(which-key-add-key-based-replacements
  "C-c o" "outline")

(dolist (binding
	 '(("C-c b m" . bookmark-set)
	   ("C-c b j" . bookmark-jump)
	   ("C-c b l" . list-bookmarks)

	   ("C-c e p" . emms-pause)
	   ("C-c e s" . emms-stop)
	   ("C-c e n" . emms-next)
	   ("C-c e b" . emms-previous)

	   ("C-c o t" . rh/outline-toggle-heading)
	   ("C-c o a" . rh/outline-toggle-visibility)

	   ("C-c s r" . replace-string)
	   ("C-c s w" . delete-trailing-whitespace)
	   ("C-c s a" . abbrev-mode)

	   ("C-c u r" . recentf-open-files)
	   ("C-c u s" . rh/eshell-toggle)
	   ("C-c u t" . rh/vterm-toggle)

	   ("C-x C-b" . ibuffer)
	   ("C-x g"   . magit-status)))
  (global-set-key (kbd (car binding)) (cdr binding)))

;; (with-eval-after-load 'dired
;;   (dolist (binding
;; 	   '(("l"     . dired-git-info-mode)
;; 	     ("m"     . dired-up-directory)
;; 	     ("i"     . rh/dired-open-file)
;; 	     ("d"     . dired-mark)
;; 	     ("u"     . dired-unmark)
;; 	     ("U"     . dired-unmark-all-marks)
;; 	     ("C-c o" . open-in-file-manager)))
;;     (meow-normal-define-key (kbd (car binding)) (cdr binding))))

(with-eval-after-load 'dired
  (defun rh/meow-dired-keys ()
    (meow-motion-define-key
     '("l" . dired-git-info-mode)
     '("m" . dired-up-directory)
     '("i" . rh/dired-open-file)
     '("d" . dired-mark)
     '("u" . dired-unmark)
     '("U" . dired-unmark-all-marks)
     '("C-c o" . open-in-file-manager)))
  (add-hook 'dired-mode-hook #'rh/meow-dired-keys))

;; (with-eval-after-load 'ibuffer
;;   (defun rh/ibuffer-keys ()
;;     (dolist (binding
;; 	     '(("d" . ibuffer-mark-forward)
;; 	       ("u" . ibuffer-unmark-forward)
;; 	       ("U" . ibuffer-unmark-all-marks)
;; 	       ("D" . ibuffer-do-kill-lines)
;; 	       ))
;;       (define-key evil-normal-state-local-map (kbd (car binding)) (cdr binding))))

;;   (add-hook 'ibuffer-mode-hook #'rh/ibuffer-keys))

;; (with-eval-after-load 'magit
;;   (defun rh/magit-keys ()
;;     (dolist (binding
;; 	     '(("u"     . magit-unstage)
;; 	       ("U"     . magit-unstage-all)
;; 	       ("C-c q" . rh/magit-quick-commit)
;; 	       ("C-c a" . rh/magit-quick-amend)))
;;       (define-key evil-normal-state-local-map (kbd (car binding)) (cdr binding))))

;;   (add-hook 'magit-status-mode-hook #'rh/magit-keys))

(provide 'knot-keybindings)

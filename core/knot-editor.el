;;; knot-editor.el --- Things about editing and keybindings -*- lexical-binding: t; -*-

;;;; Some functions for more efficient editing

(defun rh/join-line ()
  "Join the current line with the next line."
  (interactive)
  (save-excursion
    (end-of-line)
    (delete-char 1)
    (just-one-space)))

;;;; Prefixes for which-key

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "C-c b" "bookmark")
  (which-key-add-key-based-replacements "C-c e" "emms")
  (which-key-add-key-based-replacements "C-c o" "outline")
  (which-key-add-key-based-replacements "C-c s" "string-manipulation")
  (which-key-add-key-based-replacements "C-c u" "utilities"))

;;;; Global keys

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

	   ("C-c u g" . magit-status)
	   ("C-c u l" . rh/toggle-lean-scratch)
	   ("C-c u o" . rh/toggle-org-scratch)
	   ("C-c u r" . recentf-open-files)
	   ("C-c u s" . rh/eshell-toggle)
	   ("C-c u v" . rh/vterm-toggle)

	   ("C-x C-b" . ibuffer)))
  (global-set-key (kbd (car binding)) (cdr binding)))

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-colemak-dh)

  (meow-motion-define-key
   '(":" . execute-extended-command)
   '("e" . meow-prev)
   '("<escape>" . ignore))

  (meow-leader-define-key
   '("?" . meow-cheatsheet)
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument))

  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("1" . meow-expand-1)
   '("2" . meow-expand-2)
   '("3" . meow-expand-3)
   '("4" . meow-expand-4)
   '("5" . meow-expand-5)
   '("6" . meow-expand-6)
   '("7" . meow-expand-7)
   '("8" . meow-expand-8)
   '("9" . meow-expand-9)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("<" . meow-beginning-of-thing)
   '(">" . meow-end-of-thing)
   '("/" . meow-visit)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("e" . meow-prev)
   '("E" . meow-prev-expand)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-mark-word)
   '("H" . meow-mark-symbol)
   '("i" . meow-right)
   '("I" . meow-right-expand)
   '("j" . meow-join)
   '("k" . meow-kill)
   '("l" . meow-line)
   '("L" . meow-goto-line)
   '("m" . meow-left)
   '("M" . meow-left-expand)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("s" . meow-insert)
   '("S" . meow-open-above)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-search)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-delete)
   '("X" . meow-backward-delete)
   '("y" . meow-save)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)

   ;; extras
   '(":" . execute-extended-command)
   '("J" . rh/join-line)
   '("K" . kill-whole-line)))

(use-package meow :demand t
  :vc (:url "https://github.com/meow-edit/meow")
  :config
  (meow-setup)
  (meow-global-mode 1)
  )

(with-eval-after-load 'dired
  (defun rh/dired-keys ()
    (meow-motion-define-key
     '("g" . dired-git-info-mode)
     '("r" . dired-up-directory)
     '("i" . rh/dired-open-file)
     '("u" . dired-unmark)
     '("U" . dired-unmark-all-marks)
     '("C-c o" . open-in-file-manager)))
  (add-hook 'dired-mode-hook #'rh/dired-keys))

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

(with-eval-after-load 'magit
  (defun rh/magit-keys ()
    (meow-motion-define-key
     '("> c" . rh/magit-quick-commit)
     '("> a" . rh/magit-quick-amend)))
  (add-hook 'magit-mode-hook #'rh/magit-keys))

;; (defun rh/magit-keys ()
;;   (setq-local meow-motion-state-local-map
;;               (make-composed-keymap
;;                (meow--define-keys (make-sparse-keymap)
;; 				  '(": c" rh/magit-quick-commit)
;; 				  '(": a" rh/magit-quick-amend))
;;                meow-motion-state-local-map)))

;; (add-hook 'magit-mode-hook #'rh/magit-keys)

(provide 'knot-editor)

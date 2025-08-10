;;; knot-editor.el --- Things about editing and keybindings -*- lexical-binding: t; -*-

;;;; Some functions for more efficient editing

(defun rh/join-line ()
  "Join the current line with the next line, collapsing all whitespace between them to a single space."
  (interactive)
  (save-excursion
    (end-of-line)
    (delete-horizontal-space t)
    (delete-char 1)
    (just-one-space)))

;;;; Prefixes for which-key

(with-eval-after-load 'which-key
  (dolist (binding '(("C-c b" . "bookmark")
                     ("C-c e" . "emms")
		                 ("C-c o" . "outline")
		                 ("C-c s" . "string-manipulation")
		                 ("C-c u" . "utilities")))
    (which-key-add-key-based-replacements (car binding) (cdr binding))))

;;;; Global keys

(dolist (binding
	       '(("C-c k" . kill-buffer-and-window)
	         ("C-c w" . other-window)

	         ("C-c b m" . bookmark-set)
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
           ("C-c u m" . notmuch)
	         ("C-c u o" . rh/toggle-org-scratch)
	         ("C-c u r" . recentf-open-files)
	         ("C-c u s" . rh/eshell-toggle)
	         ("C-c u v" . rh/vterm-toggle)

	         ("C-x C-b" . ibuffer)))
  (global-set-key (kbd (car binding)) (cdr binding)))

;;;; Customizations to meow

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-colemak-dh)

  (meow-motion-define-key
   '(":" . mode-line-other-buffer)
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
   '(":" . mode-line-other-buffer)
   '("J" . rh/join-line)
   '("K" . kill-whole-line)))

(use-package meow
  :demand t
  :vc (:url "https://github.com/meow-edit/meow")
  :config
  (meow-setup)

  ;; Remove cursor in motion mode
  (setq meow-cursor-type-motion '(bar . 0))

  ;; Remove that annoying position hint while selecting
  (setq meow-expand-hint-remove-delay 0)

  (setq meow-use-clipboard t)
  (meow-global-mode 1))

(provide 'knot-editor)

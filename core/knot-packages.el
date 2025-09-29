;;; knot-packages.el --- Some great tools including magit and more -*- lexical-binding: t; -*-

;;; Things about windows

;; Make windows proportional while adding or deleting windows
(setq window-combination-resize t)

(use-package window
  :ensure nil
  :bind
  ("C-S-f" . follow-mode)
  ("<f9>"  . balance-windows)
  ("C-c k" . delete-window)
  ("C-c K" . kill-buffer-and-window)
  ("C-c n" . split-window-horizontally)
  ("C-c N" . split-window-vertically))

(use-package achievements
  :init (achievements-mode))

(use-package dictionary
  :bind (("C-c d l" . dictionary-lookup-definition)
         ("C-c d s" . dictionary-search)))

;;; `dictrus' --- https://github.com/rhuid/dictrus
(use-package emacs
  :bind ("C-c d d" . rh/dictrus-lookup)
  :config
  (defun rh/dictrus-lookup (word)
    "Look up WORD using the dictrus CLI and display the result in a buffer."
    (interactive (list (read-string "Lookup word: " (thing-at-point 'word t))))
    (let* ((buf (get-buffer-create "*Dictrus*"))
           (result (with-temp-buffer
                     (call-process "dictrus" nil t nil word)
                     (buffer-string))))
      (with-current-buffer buf
        (read-only-mode -1)
        (erase-buffer)
        (insert (format "Word: %s\n\n%s" word result))
        (goto-char (point-min))
        (view-mode 1)) ;; view-mode for easy quit
      (pop-to-buffer buf))))

;;; `electric-pair-mode`
;; Automatically insert matching delimiters (parentheses, quotes, braces, etc)
(use-package elec-pair
  :demand t
  :config (electric-pair-mode)
  :hook (org-mode . rh/org-electric-pairs)
  :custom (electric-pair-pairs '((?\(.?\)) (?\{.?\}) (?\[.?\])
                                 (?\".?\") (?\<.?\>)))
  :config
  (defun rh/org-electric-pairs ()
    "Org pairs for electric-pair-mode."
    (setq-local electric-pair-pairs (append '((?/.?/) (?_.?_) (?~.?~))))))

(use-package emms
  :bind (("C-c e p" . emms-pause)
         ("C-c e s" . emms-stop)
         ("C-c e n" . emms-next)
         ("C-c e b" . emms-previous))
  :vc (:url "https://git.savannah.gnu.org/git/emms.git")
  :config
  (setq emms-player-list '(emms-player-mpv)
        emms-source-file-default-directory "~/Downloads/DB Scores/")
  (require 'emms-setup)
  (emms-all) ;; or (emms-standard) for a lighter setup
  (require 'emms-player-mpv)
  (setq emms-mode-line-format " %s"
        emms-mode-line-titlebar-format "EMMS: %s")
  (emms-mode-line-mode 1))

;;; `expand-region'
;; Select regions by semantic units
(use-package expand-region
  :demand t
  :bind (("<backspace>"   . er/expand-region)
         ("S-<backspace>" . er/contract-region)))

;;;`keyfreq' : Track commands frequency
(use-package keyfreq
  :init (keyfreq-mode)
  :config (keyfreq-autosave-mode))

;;; `hippie-expand'
(use-package hippie-exp
  :bind ("C-S-e" . hippie-expand)
  :custom
  (hippie-expand-try-functions-list
   '(try-complete-file-name
     try-complete-file-name-partially
     try-expand-dabbrev-visible
     try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     try-expand-list
     try-expand-line
     try-expand-dabbrev-from-kill
     try-complete-lisp-symbol
     try-complete-lisp-symbol-partially
     try-expand-all-abbrevs)))

;;; `jinx' : https://github.com/minad/jinx
;; Requires enchant and dictionary backend
;; I am using `hunspell-en_us'
(use-package jinx
  :init (global-jinx-mode)
  :bind ("C-S-c" . jinx-correct)
  :custom (jinx-languages "en_US-large"))

(use-package magit
  :commands (magit-status magit-log)
  :bind ("C-x g" .  magit-status)
  (:map magit-mode-map
        ("." . rh/magit-quick-commit)
        ("," . rh/magit-quick-amend))
  :config
  (setq magit-display-buffer-function
	      #'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-restore-window-configuration-after-quit nil)
  (defun rh/magit-quick-commit ()
    "Prompt for a commit message in minibuffer and commit immediately."
    (interactive)
    (let ((msg (read-string "Commit message: ")))
      (magit-commit-create `("-m" ,msg))))
  (defun rh/magit-quick-amend ()
    "Quickly amend last commit with a new message via minibuffer."
    (interactive)
    (let ((msg (read-string "Amend message: ")))
      (magit-commit-create `("--amend" "-m" ,msg)))))

(use-package multiple-cursors
  :bind (("C-S-l" . mc/edit-lines)
         ("C-S-a" . mc/mark-all-like-this)
         ("C-S-n" . mc/mark-next-like-this)
         ("C-S-p" . mc/mark-previous-like-this)
         ("C->"   . mc/skip-to-next-like-this)
         ("C-<"   . mc/skip-to-previous-like-this)))

(use-package outline
  :bind (("C-S-t" . rh/outline-toggle-heading)
         ("C-S-o" . rh/outline-toggle-visibility))
  :hook
  (prog-mode . outline-minor-mode)
  (text-mode . outline-minor-mode)
  (outline-minor-mode . outline-show-all)
  (outline-minor-mode . outline-hide-body)
  :init
  ;; Set the keybinding prefix for built-in outline commands
  (setq outline-minor-mode-prefix (kbd "C-c @"))
  :config
  ;; Custom folding indicator (like +)
  (set-display-table-slot
   standard-display-table
   'selective-display
   (let ((face-offset (* (face-id 'shadow) (ash 1 22))))
     (vconcat (mapcar (lambda (c) (+ face-offset c)) " +"))))
  (defun rh/outline-toggle-heading ()
    "Toggle visibility of current outline heading."
    (interactive)
    (save-excursion
      (outline-back-to-heading)
      (if (outline-invisible-p (line-end-position))
          (outline-show-subtree)
        (outline-hide-subtree))))
  (defun rh/outline-toggle-visibility ()
    "Toggle between fully expanded and folded view of the outline buffer."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (if (cl-some (lambda (pos)
                     (goto-char pos)
                     (outline-invisible-p (line-end-position)))
                   (rh/outline-all-heading-positions))
          (outline-show-all)
        (outline-hide-body))))
  (defun rh/outline-all-heading-positions ()
    "Return a list of positions of all headings in the buffer."
    (let (positions)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward outline-regexp nil t)
          (push (point) positions)))
      positions)))

;;; Move where I mean
(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config (pdf-tools-install))

(use-package puni
  :hook ((prog-mode LaTeX-mode org-mode text-mode) . puni-mode))

;; Different color for each pair of parenthesis
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package sudo-edit
  :commands (sudo-edit))

(use-package tempel
  :disabled t
  :init (global-tempel-abbrev-mode)
  :bind (("M-/" . tempel-complete)))

;;; `vundo'
(use-package vundo
  :bind (("C-/"   . undo)
         ("C-x u" . vundo)
         ("M-U"   . undo-redo))
  :custom
  (vundo-compact-display t)
  (undo-limit (* 6 1024 1024)))

(use-package yasnippet
  :init (yas-global-mode)
  :custom (setq yas-snippet-dirs (list (concat user-emacs-directory "snippets"))))

(provide 'knot-packages)

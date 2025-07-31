;;; knot-built-ins.el --- tools which came built-in with emacs -*- lexical-binding: t; -*-

;; (require 'rh-capitalize)

(use-package emacs :straight nil :demand t
  :hook ((before-save . delete-trailing-whitespace)
	 (prog-mode   . glyphless-display-mode))
  :config

  ;; Automatically refresh the buffer when files change on disk
  (global-auto-revert-mode)

  ;; Automatic line wrapping
  (global-visual-line-mode)

  (global-hl-line-mode)
  (global-prettify-symbols-mode)
  (global-display-line-numbers-mode)
  (global-subword-mode)

  (blink-cursor-mode 0)
  (save-place-mode)
  (kill-ring-deindent-mode)

  ;; Don't generate backup files
  (setq make-backup-files nil)

  ;; Don't ask for confirmation while killing buffers
  (setq kill-buffer-query-functions nil)

  ;; All confirmations prompts be y or n
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Abbreviations
  (setq-default abbrev-mode t)
  (setq abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))
  (read-abbrev-file abbrev-file-name)
  (setq save-abbrevs 'silently)

  ;; Calendar
  (add-hook 'calendar-today-visible-hook #'calendar-mark-today)
  )

(use-package bookmark :straight nil :demand t
  :config
  (setq bookmark-save-flag 1)
  (setq bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory))
  (setq bookmark-bmenu-toggle-filenames t))

(use-package calc :straight nil
  :config
  (add-hook 'calc-trail-mode-hook 'evil-insert-state))

(use-package eww :straight nil
  :bind (("C-c w" . eww))
  :config
  (setq eww-search-prefix "https://duckduckgo.com/html/?q-")
  (setq shr-use-colors nil)
  (setq shr-width fill-column))

(use-package minibuffer :straight nil
  :hook (minibuffer-mode . savehist-mode)
  :custom (history-delete-duplicates t))

(use-package project :straight nil :demand t)

(use-package recentf :straight nil :demand t
  :init
  (recentf-mode 1)
  :custom
  (recentf-max-saved-items 100)
  (recentf-max-menu-items 25)
  (recentf-save-file (expand-file-name "recentf" user-emacs-directory))
  (recentf-auto-cleanup 'never)
  :config
  ;; Save recentf list every 5 minutes
  (run-at-time nil (* 5 60) #'recentf-save-list))

(use-package savehist :straight nil :demand t
  ;; Save minibuffer-history
  :init (savehist-mode)
  :custom
  (savehist-file (locate-user-emacs-file "history"))
  (history-length 2000)
  (savehist-additional-variables
   '(kill-ring
     register-alist
     search-ring
     regexp-search-ring)))

(provide 'knot-built-ins)

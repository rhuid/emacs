;;; knot-built-ins.el --- tools which came built-in with emacs -*- lexical-binding: t; -*-

(use-package emacs
  :demand t
  :ensure nil
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

  (setq-default cursor-type 'bar)
  (setq-default cursor-in-non-selected-windows nil)
  (blink-cursor-mode 0)

  ;; Automatically save place in each file
  (save-place-mode)

  ;; Remove indentation from text in kill-ring
  (kill-ring-deindent-mode)

  ;; Don't generate backup files
  (setq make-backup-files nil)

  ;; Don't ask for confirmation while killing buffers
  (setq kill-buffer-query-functions nil)

  ;; All confirmations prompts be y or n
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Calendar
  (add-hook 'calendar-today-visible-hook #'calendar-mark-today))

;;; Abbreviations
(use-package abbrev
  :ensure nil
  :init
  (defun rh/context-sensitive-abbrev-expand (fun &rest args)
    "Advice to prevent abbrev expansion inside comments and strings."
    (unless (nth 8 (syntax-ppss))
      (apply fun args)))

  (advice-add 'abbrev--default-expand :around #'rh/context-sensitive-abbrev-expand)

  :config
  (setq-default abbrev-mode t)
  (setq abbrev-file-name (expand-file-name "abbrev_defs.el" user-emacs-directory))
  (read-abbrev-file abbrev-file-name)
  (setq save-abbrevs 'silently))

;;; Bookmarks (Minato's Flying Raijin) are one of the many forms of teleportation in Emacs

(use-package bookmark
  :ensure nil
  :config
  (setq bookmark-save-flag 1)
  (setq bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory))
  (setq bookmark-bmenu-toggle-filenames t))

(use-package calc
  :ensure nil)

(use-package eldoc
  :ensure nil
  :commands eldoc-mode
  :config
  (setq eldoc-idle-delay 0.2))

(use-package eww
  :ensure nil
  :bind (("C-c w" . eww))
  :config
  (setq eww-search-prefix "https://duckduckgo.com/html/?q-")
  (setq shr-use-colors nil)
  (setq shr-width fill-column))

(use-package minibuffer
  :demand t
  :ensure nil
  :hook (minibuffer-mode . savehist-mode)
  :config (recentf-mode)
  :custom (history-delete-duplicates t))

(use-package project
  :demand t
  :ensure nil)

;;; Registers (a faster variant of Minato's Flying Raijin)

(use-package register
  :ensure nil
  :custom
  (register-preview-delay 0.2))

;;; `recentf'
;;; Time travel (teleport back to the past), made better with `consult-recent-file'

(use-package recentf
  :ensure nil
  :init
  (recentf-mode 1)
  :custom
  (recentf-max-saved-items 200)
  (recentf-max-menu-items 25)
  (recentf-save-file (expand-file-name "recentf" user-emacs-directory))
  (recentf-auto-cleanup 'never)
  :config
  ;; Save recentf list every 5 minutes
  (run-at-time nil (* 5 60) #'recentf-save-list))

(use-package savehist
  ;; Save minibuffer-history
  :demand t
  :ensure nil
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

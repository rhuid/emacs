;;; knot-defaults.el --- Some not-so-sane defaults? -*- lexical-binding: t; -*-

;;; `files'
(use-package files
  :ensure nil
  :demand t
  :hook (before-save . delete-trailing-whitespace)
  :config
  ;; Automatically save place in each file
  (save-place-mode)
  ;; Don't generate backup files
  (setq make-backup-files nil))

(use-package emacs
  :demand t
  :hook (prog-mode . glyphless-display-mode)

  :bind (("C-h"   . backward-delete-char)
         ("C-S-h" . backward-kill-word) ; Similar, but with Shift, it kills word instead
         ("C-S-d" . kill-word))
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

  ;; Remove indentation from text in kill-ring
  (kill-ring-deindent-mode)

  ;; Shift better reserved to be used as modifier
  (setq shift-select-mode nil)

  ;; Don't ask for confirmation while killing buffers
  (setq kill-buffer-query-functions nil)

  ;; All confirmations prompts be y or n
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Calendar
  (add-hook 'calendar-today-visible-hook #'calendar-mark-today))

;;; Abbreviations
;; An underrated killer feature, a double-edged sword, snippets on steroids
(use-package abbrev
  :ensure nil
  :bind ("C-c s a" . abbrev-mode)
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

;;; Calculator
(use-package calc
  :ensure nil)

;;; `eldoc'
(use-package eldoc
  :ensure nil
  :commands eldoc-mode
  :config
  (setq eldoc-idle-delay 0.2))

(use-package eww
  :ensure nil
  :bind (("C-c w w" . eww))
  :config
  (setq eww-search-prefix "https://duckduckgo.com/html/?q-")
  (setq shr-use-colors nil)
  (setq shr-width fill-column))

;;; Some global keys here instead of lots of `global-set-key'
(use-package keymap
  :ensure nil
  :bind
  ("C-S-t" . rh/outline-toggle-heading)
  ("C-S-o" . rh/outline-toggle-visibility)
  ("C-c e p" . emms-pause)
  ("C-c e s" . emms-stop)
  ("C-c e n" . emms-next)
  ("C-c e b" . emms-previous)

  ("C-c o b" . TeX-fold-buffer)
  ("C-c o B" . TeX-fold-clearout-buffer)

  ("C-c s r" . replace-string)
  ("C-c s w" . delete-trailing-whitespace)

  ("C-c u g" . magit-status)
  ("C-c u m" . notmuch)
  ("C-c u r" . recentf-open-files)
  ("C-c u s" . rh/eshell-toggle)
  ("C-c u v" . rh/vterm-toggle))

;;; `minibuffer'
(use-package minibuffer
  :demand t
  :ensure nil
  :hook (minibuffer-mode . savehist-mode)
  :config (recentf-mode)
  :custom (history-delete-duplicates t))

;;; `repeat'
;; Repeat commands without retyping the prefix key
(use-package repeat
  :ensure nil
  :init (repeat-mode)
  :custom (repeat-exit-timeout 5))

(provide 'knot-defaults)

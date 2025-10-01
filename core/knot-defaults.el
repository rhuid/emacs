;;; knot-defaults.el --- Some not-so-sane defaults? -*- lexical-binding: t; -*-

;;;; Concerning files
(setq-default require-final-newline t)
(save-place-mode) ; save place in each file
(setq make-backup-files nil)
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; A more sane join-line
(defun rh/join-line (&optional arg)
  "Like join-line but inverts its behavior."
  (interactive "P")
  (if arg (join-line nil) (join-line -1)))

(global-set-key (kbd "C-j") 'rh/join-line)

(global-set-key (kbd "C-\\") 'repeat)

;; Remap some of the basic and built-in commands time
;; Shift is better used as a modifier
(use-package keymap
  :ensure nil
  :config (setq shift-select-mode nil)
  :bind
  ("C-h"     . puni-backward-delete-char)
  ("C-S-h"   . puni-backward-kill-word)
  ("C-S-k"   . rh/backward-kill-line)
  ("C-x C-c" . nil)
  ("C-x r q" . save-buffers-kill-terminal)
  ("C-S-r"   . replace-string)
  ("M-L"     . duplicate-dwim)

  ("C-<backspace>" . mode-line-other-buffer)

  ;; Things about transposing
  ("C-t"   . nil)
  ("C-t w" . transpose-words)
  ("C-t c" . transpose-chars)
  ("C-t t" . transpose-chars)
  ("C-t d" . subword-transpose)
  ("C-t s" . transpose-sentences)
  ("C-t p" . transpose-paragraphs)
  ("C-t l" . transpose-lines)
  ("C-t x" . transpose-sexps)
  ("C-t r" . transpose-regions)

  ("C-c o b" . TeX-fold-buffer)
  ("C-c o B" . TeX-fold-clearout-buffer))

;;;; Concerning lines
(global-visual-line-mode)
(global-hl-line-mode)
(global-display-line-numbers-mode)
(setq-default fill-column 80)
(setq display-line-numbers-type 'relative)

;;;; Concerning cursors
(blink-cursor-mode 0)
(setq-default cursor-in-non-selected-windows nil)
(setq make-pointer-invisible t)

(use-package emacs
  :config
  (setq disabled-command-function nil) ; Enable all disabled commands
  (global-auto-revert-mode) ;; Refresh the buffer when files change on disk
  (global-subword-mode)
  (kill-ring-deindent-mode) ;; Remove indentation from text in kill-ring
  (setq kill-buffer-query-functions nil) ;; Don't ask for confirmation while killing buffers
  (fset 'yes-or-no-p 'y-or-n-p)) ;; All confirmations prompts be y or n

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

(use-package calendar
  :ensure nil
  :hook (calendar-today-visible . calendar-mark-today))

(use-package eldoc
  :ensure nil
  :commands eldoc-mode
  :config (setq eldoc-idle-delay 0.2))

(use-package minibuffer
  :demand t
  :ensure nil
  :hook (minibuffer-mode . savehist-mode)
  :config (recentf-mode)
  :custom (history-delete-duplicates t))

;;;; `minibuffer'
;; (add-hook 'minibuffer-mode-hook 'savehist-mode)
;; (recentf-mode)
;; (history-delete-duplicates t)

;; Repeat commands without retyping the prefix key
(use-package repeat
  :ensure nil
  :init (repeat-mode)
  :custom (repeat-exit-timeout 5))

;;; Date formats for use in `yasnippet'
(defun rh/date-format-candidates ()
  "Return an alist of (display . format-string) for yasnippet date choices."
  (mapcar (lambda (fmt)
            (cons (format "%-20s → %s" fmt (format-time-string fmt)) fmt))
          '("%Y-%m-%d"          ;; 2025-09-19
            "%d/%m/%Y"          ;; 19/09/2025
            "%A, %B %d, %Y"     ;; Friday, September 19, 2025
            "%b %d, %Y"         ;; Sep 19, 2025
            "%Y-%m-%d %H:%M"))) ;; 2025-09-19 20:31

(provide 'knot-defaults)

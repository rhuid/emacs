;;; knot-defaults.el --- Some not-so-sane defaults? -*- lexical-binding: t; -*-

(use-package files
  :ensure nil
  :demand t
  :hook (before-save . delete-trailing-whitespace)
  :config
  (setq-default require-final-newline t)
  ;; Automatically save place in each file
  (save-place-mode)
  ;; Don't generate backup files
  (setq make-backup-files nil))

;; Remap some of the basic and built-in commands time
;; Shift is better used as a modifier
(use-package keymap
  :ensure nil
  :config (setq shift-select-mode nil)
  ;; :hook (after-init . rh/ensure-region)
  :bind
  ("C-h"     . backward-delete-char)
  ("C-S-h"   . puni-backward-kill-word)
  ("C-S-k"   . rh/backward-kill-line)
  ("C-S-d"   . puni-forward-kill-word)
  ("C-x C-c" . nil)
  ("C-x r q" . save-buffers-kill-terminal)
  ("C-S-r"   . replace-string)
  ("C-w"     . rh/kill-in-context)
  ("M-j"     . nil)
  ("M-j"     . rh/join-line)
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
(setq-default fill-column 80)
(global-visual-line-mode)
(global-hl-line-mode)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(use-package emacs
  :hook (prog-mode . glyphless-display-mode)
  :config
  (setq-default cursor-type t)
  (setq-default cursor-in-non-selected-windows nil)
  (blink-cursor-mode 0)
  ;; Disable mouse pointer as while typing
  (setq make-pointer-invisible t)
  ;; Enable all disabled commands
  (setq disabled-command-function nil)
  ;; Automatically refresh the buffer when files change on disk
  (global-auto-revert-mode)
  (global-subword-mode)
  ;; Remove indentation from text in kill-ring
  (kill-ring-deindent-mode)
  ;; Don't ask for confirmation while killing buffers
  (setq kill-buffer-query-functions nil)
  ;; All confirmations prompts be y or n
  (fset 'yes-or-no-p 'y-or-n-p))

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

(use-package calc
  :ensure nil
  :bind ("C-c c c" . calc))

(use-package calendar
  :ensure nil
  :bind ("C-c x c" . calendar)
  :hook (calendar-today-visible . calendar-mark-today))

(use-package eldoc
  :ensure nil
  :commands eldoc-mode
  :config (setq eldoc-idle-delay 0.2))

(use-package eww
  :ensure nil
  :bind (("C-c w w" . eww))
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

;;; knot-defaults.el --- Some defaults which are supposed to be sane -*- lexical-binding: t; -*-

;; Concerning files
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-auto-revert-mode) ; Refresh the buffer when files change on disk
(save-place-mode) ; Save place in each file
(setq make-backup-files nil)
(setq-default require-final-newline t)

;; Concerning kills
(delete-selection-mode) ; Typing on a region replaces it
(kill-ring-deindent-mode)
(setq kill-buffer-query-functions nil) ; Don't ask for confirmation while killing buffers
(global-display-line-numbers-mode)

;; Concerning lines, sentences, words and characters
(setq kill-do-not-save-duplicates t)
(global-hl-line-mode)
(global-subword-mode)
(global-visual-line-mode)
(setq sentence-end-double-space nil) ; A sentence should not need to end in double spaces.
(setq display-line-numbers-type 'relative)
(setq-default fill-column 80)

;; Concerning mouse, cursors and scrolling
(blink-cursor-mode 0)
(pixel-scroll-precision-mode)
(setq make-pointer-invisible t)
(setq mouse-yank-at-point t)
(setq scroll-preserve-screen-position t)
(setq-default cursor-in-non-selected-windows nil)

;; Concerning windows
(setq window-combination-resize t) ; keep windows balanced
(winner-mode) ; undo window configurations
(global-set-key (kbd "C-<backspace>") 'winner-undo)
(setq winner-boring-buffers
      '("*Messages*" "*Completions*" "*Buffer List*" "*Async-native-compile-log*" "*scratch*"))

;; Make it more convenient (concerning convenience?)
(setq disabled-command-function nil) ; Enable all disabled commands. I (kinda) know what I am doing.
(setq use-short-answers t) ; All confirmations prompts be y or n
(repeat-mode) ; Repeat commands without retyping the prefix key
(setq repeat-exit-timeout 5)
(setq echo-keystrokes 0.1)

;; Date Formats for use in `yasnippet'
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

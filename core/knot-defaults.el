;;; knot-defaults.el --- Some defaults which are supposed to be sane -*- lexical-binding: t; -*-

;; Concerning files
(add-hook 'before-save-hook 'delete-trailing-whitespace) ; delete trailing whitespace at saving
(global-auto-revert-mode) ; refresh the buffer when files change on disk
(save-place-mode) ; save place in each file
(setq make-backup-files nil) ; don't make backup files
(setq-default require-final-newline t) ; ensure a final newline at saving

;; Concerning kills
(delete-selection-mode) ; typing on a region replaces it
(kill-ring-deindent-mode)
(setq kill-buffer-query-functions nil) ; don't ask for confirmation while killing buffers
(setq kill-do-not-save-duplicates t)
(setq kill-whole-line t) ; `C-k' kills the whole line, if the point is at the beginning of the line

;; Concerning lines, sentences, words and characters
(global-display-line-numbers-mode)
(global-hl-line-mode)
(global-subword-mode)
(global-visual-line-mode)
(setq display-line-numbers-type 'relative)
(setq sentence-end-double-space nil) ; a sentence should not need to end in double spaces
(setq-default fill-column 80)

;; Concerning mouse, cursors and scrolling
(blink-cursor-mode 0)
(pixel-scroll-precision-mode) ; make mouse scrolling smoother
(setq make-pointer-invisible t) ; hide the mouse cursor while typing
(setq mouse-yank-at-point t)
(setq scroll-preserve-screen-position t)
(setq-default cursor-in-non-selected-windows nil) ; hide cursor/point on non-active windows

;; Concerning windows
(setq window-combination-resize t) ; keep windows balanced
(winner-mode) ; undo window configurations
(bind-key "H-<tab>" 'winner-undo)
(setq winner-boring-buffers
      '("*Messages*" "*Completions*" "*Buffer List*" "*Async-native-compile-log*" "*scratch*"))

;; Make it more convenient (concerning convenience?)
(setq next-line-add-newlines t) ; `C-n' inserts newline at the end of buffer instead of throwing error
(setq disabled-command-function nil) ; enable all disabled commands, I (kinda) know what I am doing
(setq use-short-answers t) ; all confirmations prompts be y or n
(repeat-mode) ; repeat commands without retyping the prefix key
(setq repeat-exit-timeout 5)
(setq echo-keystrokes 0.1) ; display keystrokes in the echo area faster
(setq confirm-kill-processes nil)

;; Make it less irritating (concerning irritation?)
(setq suggest-key-bindings nil)

(provide 'knot-defaults)

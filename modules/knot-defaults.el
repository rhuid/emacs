;;; knot-defaults.el --- Some defaults which are supposed to be sane -*- lexical-binding: t; -*-

;; Concerning files
(add-hook 'before-save-hook 'delete-trailing-whitespace)             ; delete trailing whitespace at saving
(global-auto-revert-mode)                                            ; refresh the buffer when files change on disk
(save-place-mode)                                                    ; save place in each file
(setq make-backup-files nil)                                         ; don't make backup files
(setq-default require-final-newline t)                               ; ensure a final newline at saving

;; Concerning kills
(delete-selection-mode)                                              ; typing on a region replaces it
(kill-ring-deindent-mode)                                            ; remove indentation while saving to the kill ring
(setq kill-buffer-query-functions nil)                               ; don't ask for confirmation while killing buffers
(setq kill-do-not-save-duplicates t)                                 ; don't add duplicates to the kill king
(setq kill-whole-line t)                                             ; `C-k' at the start of a line kills the whole line

;; Concerning lines, sentences, words and characters
(global-display-line-numbers-mode)                                   ; line numbers everywhere please
(global-hl-line-mode)                                                ; highlight current line
(global-subword-mode)
(global-visual-line-mode)
(setq display-line-numbers-type 'relative)                           ; relative line numbering, yes!
(setq sentence-end-double-space nil)                                 ; a sentence should not need to end in double spaces
(setq-default fill-column 80)

;; Concerning mouse, cursors and scrolling
(blink-cursor-mode 0)                                                ; cursor should not blink
(pixel-scroll-precision-mode)                                        ; make mouse scrolling smoother
(setq make-pointer-invisible t)                                      ; hide the mouse cursor while typing
(setq mouse-yank-at-point t)
(setq scroll-preserve-screen-position t)                             ; while scrolling, try to keep the point unchanged
(setq-default cursor-in-non-selected-windows nil)                    ; hide cursor/point on non-active windows

;; Concerning windows
(setq window-combination-resize t)                                   ; keep windows balanced
(winner-mode)                                                        ; undo window configurations
(bind-key "H-<tab>" 'winner-undo)

;; Make it more convenient (concerning convenience?)
(setq
 next-line-add-newlines t                                            ; `C-n', at buffer end, inserts newline
 disabled-command-function nil                                       ; enable all disabled commands, I know what I am doing
 use-short-answers t                                                 ; all confirmations prompts be y or n
 echo-keystrokes 0.1                                                 ; display keystrokes in the echo area faster
 confirm-kill-processes nil
 suggest-key-bindings nil)                                           ; don't show equivalent keybindings when `M-x' has one

;; Some nice minor modes
(display-time-mode)                                                  ; I want to know the time from the mode-line
(repeat-mode)                                                        ; repeat commands without retyping the prefix key
(setq repeat-exit-timeout 5)                                         ; no repeat after 5 seconds

(display-startup-echo-area-message)

(provide 'knot-defaults)

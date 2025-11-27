;;; knot-defaults.el --- Sane defaults and built-in packages -*- lexical-binding: t; -*-

;; Concerning files
(add-hook 'before-save-hook 'delete-trailing-whitespace)          ; delete trailing whitespace at saving
(global-auto-revert-mode)                                         ; refresh the buffer when files change on disk
(save-place-mode)                                                 ; save place in each file
(setq make-backup-files nil)                                      ; don't make backup files
(setq-default require-final-newline t)                            ; ensure a final newline at saving

;; Concerning kills
(kill-ring-deindent-mode)                                         ; remove indentation while saving to the kill ring
(setq kill-buffer-query-functions nil)                            ; don't ask for confirmation while killing buffers
(setq kill-do-not-save-duplicates t)                              ; don't add duplicates to the kill king

;; Concerning lines, sentences, words and characters
(global-display-line-numbers-mode)                                ; line numbers everywhere please
(global-hl-line-mode)                                             ; highlight current line
(global-subword-mode)
(global-visual-line-mode)
(setq display-line-numbers-type 'relative)                        ; relative line numbering, yes!
(setq display-line-numbers-width-start t )                        ; count number of lines to use for line number width
(setq sentence-end-double-space nil)                              ; a sentence should not need to end in double spaces
(setq-default fill-column 80)

;; Concerning mouse, cursors and scrolling
(blink-cursor-mode 0)                                             ; cursor should not blink
(pixel-scroll-precision-mode)                                     ; make mouse scrolling smoother
(setq make-pointer-invisible t)                                   ; hide the mouse cursor while typing
(setq mouse-yank-at-point t)                                      ; mouse yank commands yank at point (not at click)
(setq scroll-preserve-screen-position t)                          ; while scrolling, try to keep the point unchanged
(setq-default cursor-in-non-selected-windows nil)                 ; hide cursor/point on non-active windows

;; Concerning the mode-line
(display-time-mode)                                               ; I want to know the time from the mode-line
(setq display-time-day-and-date t)                                ; display date as well
(display-battery-mode)                                            ; display battery level in the mode-line

;; Concerning windows
(setq window-combination-resize t)                                ; keep windows balanced
(winner-mode)                                                     ; undo window configurations
(bind-key "H-<tab>" 'winner-undo)

;; Make it more convenient (concerning convenience?)
(setq use-dialog-box nil)                                         ; no dialog box, please
(setq next-line-add-newlines t)                                   ; `C-n' adds newline, avoid `end-of-buffer' error
(setq disabled-command-function nil)                              ; enable all disabled commands, I know what I am doing
(setq use-short-answers t)                                        ; all confirmations prompts be y or n
(setq echo-keystrokes 0.1)                                        ; display keystrokes in the echo area faster
(setq idle-update-delay 0.1)                                      ; update things on screen faster after typing
(setq confirm-kill-procetsses nil)                                ; don't confirm killing processes on exit
(setq shift-select-mode nil)                                      ; we have a better use of `Shift' as modifier key
(setq suggest-key-bindings nil)                                   ; don't show equivalent keybindings when `M-x' has one

;; Concerning editing
(setq duplicate-line-final-position   1)                          ; move point to the first new line
(setq duplicate-region-final-position 1)                          ; put the region around the first copy
(setq delete-pair-blink-delay 0)                                  ; heck, why would I want any delay?

;; `isearch' --- Search as you type, and watch your buffer follow
(setq isearch-allow-scroll 'unlimited)                            ; scroll as much as you please
(setq isearch-lazy-count t)                                       ; show number of matches in the mode-line
(setq isearch-repeat-on-direction-change t)                       ; allow switching direction
(setq search-default-mode 'char-fold-to-regexp)                   ; match accented letters too
(setq search-whitespace-regexp ".*?")                             ; type "t n" to match "teleportation"

;; Minor modes
(abbrev-mode)                                                     ; let Emacs finish your phrases, because typing is hard work
(delete-selection-mode)                                           ; typing on a region replaces it
(global-eldoc-mode)                                               ; a whisper of documentation watches over you as you type
(electric-pair-mode)                                              ; auto-insert the closing delimiter
(global-goto-address-mode)                                        ; make URLs and email addresses clickable
(global-prettify-symbols-mode)                                    ; pretty math symbols
(add-hook 'prog-mode-hook 'outline-minor-mode)                    ; fold and bloom your buffer's hierarchy
(show-paren-mode)                                                 ; highlight matching parenthesis
(repeat-mode)                                                     ; repeat commands without retyping the prefix key
(recentf-mode)                                                    ; save recent files
(savehist-mode)                                                   ; save minibuffer history

;; Customizations for some minor modes
(setq read-abbrev-file        abbrev-file-name)
(setq save-abbrevs            'silently)
(setq eldoc-idle-delay        0.2)
(setq repeat-exit-timeout     5)
(setq recentf-max-saved-items 200)
(setq recentf-max-menu-items  25)
(setq show-paren-delay        0.01)
(setq history-length          2000)
(setq history-delete-duplicates t)
(setq savehist-additional-variables '(kill-ring))

;; Prevent abbrev expansion inside comments and strings
(advice-add 'abbrev--default-expand :around (lambda (fun &rest args) (unless (nth 8 (syntax-ppss)) (apply fun args))))

(provide 'knot-defaults)

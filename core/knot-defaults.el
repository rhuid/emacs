;;; knot-defaults.el --- Some sane and not-so-sane defaults? -*- lexical-binding: t; -*-

;; Use `C-h' for `DEL' (backspace).
(define-key key-translation-map [?\C-h] [?\C-?])

;; Arrows keys are wrapped with Meta! Useful for `move-text' and promoting/demoting headings in `org-mode'
(define-key key-translation-map [left] (kbd "M-<left>"))
(define-key key-translation-map [right] (kbd "M-<right>"))
(define-key key-translation-map [up] (kbd "M-<up>"))
(define-key key-translation-map [down] (kbd "M-<down>"))

;; Detach `C-i' from `TAB'
(define-key input-decode-map "\C-i" [Ci]) ; this alone won't apply to client frames, so we need the line below
(add-hook 'after-make-frame-functions
          '(lambda (frame) (with-selected-frame frame (define-key input-decode-map "\C-i" [Ci]))))

;; Disable return (enter) key and backspace. Use `C-m' and `C-h' instead.
(global-set-key (kbd "<return>") 'ignore)
(global-set-key (kbd "<backspace>") 'ignore)

(setq shift-select-mode nil) ; Shift is better used as a modifier

;; Readjusting some built-in keybindings
(global-set-key (kbd "M-z")     'zap-up-to-char)
(global-set-key (kbd "M-D")     'duplicate-dwim)
(global-set-key (kbd "M-l")     'copy-from-above-command)
(global-set-key (kbd "C-S-r")   'replace-string)
(global-set-key (kbd "C-x C-a") 'align-regexp)
(global-set-key (kbd "C-x C-u") 'upcase-dwim)
(global-set-key (kbd "C-x C-l") 'downcase-dwim)
(global-set-key (kbd "C-x C-c") 'capitalize-dwim)
(global-set-key (kbd "C-x C-t") 'transpose-sentences) ; `transpose-lines' has been taken care of by `move-text'
(global-set-key (kbd "C-S-t")   'transpose-paragraphs)
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "M-s l")   'sort-lines) ; `M-s' is overloaded by default
(global-set-key (kbd "M-n")     'forward-paragraph)  ; Think of `n'ext paragraph
(global-set-key (kbd "M-p")     'backward-paragraph) ; Think of `p'revious paragraph

;; A more sensible `join-line' (also accepts universal argument)
(defun rh/join-line (&optional arg)
  "Like join-line but inverts its behavior."
  (interactive "P")
  (if arg (join-line nil) (join-line -1)))

(global-set-key (kbd "C-j") 'rh/join-line)

;; Some bunch of advice
(advice-add 'duplicate-dwim :after (lambda (&rest _args) (next-line)))

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
(setq kill-do-not-save-duplicates t)

;; Concerning lines, sentences, words and characters
(global-display-line-numbers-mode)
(global-hl-line-mode)
(global-subword-mode)
(global-visual-line-mode)
(setq sentence-end-double-space nil) ; A sentence should not need to end in double spaces.
(setq display-line-numbers-type 'relative)
(setq-default fill-column 80)

;; Concerning mouse and cursors
(blink-cursor-mode 0)
(pixel-scroll-precision-mode)
(setq make-pointer-invisible t)
(setq mouse-yank-at-point t)
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

;; Date formats for use in `yasnippet'
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

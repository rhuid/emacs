;;; knot-defaults.el --- Some sane and not-so-sane defaults? -*- lexical-binding: t; -*-

;; Use `C-h' for `DEL' (backspace).
(define-key key-translation-map [?\C-h] [?\C-?])

;; Arrows keys are wrapped with Meta! Beware!
;; Useful for `move-text' and promoting/demoting headings in `org-mode'
(define-key key-translation-map [left] (kbd "M-<left>"))
(define-key key-translation-map [right] (kbd "M-<right>"))
(define-key key-translation-map [up] (kbd "M-<up>"))
(define-key key-translation-map [down] (kbd "M-<down>"))

;; Detach `C-i' from `TAB' (won't work in client frames, need separate solution given below)
(define-key input-decode-map "\C-i" [Ci])

(defun rh/detach-Ci-from-TAB (frame)
  "Detach `C-i' from `TAB' in the current frame."
  (with-selected-frame frame (define-key input-decode-map "\C-i" [Ci])))

;; Need to run `rh/detach-Ci-from-TAB' whenever a new frame is created.
(add-hook 'after-make-frame-functions 'rh/detach-Ci-from-TAB)

;; Disable return (enter) key and backspace. Use `C-m' and `C-h' instead.
(global-set-key (kbd "<return>") 'ignore)
(global-set-key (kbd "<backspace>") 'ignore)

;; Concerning files
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(global-auto-revert-mode) ; Refresh the buffer when files change on disk
(save-place-mode) ; Save place in each file
(setq make-backup-files nil)
(setq-default require-final-newline t)

;; A more sensible `join-line' (also accepts universal argument)
(defun rh/join-line (&optional arg)
  "Like join-line but inverts its behavior."
  (interactive "P")
  (if arg (join-line nil) (join-line -1)))

(global-set-key (kbd "C-j") 'rh/join-line)

;; Readjusting some built-in keybindings
(use-package keymap
  :ensure nil
  :config (setq shift-select-mode nil) ; Shift is better used as a modifier
  :bind
  ;; Make text editing faster
  ("M-z" . zap-up-to-char)
  ("M-D" . duplicate-dwim)
  ("M-l" . copy-from-above-command)
  ("C-x C-a" . align-regexp)

  ("C-\\"    . repeat)
  ("C-S-r"   . replace-string)
  ("C-x r q" . save-buffers-kill-terminal)
  ("S-<backspace>" . mode-line-other-buffer)

  ;; Changing case made easier. Also free up `M-u', `M-l' and `M-c'
  ("C-x C-u" . upcase-dwim)
  ("C-x C-l" . downcase-dwim)
  ("C-x C-c" . capitalize-dwim)

  ;; `transpose-lines' has been taken care of by `move-text'
  ("C-x C-t" . transpose-sentences)
  ("C-S-t"   . transpose-paragraphs))

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

;; Make it more convenient
(setq disabled-command-function nil) ; Enable all disabled commands. I (kinda) know what I am doing.
(setq use-short-answers t) ; All confirmations prompts be y or n

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
  (setq abbrev-file-name (expand-file-name "library/abbrevs.el" user-emacs-directory))
  (read-abbrev-file abbrev-file-name)
  (setq save-abbrevs 'silently))

(use-package register
  :ensure nil
  :custom (register-use-preview nil)) ; preview without delay

;; Repeat commands without retyping the prefix key
(repeat-mode)
(setq repeat-exit-timeout 5)

;; Some bunch of advice
(advice-add 'duplicate-dwim :after (lambda (&rest _args) (next-line)))

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

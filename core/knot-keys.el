;;; knot-keys.el --- Some extra global keys for the thermonuclear editor -*- lexical-binding: t; -*-

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

;; https://github.com/magnars/emacsd-reboot/blob/main/settings/tooling.el
(defmacro lamb (&rest body)
  "Shorthand for interactive lambdas that ignore errors."
  `(lambda ()
     (interactive)
     (ignore-errors
       ,@body)))

(setq shift-select-mode nil) ; Shift is better used as a modifier

;; Readjusting some built-in keybindings
(global-set-key (kbd "M-z")     'zap-up-to-char)
(global-set-key (kbd "C-x C-u") 'upcase-dwim)
(global-set-key (kbd "C-x C-l") 'downcase-dwim)
(global-set-key (kbd "C-x C-c") 'capitalize-dwim)
(global-set-key (kbd "C-x C-t") 'transpose-sentences) ; `transpose-lines' has been taken care of by `move-text'
(global-set-key (kbd "C-S-t")   'transpose-paragraphs)
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)

;; A faster `C-n' and `C-p'
(global-set-key (kbd "M-n") (lamb (forward-line 5) (recenter)))
(global-set-key (kbd "M-p") (lamb (previous-line 5) (recenter)))

;; Paragraph navigation: `N'ext paragraph and `P'revious paragraph
(global-set-key (kbd "M-N") 'forward-paragraph)
(global-set-key (kbd "M-P") 'backward-paragraph)

;; These amazing commands come built-in but unbound. So why not bind them?
(global-set-key (kbd "M-A")   'copy-from-above-command)
(global-set-key (kbd "M-D")   'duplicate-dwim)
(global-set-key (kbd "M-|")   'delete-all-space) ; Big brother to the built-in `M-\' : delete-horizontal-space
(global-set-key (kbd "M-s a") 'align-regexp)
(global-set-key (kbd "M-s l") 'sort-lines)
(global-set-key (kbd "M-s r") 'replace-string)
(global-set-key (kbd "M-s d") 'delete-duplicate-lines)

;; The following should have been loaded by default without needing to load `org-mode'
(global-set-key (kbd "C-+") 'org-increase-number-at-point)
(global-set-key (kbd "C-_") 'org-decrease-number-at-point)

;; Best used in conjunction with er/expand-region
(global-set-key (kbd "M-s p") 'delete-pair)
(setq delete-pair-blink-delay 0) ; Heck, why would I want any delay?

;; A more sensible `join-line' (also accepts universal argument)
(defun rh/join-line (&optional arg)
  "Like join-line but inverts its behavior."
  (interactive "P")
  (if arg (join-line nil) (join-line -1)))

(global-set-key (kbd "C-j") 'rh/join-line)

;; Some bunch of advice
(advice-add 'duplicate-dwim :after (lambda (&rest _args) (next-line)))

(provide 'knot-keys)

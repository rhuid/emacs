;;; knot-keys.el --- Some extra global keys for the thermonuclear editor -*- lexical-binding: t; -*-

;; A lot of great commands come built-in but unbound. So why not bind them?
;; Some of the default keybindings are also be adjusted.
;; But before that, we do some low-level key remapping first.

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
     "This is an anonymous command defined using `lamb'. Check config for more info."
     (interactive)
     (ignore-errors
       ,@body)))

;; A prefix key for toggling `m'inor modes
(define-prefix-command 'toggle-minor-mode-map)
(global-set-key (kbd "C-x m") 'toggle-minor-mode-map)

;; Some built-in modes
(define-key toggle-minor-mode-map (kbd "f") 'follow-mode)

;; We have a better use of `Shift'
(setq shift-select-mode nil)

;; The built-in repeat `C-x z' is cumbersome even with repeat (no pun intended).
(global-set-key (kbd "C-:") 'repeat)

;; The default command bound to `M-z' is `zap-to-char'. However, the below makes more sense.
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; Changing case made easy. Do what I mean!
(global-set-key (kbd "C-x C-u") 'upcase-dwim)
(global-set-key (kbd "C-x C-l") 'downcase-dwim)
(global-set-key (kbd "C-x C-c") 'capitalize-dwim)

;; Transposing things around: `transpose-lines' has been taken care of by `move-text'.
(global-set-key (kbd "C-x C-t") 'transpose-sentences)
(global-set-key (kbd "C-S-t")   'transpose-paragraphs)

;; Need for Speed: Shift... Make `C-n', `C-p', `C-f' and `C-b' faster
(global-set-key (kbd "C-S-n") (lamb (forward-line 5) (recenter)))
(global-set-key (kbd "C-S-p") (lamb (previous-line 5) (recenter)))
(global-set-key (kbd "C-S-f") (lamb (forward-char 5) (recenter)))
(global-set-key (kbd "C-S-b") (lamb (backward-char 5) (recenter)))

;; Need for Speed: Shift (more shift for more speed)
(global-set-key (kbd "M-A") 'copy-from-above-command)
(global-set-key (kbd "M-D") 'duplicate-dwim)
(global-set-key (kbd "M-F") (lamb (forward-word 4))) ; `M-f' on steroids
(global-set-key (kbd "M-B") (lamb (backward-word 4))) ; `M-b' on steroids
(global-set-key (kbd "M-|") 'delete-all-space) ; big brother to the built-in `M-\\' : `delete-horizontal-space'

;; Paragraph navigation: `n'ext paragraph and `p'revious paragraph
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

;; The prefix `M-s' is well placed on the home row and is criminally underused. Why not redeem it?
;; And make it mnemonic: `M-s' for `M'anipulate-`s'tring
(global-set-key (kbd "M-s a") 'align-regexp)
(global-set-key (kbd "M-s d") 'delete-duplicate-lines)
(global-set-key (kbd "M-s k") 'keep-lines)
(global-set-key (kbd "M-s l") 'sort-lines)
(global-set-key (kbd "M-s r") 'replace-string)

;; Best used in conjunction with er/expand-region
(global-set-key (kbd "M-s p") 'delete-pair)
(setq delete-pair-blink-delay 0) ; Heck, why would I want any delay?

;; The following should have been universal without needing to load `org-mode'
(global-set-key (kbd "C-+") 'org-increase-number-at-point)
(global-set-key (kbd "C-_") 'org-decrease-number-at-point)

;; Keyboard macros made easier
(global-set-key (kbd "C-(") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "C-)") 'kmacro-end-or-call-macro)

;; Used rarely, but it's nice to keep them bound, because I don't wanna do M-x and type the name again
(global-set-key (kbd "S-<f2>") 'rename-visited-file) ; a homage to GUI file managers' <f2> renaming
(global-set-key (kbd "S-<f5>") 'recover-this-file)

;; Because the default `C-x C-c' is too easy to reach.
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)

;; A more sensible `join-line' (also accepts universal argument)
(defun rh/join-line (&optional arg)
  "Like join-line but inverts its behavior."
  (interactive "P")
  (if arg (join-line nil) (join-line -1)))

(global-set-key (kbd "C-j") 'rh/join-line)

;; Some bunch of advice
(advice-add 'duplicate-dwim :after (lambda (&rest _args) (next-line)))

(provide 'knot-keys)

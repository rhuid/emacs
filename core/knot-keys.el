;;; knot-keys.el --- Some extra global keys for the thermonuclear editor -*- lexical-binding: t; -*-

;; A lot of great commands come built-in but unbound. So why not bind them?
;; Some of the default keybindings are also be adjusted.
;; But before that, we do some low-level key remapping first.

;; Use `C-h' for `DEL' (backspace).
(define-key key-translation-map [?\C-h] [?\C-?])

;; Detach `C-i' from `TAB'
(define-key input-decode-map "\C-i" [Ci]) ; this alone won't apply to client frames, so we need the line below
(add-hook 'after-make-frame-functions
          (lambda (frame) (with-selected-frame frame (define-key input-decode-map "\C-i" [Ci]))))

;; `Hyper' key is well placed on my keyboard, so let's make good use of that first.
;; Also, `Hyper' uses my right thumb while `Ctrl' uses my left pinky.
;; `H-x' is much more comfortable than `C-x' for some certain key sequences.
(define-key key-translation-map (kbd "H-x g") (kbd "C-x g")) ; `magit'
(define-key key-translation-map (kbd "H-x z") (kbd "C-x z")) ; `repeat'
(define-key key-translation-map (kbd "H-x H-s") (kbd "C-x C-s"))

;; A prefix key for toggling `m'inor modes
(define-prefix-command 'toggle-minor-mode-map)
(global-set-key (kbd "C-x m") 'toggle-minor-mode-map)
(keymap-set toggle-minor-mode-map (kbd "f") 'follow-mode)
(keymap-set toggle-minor-mode-map (kbd "l") 'display-line-numbers-mode)

;; The default command bound to `M-z' is `zap-to-char'. However, the below makes more sense.
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; Think of "zap pair"
(global-set-key (kbd "C-M-z") 'delete-pair)
(setq delete-pair-blink-delay 0) ; Heck, why would I want any delay?

;; Changing case made easy. Do what I mean!
(global-set-key [remap capitalize-word] 'capitalize-dwim)
(global-set-key [remap upcase-word] 'upcase-dwim)
(global-set-key [remap downcase-word] 'downcase-dwim)

;; We have a better use of `Shift' as modifier key
(setq shift-select-mode nil)

;; Transposing things around: `transpose-lines' has been taken care of by `move-text'.
(global-set-key (kbd "M-T")     'transpose-sentences)
(global-set-key (kbd "C-x C-t") 'transpose-paragraphs)

;; Need for Speed: Shift... Make `C-n', `C-p', `C-f' and `C-b' faster
(global-set-key (kbd "C-S-n") (lamb (forward-line 5) (recenter)))
(global-set-key (kbd "C-S-p") (lamb (previous-line 5) (recenter)))
(global-set-key (kbd "C-S-f") (lamb (forward-char 5) (recenter)))
(global-set-key (kbd "C-S-b") (lamb (backward-char 5) (recenter)))

;; Paragraph navigation: `n'ext paragraph and `p'revious paragraph
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-N") (lamb (forward-paragraph 4) (recenter)))
(global-set-key (kbd "M-P") (lamb (backward-paragraph 4) (recenter)))

;; Need for Speed: Shift (more shift for more speed)
(global-set-key (kbd "M-A") 'copy-from-above-command)
(global-set-key (kbd "M-D") 'duplicate-dwim)
(global-set-key (kbd "M-F") (lamb (forward-word 4) (recenter)))
(global-set-key (kbd "M-B") (lamb (backward-word 4) (recenter)))
(global-set-key (kbd "M-|") 'delete-all-space) ; big brother to the built-in `M-\\' : `delete-horizontal-space'

;; The prefix `M-s' is well placed on the home row and is criminally underused. Why not redeem it?
;; And make it mnemonic: `M-s' for `M'anipulate-`s'tring
(global-set-key (kbd "M-s a") 'align-regexp)
(global-set-key (kbd "M-s c") 'count-matches)
(global-set-key (kbd "M-s d") 'delete-duplicate-lines)
(global-set-key (kbd "M-s f") 'flush-lines)
(global-set-key (kbd "M-s k") 'keep-lines)
(global-set-key (kbd "M-s l") 'sort-lines)
(global-set-key (kbd "M-s r") 'replace-string)

;; The default `M-@' is less ergonomic.
(global-set-key (kbd "C-@") 'mark-word)

;; Join lines in a more sensible way.
(global-set-key (kbd "C-j") (lamb (join-line -1))) ; join this line to the next
(global-set-key (kbd "C-S-j") 'join-line) ; join this line to the previous

;; The following should have been universal without needing to load `org-mode'.
(global-set-key (kbd "C-+") 'org-increase-number-at-point)
(global-set-key (kbd "C-_") 'org-decrease-number-at-point)
(global-set-key (kbd "M-+") (lamb (org-increase-number-at-point 10)))
(global-set-key (kbd "M-_") (lamb (org-decrease-number-at-point 10)))

;; Keyboard macros made easier.
(global-set-key (kbd "C-(") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "C-)") 'kmacro-end-or-call-macro)

;; Used rarely, but it's nice to keep them bound, because I don't wanna do M-x and type the name again.
(global-set-key (kbd "S-<f2>") 'rename-visited-file) ; a homage to GUI file managers' <f2> renaming
(global-set-key (kbd "S-<f5>") 'recover-this-file)

;; Because the default `C-x C-c' is too easy to reach.
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)

;; Some bunch of advice.
(advice-add 'duplicate-dwim :after (lambda (&rest _args) (next-line)))

(provide 'knot-keys)

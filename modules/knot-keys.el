;;; knot-keys.el --- Some extra global keys for the thermonuclear editor -*- lexical-binding: t; -*-

;; Use `C-h' for `DEL' (backspace).
(define-key key-translation-map [?\C-h] [?\C-?])

;; Detach `C-i' from `TAB'
(define-key input-decode-map "\C-i" [Ci])               ; this alone won't apply to client frames, so we need the line below
(add-hook 'after-make-frame-functions
          (lambda (frame) (with-selected-frame frame (define-key input-decode-map "\C-i" [Ci]))))

;; `Hyper' key is well placed on my keyboard, so let's make good use of that first.
;; `H-x' is much more comfortable than `C-x' for some certain key sequences.
(define-key key-translation-map (kbd "H-x g") (kbd "C-x g"))                     ; `magit'
(define-key key-translation-map (kbd "C-H-b") (kbd "C-x b"))                     ; `consult-buffer'
(define-key key-translation-map (kbd "H-x H-s") (kbd "C-x C-s"))
(bind-key "H-r" ctl-x-r-map)                                                     ; `H-r' is much faster to type than `C-x r'

;; Readjustments to some default keybindings
(bind-key "C-z" 'repeat)
(bind-key "M-m" 'mark-word)                                                      ; by default, `M-m' is `back-to-indentation'
(bind-key "M-M" (lamb (mark-word 4 t)))                                          ; mark 4 words at a time
(bind-key "C-x C-c" (lamb (message "Sorcerers never quit sorcery.")))

;; No arrows. BACK TO THE CHORDS!
(dolist (key '("<up>" "<down>" "<right>" "<left>"))
  (bind-key key (lamb (message "Arrows? Where we are editing, we don't need arrows."))))

;; A prefix key for toggling `m'inor modes
(define-prefix-command 'toggle-minor-mode-map)
(bind-key "C-x m" 'toggle-minor-mode-map)
(keymap-set toggle-minor-mode-map (kbd "f") 'follow-mode)
(keymap-set toggle-minor-mode-map (kbd "l") 'display-line-numbers-mode)

;; Zapping
(bind-key "M-z" 'zap-up-to-char)                                           ; by default, `M-z' is bound to `zap-to-char'
(bind-key "C-M-z" 'delete-pair)                                            ; think of "zap pair"
(setopt delete-pair-blink-delay 0)                                         ; heck, why would I want any delay?

;; Changing case
(bind-key [remap capitalize-word] 'capitalize-dwim)
(bind-key [remap upcase-word] 'upcase-dwim)
(bind-key [remap downcase-word] 'downcase-dwim)

;; Adjust font size globally
(bind-key [remap text-scale-adjust] 'global-text-scale-adjust)
(dolist (key '("C-H-=" "C-H--" "C-H-0"))
  (bind-key key 'global-text-scale-adjust))

;; Duplicating lines/regions
(bind-key "C-:" 'copy-from-above-command)
(bind-key "C-<" 'duplicate-dwim)
(advice-add 'duplicate-dwim :after (lambda (&rest _args) (next-line)))

;; Transposing things around: `transpose-lines' has been taken care of by `move-text'.
(bind-key "M-T" 'transpose-sentences)
(bind-key "H-t" 'transpose-paragraphs)

;; Need for Speed: Shift... Hold `S'hift for `S'peed
(bind-key "C-S-n" (lamb (forward-line 5) (recenter)))
(bind-key "C-S-p" (lamb (previous-line 5) (recenter)))
(bind-key "C-S-f" (lamb (forward-char 5) (recenter)))
(bind-key "C-S-b" (lamb (backward-char 5) (recenter)))

;; Paragraph navigation: `n'ext paragraph and `p'revious paragraph
(bind-key "M-n" 'forward-paragraph)
(bind-key "M-p" 'backward-paragraph)
(bind-key "M-N" (lamb (forward-paragraph 4) (recenter)))
(bind-key "M-P" (lamb (backward-paragraph 4) (recenter)))

;; More `s'hift for more `s'peed.
(bind-key "M-F" (lamb (forward-word 4) (recenter)))
(bind-key "M-B" (lamb (backward-word 4) (recenter)))
(bind-key "M-|" 'delete-all-space)                          ; big brother to the built-in `M-\\' : `delete-horizontal-space'

;; The prefix `M-s' is well placed on the home row and is criminally underused. Why not redeem it?
;; And make it mnemonic: `M-s' for `M'anipulate-`s'tring
(bind-key "M-s a" 'align-regexp)
(bind-key "M-s c" 'count-matches)
(bind-key "M-s d" 'delete-duplicate-lines)
(bind-key "M-s f" 'flush-lines)
(bind-key "M-s k" 'keep-lines)
(bind-key "M-s l" 'sort-lines)
(bind-key "M-s r" 'replace-string)

;; Join lines in a more sensible way.
(bind-key "C-j" (lamb (join-line -1)))                                     ; join this line to the next
(bind-key "C-S-j" 'join-line)                                              ; join this line to the previous

;; We need to load `org-mode' for the following.
(autoload 'org-increase-number-at-point "org" nil t)
(autoload 'org-decrease-number-at-point "org" nil t)
(bind-key "C-+" 'org-increase-number-at-point)
(bind-key "C-_" 'org-decrease-number-at-point)
(bind-key "M-+" (lamb (org-increase-number-at-point 10)))
(bind-key "M-_" (lamb (org-decrease-number-at-point 10)))

;; Summon keyboard macros easily.
(bind-key "C-(" 'kmacro-start-macro-or-insert-counter)
(bind-key "C-)" 'kmacro-end-or-call-macro)

;; Window/buffer navigation and management
(bind-key "M-o" 'other-window)
(bind-key "C-x C-b" 'ibuffer)
(bind-key "H-4" 'display-buffer)
(bind-key "M-H-0" 'kill-buffer-and-window)

;; I don't wanna do M-x and type the name again.
(bind-key "S-<f2>" 'rename-visited-file)                                   ; a homage to GUI file managers' <f2> renaming
(bind-key "S-<f5>" 'recover-this-file)
(bind-key "S-<delete>" 'delete-file)                                       ; another homage!

(provide 'knot-keys)

;;; knot-keys.el --- Some extra global keys for the thermonuclear editor -*- lexical-binding: t; -*-

(require 'knot-macros)

;; Use `C-h' for `DEL' (backspace).
(define-key key-translation-map [?\C-h] [?\C-?])

;; Use hyper keys instead of meta-arrows
(define-key key-translation-map [?\H-u] [M-up])
(define-key key-translation-map [?\H-e] [M-down])
(define-key key-translation-map [?\H-n] [M-left])
(define-key key-translation-map [?\H-i] [M-right])

;; Detach `C-i' from `TAB'
(define-key input-decode-map "\C-i" [Ci])               ; this alone won't apply to client frames, so we need the line below
(add-hook 'after-make-frame-functions
          (lambda (frame) (with-selected-frame frame (define-key input-decode-map "\C-i" [Ci]))))

;; Because my meta is on the right side of my keyboard
(bind-key "M-`"   'negative-argument)
(bind-key "C-M-`" 'negative-argument)

;; `H-x' is much more comfortable than `C-x' for some certain key sequences.
(define-key key-translation-map (kbd "C-H-b") (kbd "C-x b"))                    ; `consult-buffer'
(define-key key-translation-map (kbd "H-x H-s") (kbd "C-x C-s"))                ; more ergonomic saving

;; Create a version of these commands that apply to the whole buffer if there is no active region.
(rh/define-region-or-buffer-command query-replace)
(rh/define-region-or-buffer-command query-replace-regexp)
(rh/define-region-or-buffer-command replace-string)

;; Readjustments and (re)bindings of some inbuilt commands
(bind-key "C-z"   'repeat)
(bind-key "C-&"   'replace-regexp)
(bind-key "M-%"   'rh/region-or-buffer--query-replace)
(bind-key "C-M-%" 'rh/region-or-buffer--query-replace-regexp)
(bind-key "C-%"   'rh/region-or-buffer--replace-string)                         ; reminiscent of `M-%' and `C-M-%'?
(bind-key "M-m"   'mark-word)                                                   ; by default, `M-m' is `back-to-indentation'
(bind-key "C-S-j" 'join-line)                                                   ; join this line to the previous
(bind-key "M-M"   (lamb (mark-word 4 t)))                                       ; mark 4 words at a time
(bind-key "M-r"   ctl-x-r-map)                                                  ; `M-r' is much faster to type than `C-x r'
(bind-key [remap text-scale-adjust] 'global-text-scale-adjust)                  ; always adjust text scale globally
(bind-key "C-x C-c" (lamb (message "Sorcerers never quit sorcery.")))

;; No arrows. BACK TO THE CHORDS!
(dolist (key '("<up>" "<down>" "<right>" "<left>"))
  (bind-key key (lamb (message "Arrows? Where we are editing, we don't need arrows."))))

;; Zapping
(bind-key "M-z" 'zap-up-to-char)                                                ; by default, `M-z' is bound to `zap-to-char'
(bind-key "M-Z" 'zap-to-char)
(bind-key "C-M-z" 'delete-pair)                                                 ; think of "zap pair"

;; Changing case
(bind-key [remap capitalize-word] 'capitalize-dwim)
(bind-key [remap upcase-word] 'upcase-dwim)
(bind-key [remap downcase-word] 'downcase-dwim)

;; Duplicating lines/regions
(bind-key "C-:" 'copy-from-above-command)
(bind-key "C-<" 'duplicate-dwim)

;; Transposing things around: `transpose-lines' has been taken care of by `move-text'.
(bind-key "M-T" 'transpose-sentences)
(bind-key "H-t" 'transpose-paragraphs)

;; Need for Speed: Shift... Hold `S'hift for `S'peed
(bind-key "C-S-n" (lamb (next-line 5) (recenter)))
(bind-key "C-S-p" (lamb (previous-line 5) (recenter)))
(bind-key "C-S-f" (lamb (forward-char 4) (recenter)))
(bind-key "C-S-b" (lamb (backward-char 4) (recenter)))

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

;; Sorting (who says we can't overload `M-s'?)
(bind-key "M-s M-l" 'sort-lines)
(bind-key "M-s M-c" 'sort-columns)
(bind-key "M-s M-f" 'sort-fields)
(bind-key "M-s M-p" 'sort-paragraphs)
(bind-key "M-s M-r" 'reverse-region)

;; A prefix key for toggling `m'inor modes
(define-prefix-command 'toggle-minor-mode-map)
(bind-key "C-x m" 'toggle-minor-mode-map)
(keymap-set toggle-minor-mode-map (kbd "f") 'follow-mode)
(keymap-set toggle-minor-mode-map (kbd "l") 'display-line-numbers-mode)

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
(bind-key "H-1" 'delete-other-windows)
(bind-key "H-2" 'split-window-below)
(bind-key "H-3" 'split-window-right)
(bind-key "H-4" 'display-buffer)
(bind-key "H-0" 'delete-window)
(bind-key "H-<delete>" 'kill-buffer-and-window)
(bind-key "C-x C-b" 'ibuffer)

;; I don't wanna do M-x and type the name again.
(bind-key "S-<f2>" 'rename-visited-file)                                   ; a homage to GUI file managers' <f2> renaming
(bind-key "S-<f5>" 'recover-this-file)
(bind-key "S-<delete>" 'delete-file)                                       ; another homage!

(provide 'knot-keys)

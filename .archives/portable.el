;; Portable configuration that should work out of the box without any external packages.
;; This file is primarily meant to serve the following purpose:
;; when I have to work in another computer and I have no time to git clone my configuration and reproduce my main setup.
;; In short, this only uses inbuilt features that Emacs ships with --- and I should be able survive very well with this.
;; The idea is simple: I simply copy-paste this whole file in the scratch buffer and evaluate buffer. Then I am ready.

(menu-bar-mode   -1)
(tool-bar-mode   -1)
(scroll-bar-mode -1)
(tooltip-mode    -1)
(set-fringe-mode -1)

;; Concerning files
(add-hook 'before-save-hook 'delete-trailing-whitespace)               ; delete trailing whitespace at saving
(global-auto-revert-mode)                                              ; refresh the buffer when files change on disk
(save-place-mode)                                                      ; save place in each file
(setq-default require-final-newline t)                                 ; ensure a final newline at saving

;; Concerning kills
(kill-ring-deindent-mode)                                              ; remove indentation while saving to the kill ring
(setq kill-buffer-query-functions nil)                                 ; don't ask for confirmation while killing buffers
(setq kill-do-not-save-duplicates t)                                   ; don't add duplicates to the kill king
(setq kill-whole-line t)                                               ; `C-k' at the start of a line kills the whole line

;; Concerning lines, sentences, words and characters
(global-display-line-numbers-mode)                                     ; line numbers everywhere please
(global-hl-line-mode)                                                  ; highlight current line
(global-subword-mode)
(global-visual-line-mode)
(setq display-line-numbers-type 'relative)                             ; relative line numbering, yes!
(setq sentence-end-double-space nil)                                   ; a sentence should not need to end in double spaces
(setq-default fill-column 80)

;; Concerning mouse, cursors and scrolling
(blink-cursor-mode 0)                                                  ; cursor should not blink
(pixel-scroll-precision-mode)                                          ; make mouse scrolling smoother
(setq make-pointer-invisible t)                                        ; hide the mouse cursor while typing
(setq mouse-yank-at-point t)                                           ; mouse yank commands yank at point (not at click)
(setq scroll-preserve-screen-position t)                               ; while scrolling, try to keep the point unchanged
(setq-default cursor-in-non-selected-windows nil)                      ; hide cursor/point on non-active windows

;; Concerning windows
(setq window-combination-resize t)                                     ; keep windows balanced
(winner-mode)                                                          ; undo window configurations
(global-set-key (kbd "H-<tab>") 'winner-undo)

;; Make it more convenient (concerning convenience?)
(setq
 next-line-add-newlines t                                              ; `C-n', at buffer end, inserts newline
 disabled-command-function nil                                         ; enable all disabled commands, I know what I am doing
 use-short-answers t                                                   ; all confirmations prompts be y or n
 echo-keystrokes 0.1                                                   ; display keystrokes in the echo area faster
 confirm-kill-processes nil                                            ; don't confirm killing processes on exit
 shift-select-mode nil                                                 ; we have a better use of `Shift' as modifier key
 )

;; Some nice minor modes
(delete-selection-mode)                                                ; typing on a region replaces it
(global-goto-address-mode)                                             ; make URLs and email addresses clickable
(global-prettify-symbols-mode)                                         ; pretty math symbols
(repeat-mode)                                                          ; repeat commands without retyping the prefix key
(setq repeat-exit-timeout 5)                                           ; no repeat after 5 seconds
(fido-mode)
(global-eldoc-mode)
(setq eldoc-idle-delay 0.2)
(electric-pair-mode)

;; `isearch'
(setq isearch-allow-scroll 'unlimited
      isearch-lazy-count t
      isearch-repeat-on-direction-change t
      search-default-mode 'char-fold-to-regexp
      search-whitespace-regexp ".*?"
      )

(defmacro lamb (&rest body)
  "Return an interactive lambda that ignore errors."
  `(lambda ()
     (interactive)
     (ignore-errors ,@body)))

;; Use `C-h' for `DEL' (backspace).
(define-key key-translation-map [?\C-h] [?\C-?])

;; Detach `C-i' from `TAB'
(define-key input-decode-map "\C-i" [Ci])
(add-hook 'after-make-frame-functions
          (lambda (frame) (with-selected-frame frame (define-key input-decode-map "\C-i" [Ci]))))

;; `H-x' is much more comfortable than `C-x' for some certain key sequences.
(define-key key-translation-map (kbd "C-H-b") (kbd "C-x b"))
(define-key key-translation-map (kbd "H-x H-s") (kbd "C-x C-s"))

;; Readjustments and (re)bindings of some inbuilt commands
(global-set-key (kbd "C-z") 'repeat)
(global-set-key (kbd "C-%") 'replace-string)
(global-set-key (kbd "M-m") 'mark-word)
(global-set-key (kbd "M-M") (lamb (mark-word 4 t)))
(global-set-key (kbd "C-x C-c") (lamb (message "Sorcerers never quit sorcery.")))
(global-set-key (kbd "M-r") ctl-x-r-map)
(global-set-key [remap text-scale-adjust] 'global-text-scale-adjust)

;; I teleport a lot!
(global-set-key (kbd "<Ci>") (lamb (point-to-register ?6)))
(global-set-key (kbd "C-S-i") (lamb (jump-to-register ?6)))

;; Zapping
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-M-z") 'delete-pair)
(setopt delete-pair-blink-delay 0)

;; Changing case
(bind-key [remap capitalize-word] 'capitalize-dwim)
(bind-key [remap upcase-word] 'upcase-dwim)
(bind-key [remap downcase-word] 'downcase-dwim)

;; Duplicating lines/regions
(global-set-key (kbd "C-:") 'copy-from-above-command)
(global-set-key (kbd "C-<") 'duplicate-dwim)
(advice-add 'duplicate-dwim :after (lambda (&rest _args) (next-line)))

;; Transposing things around: `transpose-lines' has been taken care of by `move-text'.
(global-set-key (kbd "M-T") 'transpose-sentences)
(global-set-key (kbd "H-t") 'transpose-paragraphs)

;; Need for Speed: Shift... Hold `S'hift for `S'peed
(global-set-key (kbd "C-S-n") (lamb (forward-line 5) (recenter)))
(global-set-key (kbd "C-S-p") (lamb (previous-line 5) (recenter)))
(global-set-key (kbd "C-S-f") (lamb (forward-char 4) (recenter)))
(global-set-key (kbd "C-S-b") (lamb (backward-char 4) (recenter)))

;; Paragraph navigation: `n'ext paragraph and `p'revious paragraph
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-N") (lamb (forward-paragraph 4) (recenter)))
(global-set-key (kbd "M-P") (lamb (backward-paragraph 4) (recenter)))

;; More `s'hift for more `s'peed.
(global-set-key (kbd "M-F") (lamb (forward-word 4) (recenter)))
(global-set-key (kbd "M-B") (lamb (backward-word 4) (recenter)))
(global-set-key (kbd "M-|") 'delete-all-space)

;; The prefix `M-s' is well placed on the home row and is criminally underused. Why not redeem it?
;; And make it mnemonic: `M-s' for `M'anipulate-`s'tring
(global-set-key (kbd "M-s a") 'align-regexp)
(global-set-key (kbd "M-s c") 'count-matches)
(global-set-key (kbd "M-s d") 'delete-duplicate-lines)
(global-set-key (kbd "M-s f") 'flush-lines)
(global-set-key (kbd "M-s k") 'keep-lines)
(global-set-key (kbd "M-s l") 'sort-lines)

;; Join lines in a more sensible way.
(global-set-key (kbd "C-j") (lamb (join-line -1)))
(global-set-key (kbd "C-S-j") 'join-line)

;; We need to load `org-mode' for the following.
(autoload 'org-increase-number-at-point "org" nil t)
(autoload 'org-decrease-number-at-point "org" nil t)
(global-set-key (kbd "C-+") 'org-increase-number-at-point)
(global-set-key (kbd "C-_") 'org-decrease-number-at-point)
(global-set-key (kbd "M-+") (lamb (org-increase-number-at-point 10)))
(global-set-key (kbd "M-_") (lamb (org-decrease-number-at-point 10)))

;; Summon keyboard macros easily.
(global-set-key (kbd "C-(") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "C-)") 'kmacro-end-or-call-macro)

;; Window/buffer navigation and management
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; I don't wanna do M-x and type the name again.
(global-set-key (kbd "S-<f2>") 'rename-visited-file)
(global-set-key (kbd "S-<f5>") 'recover-this-file)
(global-set-key (kbd "S-<delete>") 'delete-file)

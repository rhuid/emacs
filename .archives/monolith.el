;;; init.el --- The main init.el file -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq-default default-directory "~/")
(setq vc-follow-symlinks t)                                             ; always follow symlinks without asking

;; Set up packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") 'append)
(require 'use-package)
(setq use-package-always-ensure    t                                    ; always install packages if not found
      use-package-always-defer     t                                    ; always defer packages by default
      use-package-vc-prefer-newest t                                    ; :rev :newest by default
      load-prefer-newer t                                               ; prefer .el over .eln or .elc if it's more recent
      use-package-enable-imenu-support t)

;; Inherit shell variables (could be important for daemon)
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize)
  (setq exec-path-from-shell-variables '("PATH" "MANPATH")))

;; Garbage collector tweaks to make Emacs more responsive
(use-package gcmh
  :init (gcmh-mode)
  :custom
  (gcmh-idle-delay 20)                                                  ; run GC after 20 secs idle
  (gcmh-high-cons-threshold (* 256 1024 1024)))                         ; while typing, don't run GC until (threshold 256 MB)

;; Set up appearance
(load-theme 'modus-operandi)                                            ; now built-in with Emacs
(use-package mood-line :init (mood-line-mode))                          ; a minimalist mode-line

;; Local modules
(require 'knot-macros)
(require 'knot-editing)
(require 'knot-misc)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))  ; don't mess up my init, use a custom file
(load custom-file)

;;;; Keybindings

;; Use `C-h' for `DEL' (backspace) and rebind `help-command' to `C-M-?'
(define-key key-translation-map [?\C-h] [?\C-?])
(bind-key "C-M-?" 'help-command)

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
(bind-key "C-S-s" 'isearch-forward-thing-at-point)
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
(bind-key "C-x C-b" 'ibuffer)

;;;; Sane defaults

;; Concerning files
(add-hook 'before-save-hook 'delete-trailing-whitespace)               ; delete trailing whitespace at saving
(global-auto-revert-mode)                                              ; refresh the buffer when files change on disk
(save-place-mode)                                                      ; save place in each file
(setq make-backup-files nil)                                           ; don't make backup files
(setq-default require-final-newline t)                                 ; ensure a final newline at saving

;; Concerning kills
(kill-ring-deindent-mode)                                              ; remove indentation while saving to the kill ring
(setq kill-buffer-query-functions nil)                                 ; don't ask for confirmation while killing buffers
(setq kill-do-not-save-duplicates t)                                   ; don't add duplicates to the kill king

;; Concerning lines, sentences, words and characters
(global-display-line-numbers-mode)                                     ; line numbers everywhere please
(global-hl-line-mode)                                                  ; highlight current line
(global-subword-mode)
(global-visual-line-mode)
(setq display-line-numbers-type 'relative)                             ; relative line numbering, yes!
(setq display-line-numbers-width-start t )                             ; count number of lines to use for line number width
(setq sentence-end-double-space nil)                                   ; a sentence should not need to end in double spaces
(setq-default fill-column 80)

;; Concerning mouse, cursors and scrolling
(blink-cursor-mode 0)                                                  ; cursor should not blink
(pixel-scroll-precision-mode)                                          ; make mouse scrolling smoother
(setq make-pointer-invisible t)                                        ; hide the mouse cursor while typing
(setq mouse-yank-at-point t)                                           ; mouse yank commands yank at point (not at click)
(setq scroll-preserve-screen-position t)                               ; while scrolling, try to keep the point unchanged
(setq-default cursor-in-non-selected-windows nil)                      ; hide cursor/point on non-active windows

;; Concerning the mode-line
(display-time-mode)                                                    ; I want to know the time from the mode-line
(setq display-time-day-and-date t)                                     ; display date as well
(display-battery-mode)                                                 ; display battery level in the mode-line

;; Concerning windows
(setq window-combination-resize t)                                     ; keep windows balanced
(winner-mode)                                                          ; undo window configurations
(bind-key "H-<tab>" 'winner-undo)

;; Make it more convenient (concerning convenience?)
(setq
 use-dialog-box nil                                                    ; no dialog box, please
 next-line-add-newlines t                                              ; `C-n' adds newline, avoid `end-of-buffer' error
 disabled-command-function nil                                         ; enable all disabled commands, I know what I am doing
 use-short-answers t                                                   ; all confirmations prompts be y or n
 echo-keystrokes 0.1                                                   ; display keystrokes in the echo area faster
 idle-update-delay 0.1                                                 ; update things on screen faster after typing
 confirm-kill-processes nil                                            ; don't confirm killing processes on exit
 shift-select-mode nil                                                 ; we have a better use of `Shift' as modifier key
 suggest-key-bindings nil)                                             ; don't show equivalent keybindings when `M-x' has one

;; Concerning editing
(setq
 duplicate-line-final-position   1                                     ; move point to the first new line
 duplicate-region-final-position 1                                     ; put the region around the first copy
 delete-pair-blink-delay 0)                                            ; heck, why would I want any delay?

;; Some nice minor modes
(delete-selection-mode)                                                ; typing on a region replaces it
(global-goto-address-mode)                                             ; make URLs and email addresses clickable
(global-prettify-symbols-mode)                                         ; pretty math symbols
(repeat-mode)                                                          ; repeat commands without retyping the prefix key
(recentf-mode)                                                         ; save recent files
(savehist-mode)                                                        ; save minibuffer history
(setq
 repeat-exit-timeout 5                                                 ; no repeat after 5 seconds
 recentf-max-saved-items 200
 recentf-max-menu-items 25
 history-length 2000
 history-delete-duplicates t
 savehist-additional-variables '(kill-ring search-ring regexp-search-ring))

;;;; Packages

;; Let Emacs finish your phrases. Because typing is hard work.
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

;; Sail through the visible screen at the speed of thought.
(use-package avy
  :bind (("C-," . avy-goto-char-timer)
         :map isearch-mode-map
         ("C-," . avy-isearch))
  :custom
  (avy-keys '(?t ?n ?s ?e ?r ?i ?a ?o))
  (avy-timeout-seconds 0.2))

;; Type comfortably --- `eldoc' watches over you; a whisper of documentation as you type.
(use-package eldoc
  :ensure nil
  :init (global-eldoc-mode)
  :config (setq eldoc-idle-delay 0.2))

;; Electric pairs: Auto-insert the closing delimiter.
(use-package elec-pair
  :init (electric-pair-mode)
  :hook (org-mode . rh/org-electric-pairs)
  :custom (electric-pair-pairs '((?\(.?\)) (?\{.?\}) (?\[.?\]) (?\".?\") (?\<.?\>)))
  :config
  (defun rh/org-electric-pairs ()
    "Org pairs for electric-pair-mode."
    (setq-local electric-pair-pairs (append '((?_.?_) (?~.?~))))))

;; Select and expand regions by semantic units.
(use-package expand-region
  :bind ("C->" . er/expand-region))

;; Some modes look cleaner without it.
(use-package hide-mode-line
  :hook ((dired-mode org-mode eshell-mode) . hide-mode-line-mode))

;; Let Emacs whisper the rest of your words; completion for those who prefer serendipity over precision.
(use-package hippie-exp
  :bind ("M-/" . hippie-expand)
  :custom
  (hippie-expand-try-functions-list
   '(try-complete-file-name
     try-complete-file-name-partially
     try-expand-dabbrev-visible
     try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     try-expand-list
     try-expand-line
     try-expand-dabbrev-from-kill
     try-complete-lisp-symbol
     try-complete-lisp-symbol-partially
     try-expand-all-abbrevs)))

;; Search as you type, and watch your buffer follow.
(use-package isearch
  :ensure nil
  :custom
  (isearch-allow-scroll 'unlimited)                                          ; scroll as much as you please
  (isearch-lazy-count t)                                                     ; show number of matches in the mode-line
  (isearch-repeat-on-direction-change t)                                     ; allow switching direction
  (search-default-mode 'char-fold-to-regexp)                                 ; match accented letters too
  (search-whitespace-regexp ".*?"))                                          ; type "t n" to match "teleportation"

;; Type freely; Jinx has your back. A silent guardian of your spelling.
;; Requires enchant and dictionary backend (I use `hunspell-en_us').
(use-package jinx
  :init (global-jinx-mode)
  :bind ("C-*" . jinx-correct) ("C-M-*" . jinx-correct-word)
  :custom (jinx-languages "en_US-large"))

;; Git, without ever leaving home and without touching the terminal.
(use-package magit
  :bind ("C-x g" .  magit-status)
  (:map magit-mode-map ("." . rh/commit) ("," . rh/amend))
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (defun rh/commit ()
    "Prompt for a commit message in minibuffer and commit immediately."
    (interactive)
    (let ((msg (read-string "Commit message: ")))
      (magit-commit-create `("-m" ,msg))))
  (defun rh/amend ()
    "Quickly amend last commit with a new message via minibuffer."
    (interactive)
    (let ((msg (read-string "Amend message: ")))
      (magit-commit-create `("--amend" "-m" ,msg)))))

;; Edit everywhere at once; parallel editing.
(use-package multiple-cursors
  :bind (("C-M-S-a" . mc/mark-all-like-this-dwim)
         ("M-S-RET" . mc/edit-lines)                                                          ; `C-M-S-m'
         ("C-M-S-n" . mc/mark-next-like-this)
         ("C-M-S-p" . mc/mark-previous-like-this)
         ("C-M->"   . mc/skip-to-next-like-this)
         ("C-M-<"   . mc/skip-to-previous-like-this))
  :custom (mc/always-run-for-all t))

;; Fold and bloom your buffer's hierarchy.
(use-package outline
  :hook ((prog-mode text-mode) . outline-minor-mode)
  :init (setq outline-minor-mode-prefix (kbd "C-c o")))

;; Move where I mean.
(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line-or-comment)
         ("C-e" . mwim-end-of-code-or-line)))

;; Highlight matching parentheses, braces and brackets.
(use-package paren
  :init (show-paren-mode)
  :custom (show-paren-delay 0))

;; Navigate your project with ease; lightweight and comes built-in.
(use-package project
  :custom (project-switch-commands
           '((magit-project-status "Magit"     ?m)
             (project-find-file    "Find file" ?f)
             (project-dired        "Dired"     ?d)
             (project-eshell       "Eshell"    ?e))))

;; Structural editing: lightweight, and language-agnostic
(use-package puni
  :init (puni-global-mode)
  :bind (:map puni-mode-map
              ("C-w" . nil)                                ; taken by whole-line-or-region-kill-region
              ("M-o" . rh/puni-rewrap-sexp)
              ("M-K" . kill-paragraph)
              ("M-H" . backward-kill-paragraph)
              ("C-M-f" . puni-forward-sexp-or-up-list)
              ("C-M-b" . puni-backward-sexp-or-up-list)
              ("C-S-h" . puni-backward-kill-word)
              ("C-S-i" . puni-backward-kill-word)
              ("C-M-r" . puni-raise)
              ("C-M-s" . puni-squeeze)
              ("C-H-i" . puni-slurp-forward)
              ("C-H-n" . puni-barf-forward)
              ("M-H-n" . puni-slurp-backward)
              ("M-H-i" . puni-barf-backward)
              ("C-M-c p" . puni-split)
              ("C-M-c s" . puni-splice))
  :custom
  (puni-squeeze-flash nil)
  (puni-blink-for-sexp-manipulating nil)
  (puni-confirm-when-delete-unbalanced-active-region nil)
  :config
  (defun rh/puni-rewrap-sexp ()
    "Rewrap the current sexp."
    (interactive)
    (let ((delimiter (read-char "Opening delimiter: ")))
      (save-excursion
        (backward-up-list 1 t t)
        (mark-sexp 1 nil)
        (when (use-region-p)
          (let ((beg (region-beginning))
                (end (region-end)))
            (cond
             ((= delimiter ?\() (puni-wrap-round 1))
             ((= delimiter ?\[) (puni-wrap-square 1))
             ((= delimiter ?\{) (puni-wrap-curly 1))
             ((= delimiter ?\<) (puni-wrap-angle 1))
             ((= delimiter ?\") (puni--wrap-region beg end "\"" "\""))
             ((= delimiter ?\') (puni--wrap-region beg end "\'" "\'"))
             (t (deactivate-mark)
                (error "Invalid delimiter entered."))))
          (delete-pair 1))))))

;; Let's feel a bit more spacious.
(use-package spacious-padding
  :init (spacious-padding-mode)
  :custom (spacious-padding-widths '( :internal-border-width 12 :mode-line-width 3 )))

;; Like a presentation mode, much more readable and pleasant to the eyes.
(use-package visual-fill-column
  :bind ("C-H-SPC" . visual-fill-column-mode)
  :hook ((org-mode text-mode magit-status-mode emacs-lisp-mode eshell-mode)
         . visual-fill-column-mode)
  :custom
  (visual-fill-column-width 130)
  (visual-fill-column-center-text t))

;; Undo tree: trace your edits as a living tree.
(use-package vundo
  :bind ("C-x u" . vundo) ("C-?" . undo-redo)
  :custom (undo-limit (* 16 1024 1024)))

;; If there is no active region, kill/copy the current line
(use-package whole-line-or-region
  :init (whole-line-or-region-global-mode))

;; Turn a few keystrokes into full templates.
(use-package yasnippet
  :init (yas-global-mode)
  :custom (yas-snippet-dirs (list (concat user-emacs-directory "snippets"))))

;; Some more packages
(use-package move-text :init (move-text-default-bindings))                     ; move line or region up and down with ease
(use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))   ; highlight nested parentheses cleanly
(use-package rainbow-mode :hook (prog-mode . rainbow-mode))                    ; colorize stings that represent colors
(use-package sudo-edit)
(use-package tree-sitter)
(use-package tree-sitter-langs)

;; Vertical completion UI for the minibuffer.
(use-package vertico
  :init (vertico-mode)
  :custom
  (vertico-resize t)
  (vertico-cycle t))

;; Directory traversal made swift.
(use-package vertico-directory
  :after vertico
  :load-path "~/.emacs.d/elpa/vertico/extensions/"
  :bind (:map vertico-map ("C-w" . vertico-directory-delete-word)))

;; Find what you seek, no matter the order.
(use-package orderless
  :init (setq completion-styles '(orderless partial-completion)))

;; See the story behind every completion candidate.
(use-package marginalia
  :init (marginalia-mode))

;; Consult is your compass for Emacs; a command center for the thermonuclear editor.
(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-x f" . consult-recent-file)
         ("C-S-l" . consult-line)
         ("C-M-g" . consult-ripgrep)
         ("M-g g" . consult-grep)
         ("H-g"   . consult-git-grep)
         ("H-l"   . consult-focus-lines)
         ("M-y"   . consult-yank-pop)
         ("M-g m" . consult-mark)
         ("H-x f" . consult-find)
         ("M-O"   . consult-outline)
         ("H-x m" . consult-minor-mode-menu))
  :config (setq consult-preview-key "C-,")
  :custom (register-use-preview nil))

;; Teleport to any directory with a keystroke.
(use-package consult-dir
  :bind (("C-x C-d" . consult-dir))
  (:map vertico-map ("C-x C-j" . consult-dir-jump-file)))

;; Actions appear where your cursor lingers.
(use-package embark
  :bind (("C-." . embark-act) ("M-." . embark-dwim))
  :init (setq prefix-help-command #'embark-prefix-help-command))

;; Preview actions before they take flight.
(use-package embark-consult
  :after consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Get hints for your next move. Display available keybindings in context after a prefix.
(use-package which-key
  :init (which-key-mode))

;; Completion floats directly under your cursor.
(use-package corfu
  :init (global-corfu-mode)
  :hook ((corfu-mode . corfu-history-mode)
         (corfu-mode . corfu-indexed-mode)
         (corfu-mode . corfu-popupinfo-mode))
  :bind (:map corfu-map ("TAB" . nil) ("RET" . nil) ("C-t" . 'corfu-insert))
  :custom (corfu-auto t)) ; Enable auto popup

;; Add sources for completion.
(use-package cape
  :after corfu
  :hook ((org-mode LaTeX-mode) . rh/cape-dict)
  :demand t
  :config
  (setq cape-dict-file (list (concat user-emacs-directory "library/personal-dictionary")))
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (defun rh/cape-dict ()
    (add-to-list (make-local-variable 'completion-at-point-functions) #'cape-dict)))

(use-package org
  :ensure nil
  :hook (org-mode . (lambda () (display-line-numbers-mode -1)))
  :custom
  (org-hide-leading-stars t)
  (org-log-done 'time)
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-special-ctrl-a/e t)
  (org-pretty-entities t)
  (org-catch-invisible-edits 'show-and-error)
  (org-insert-heading-respect-content t)
  (org-ellipsis "…")
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-agenda-tags-column 0)
  (org-table-auto-align t)
  (org-startup-folded 'content)
  (org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)))
  (org-directory "~/org")
  (org-todo-keywords
   '((sequence "TODO" "WAITING" "IN-PROGRESS" "|" "DONE" "CANCELLED")))
  :config
  (add-to-list 'org-export-backends 'md)
  ;; Set faces for the headings
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.7))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.5))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.3))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.1)))))
  :bind (:map org-mode-map
              ("C-j" . nil)
              ("C-," . nil)
              ("C-'" . nil)
              ("C-S-o" . org-shifttab)))

(use-package org-modern
  :after org
  :init (global-org-modern-mode)
  :custom (org-modern-star t))

(use-package ox-reveal
  :after org
  :init (load-library "ox-reveal")
  :config
  (setq org-re-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
  (add-to-list 'org-export-backends 'reveal))

(use-package org-agenda
  :after org
  :ensure nil
  :commands (org-agenda org-todo-list))

(use-package latex
  :ensure auctex
  :hook
  (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . TeX-fold-mode)
  (LaTeX-mode . turn-on-reftex)
  (LaTeX-mode . rh/provide-keywords-math-face)
  (post-command . rh/toggle-latex-abbrev)
  :bind (:map LaTeX-mode-map ("C-c C-u" . rh/tex-fold-buffer))
  :config
  (setq TeX-auto-save t
        TeX-parse-self t)
  (setq TeX-view-program-selection '((output-pdf "Evince") (output-html "firefox")))
  (setq TeX-view-program-list '(("Evince" "flatpak run org.gnome.Evince --page-index=%(outpage) %o")))

  (defun rh/toggle-latex-abbrev ()
    "Disable abbrevs inside math mode in Latex."
    (if (and (derived-mode-p 'LaTeX-mode) (texmathp))
        (abbrev-mode -1)
      (abbrev-mode 1)))

  (defun rh/tex-fold-buffer (&optional prefix)
    "Fold the buffer but clears the fold with the prefix argument \\[universal-argument]."
    (interactive "P")
    (if prefix
        (TeX-fold-clearout-buffer)
      (TeX-fold-buffer)))

  (defface rh/math-delimiter-face
    '((t (:inherit font-lock-comment-face :weight thin :slant normal)))
    "Face used for math delimiters in Latex."
    :group 'latex)

  (defun rh/provide-keywords-math-face ()
    "Provide keywords that have `rh/math-delimiter-face'."
    (font-lock-add-keywords
     nil
     '(;; Math delimiters
       ("\\(\\\\[][()]\\|\\$\\)" 0 'rh/math-delimiter-face t)
       ))))

(use-package auctex-latexmk
  :after auctex
  :config (auctex-latexmk-setup))

(use-package cdlatex
  :hook
  (LaTeX-mode . turn-on-cdlatex)
  (org-mode . turn-on-org-cdlatex)
  :bind (:map cdlatex-mode-map ("TAB" . rh/latex-tab-action))
  :custom (cdlatex-paired-parens "$([{")
  :config
  (defun rh/latex-tab-action ()
    "Try `yasnippet' first, then fall back to `CDLaTeX'."
    (interactive)
    (unless (and (bound-and-true-p yas-minor-mode) (yas-expand))
      (cdlatex-tab)))

  (setq cdlatex-env-alist
        '(("axiom"        "\\begin{axiom}\nAUTOLABEL\n?\n\\end{axiom}\n" nil)
          ("defintion"    "\\begin{definition}\nAUTOLABEL\n?\n\\end{definition}\n" nil)
          ("lemma"        "\\begin{lemma}\nAUTOLABEL\n?\n\\end{lemma}\n" nil)
          ("proposition"  "\\begin{proposition}\nAUTOLABEL\n?\n\\end{proposition}\n" nil)
          ("theorem"      "\\begin{theorem}\nAUTOLABEL\n?\n\\end{theorem}\n" nil)
          ("corollary"    "\\begin{corollary}\nAUTOLABEL\n?\n\\end{corollary}\n" nil)))
  (setq cdlatex-command-alist
        '(("ax"   "Insert axiom env"       "" cdlatex-environment ("axiom") t nil)
          ("def"  "Insert definition env"  "" cdlatex-environment ("definition") t nil)
          ("lem"  "Insert lemma env"       "" cdlatex-environment ("lemma") t nil)
          ("prop" "Insert proposition env" "" cdlatex-environment ("proposition") t nil)
          ("th"   "Insert theorem env"     "" cdlatex-environment ("theorem") t nil)
          ("cor"  "Insert corollary env"   "" cdlatex-environment ("corollary") t nil)
          ("pr"   "Insert proof env"       "" cdlatex-environment ("proof") t nil)
          ("dp"   "Insert displaymath env" "" cdlatex-environment ("displaymath") t nil))))

(use-package latex-preview-pane
  :bind ("C-c C-p C-l" . latex-preview-pane-mode))

(use-package typst-ts-mode
  :hook (typst-ts-mode . typst-ts-watch-mode))

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("b" . dired-up-directory)                  ; `b' for back
              ("r" . dired-toggle-read-only)              ;  enter `WDired' mode!
              ("f" . dired-display-file))                 ; `f' for display-`f'ile
  :hook ((dired-mode . rh/dired-setup)
         (dired-mode . dired-hide-details-mode))
  :config
  (defun rh/dired-setup ()
    (display-line-numbers-mode -1)
    (set-window-buffer (selected-window) (current-buffer))
    (hl-line-mode 1))
  (setq dired-listing-switches "-alh --group-directories-first"
        dired-dwim-target t
        dired-mouse-drag-files t                                         ; drag and drop files with mouse
        dired-recursive-copies 'always)
  (use-package diredfl :init (diredfl-global-mode))                      ; make dired look better
  (use-package all-the-icons-dired                                       ; icons for dired buffer
    :hook (dired-mode . all-the-icons-dired-mode))
  (use-package dired-git-info                                            ; show git info in dired
    :bind (:map dired-mode-map (")" . dired-git-info-mode))
    :config (setq dgi-auto-hide-details-p nil)))

(use-package eshell
  :ensure nil
  :hook (eshell-mode . rh/eshell-initialization)
  :custom
  (eshell-prompt-regexp "[$#] ")
  (eshell-prompt-function #'rh/prompt-function)
  (eshell-history-size 100000)
  (eshell-stringify-t nil)
  :config
  (defun rh/eshell-initialization ()
    (display-line-numbers-mode -1)
    (with-current-buffer "*eshell*"
      (setq-local left-margin-width  4
                  right-margin-width 4)
      (set-window-buffer (selected-window) (current-buffer))))

  (defun rh/prompt-function ()
    "A minimalist prompt of the form "
    (let ((promptface `(:foreground "violet"))
          (pwd (abbreviate-file-name (eshell/pwd))))
      (concat
       (propertize pwd 'face promptface) "\n"
       (if (zerop (user-uid)) "# " "→ "))))

  (use-package eat :init (eat-eshell-mode)) ; emulate a terminal
  (use-package esh-autosuggest :init (esh-autosuggest-mode))
  (use-package eshell-syntax-highlighting :init (eshell-syntax-highlighting-global-mode)))

(use-package eshell-toggle
  :bind ("M-S" . eshell-toggle)
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-project-root t))

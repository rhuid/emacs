;;; knot-editor.el --- Edit at the speed of thought (well, not literally) -*- lexical-binding: t; -*-

;;; Some commands for faster editing
;; Most of them are written using built-in functions. Some use functions from `avy'.
;; Helper functions are tagged `helper'.

;; This command is context-sensitive. In most cases, it inserts a space character while joining
;; but when the starting character of the next non-empty line is a closing parenthesis, it leaves no space.
(defun rh/join-line ()
  "Join the current line with the next non-empty line."
  (interactive)
  (save-excursion
    (end-of-line)
    (delete-all-space)
    (unless (rh/at-parenthesis-end-p)
      (insert " "))))

;; helper
(defun rh/at-parenthesis-end-p ()
  "Return non-nil if the character at point is )."
  (looking-at-p "[)]"))

;; A multipurpose trash cleaner without cluttering the kill ring!
(defun rh/delete-in-context ()
  "Delete region (if active), else delete current line (if non-empty), else delete-blank-lines."
  (interactive)
  (cond ((use-region-p)
         ;; If there is a region, delete it
         (let ((inhibit-read-only t))
           (delete-region (region-beginning) (region-end))))
        ;; Else, if the line has a non-whitespace chracter, delete the line
        ((not (string-blank-p (string-trim (thing-at-point 'line t))))
         (let ((inhibit-read-only t))
           (delete-region (line-beginning-position) (line-beginning-position 2))))
        ;; If the line is just whitespace, delete all surrounding lines
        (t (delete-blank-lines))))

;; Kill but in a context-sensitive way
(defun rh/kill-in-context ()
  "Kill region (if active), else kill current line (if non-empty), else delete-blank-lines."
  (interactive)
  (cond ((use-region-p)
         (let ((inhibit-read-only t))
           (call-interactively 'kill-region)))

        ((not (string-blank-p (string-trim (thing-at-point 'line t))))
         (let ((inhibit-read-only t))
           (call-interactively 'kill-whole-line)))

        (t nil)))

;; Like kill-ring-save, but context-sensitive
(defun rh/put-into-kill-ring ()
  "Put region (if active) into kill-ring. Else copy any line from visible screen."
  (interactive)
  (cond ((use-region-p)
         (let ((inhibit-read-only t))
           (call-interactively 'kill-ring-save)))

        ((not (string-blank-p (string-trim (thing-at-point 'line t))))
         (let ((inhibit-read-only t))
           (kill-ring-save (line-beginning-position) (line-beginning-position 2))))

        (t nil)))

;; helper (lol)
(defun rh/insert-space ()
  "Insert a space character."
  (interactive)
  (insert " "))

;;; Enclose region with delimiters
(use-package emacs
  :bind ("C-c s e" . rh/enclose-region)
  :config
  (defun rh/enclose-region (beg end opener)
    "Enclose region with OPENER and its matching closer."
    (interactive
     (list (region-beginning) (region-end)
           (let ((electric-pair-mode nil)) ;; disable pairing in prompt
             (read-string "Open (default \"): "))))
    (let* ((open (if (string-empty-p opener) "\"" opener))
           (close (pcase open
                    ("(" ")")
                    ("[" "]")
                    ("{" "}")
                    ("<" ">")
                    ("'" "'")
                    ("`" "`")
                    (_ "\"")))) ;; fallback
      (save-excursion
        (goto-char end)
        (insert close)
        (goto-char beg)
        (insert open)))))

;; dependency on meow library
(defun rh/contextual-line-expand ()
  "Select current line. If region already exists, expand line."
  (interactive)
  (if (use-region-p)
      (call-interactively 'meow-line-expand)
    (call-interactively 'meow-line)))

;;; `expand-region'
;; Select regions by semantic units
(use-package expand-region
  :demand t
  :bind ("C-=" . er/expand-region))

;;; `multiple-cursors'
;; An army of shadow clones
(use-package multiple-cursors
  :bind (("C-S-l" . mc/edit-lines)
         ("C-S-a" . mc/mark-all-like-this)
         ("C-S-n" . mc/mark-next-like-this)
         ("C-S-p" . mc/mark-previous-like-this)
         ("C->"   . mc/skip-to-next-like-this)
         ("C-<"   . mc/skip-to-previous-like-this)))

;;; My modal design built on `meow'
;; Requires `avy' and `consult'
(defun rh/modal-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-colemak-dh)

  (meow-motion-define-key
   '(":" . mode-line-other-buffer)
   '("e" . meow-prev))

  (meow-leader-define-key
   '("?" . meow-cheatsheet)
   '("1" . meow-digit-argument)         '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)         '("4" . meow-digit-argument)
   '("6" . meow-digit-argument)         '("5" . meow-digit-argument)
   '("7" . meow-digit-argument)         '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)         '("0" . meow-digit-argument))

  (meow-normal-define-key
   '("0" . meow-expand-0)               '("1" . meow-expand-1)
   '("2" . meow-expand-2)               '("3" . meow-expand-3)
   '("4" . meow-expand-4)               '("5" . meow-expand-5)
   '("6" . meow-expand-6)               '("7" . meow-expand-7)
   '("8" . meow-expand-8)               '("9" . meow-expand-9)
   '("-" . negative-argument)
   '(";" . meow-reverse)
  '(":" . mode-line-other-buffer)
  '("," . meow-inner-of-thing)         '("." . meow-bounds-of-thing)
  '("<" . meow-beginning-of-thing)     '(">" . meow-end-of-thing)
  '("/" . meow-visit)
  '("a" . meow-append)                 '("A" . meow-open-below)
  '("b" . meow-back-word)              '("B" . meow-back-symbol)
  '("c" . meow-change)
  '("d" . rh/delete-in-context)        '("D" . delete-all-space)
  '("e" . meow-prev-expand)            '("E" . scroll-down-command)
  '("f" . rh/insert-space)
  '("g" . meow-cancel-selection)       '("G" . meow-grab)
  '("h" . meow-mark-word)              '("H" . meow-mark-symbol)
  '("i" . meow-right-expand)
  '("j" . meow-join)                   '("J" . rh/join-line)
  '("k" . rh/kill-in-context)          '("K" . avy-move-region)
  '("l" . meow-line)                   '("L" . consult-goto-line)
  '("m" . meow-left-expand)
  '("n" . meow-next-expand)            '("N" . scroll-up-command)
  '("o" . meow-block)                  '("O" . meow-to-block)
  '("p" . rh/put-into-kill-ring)       '("P" . avy-copy-region)
  '("q" . meow-quit)                   '("Q" . delete-window)
  '("r" . meow-replace)
  '("s" . meow-insert)                 '("S" . meow-open-above)
  '("t" . meow-till-expand)
  '("u" . meow-undo)                   '("U" . meow-undo-in-selection)
  '("v" . meow-search)
  '("w" . meow-next-word)              '("W" . meow-next-symbol)
  '("x" . delete-char)
  '("y" . yank)                        '("Y" . yank-pop)
  '("z" . meow-pop-selection)
  '("'" . repeat)))

;;; `meow'
(use-package meow
  :demand t
  :vc (:url "https://github.com/meow-edit/meow")
  :hook ((post-self-insert-hook . rh/go-normal-state))
  :config
  (rh/modal-setup)
  (meow-global-mode 1)
  (define-key meow-insert-state-keymap [escape] nil)
  (define-key meow-motion-state-keymap [escape] nil)

  (setq meow-cursor-type-motion '(bar . 0)) ; Remove cursor in motion mode
  (setq meow-expand-hint-remove-delay 0) ; Remove that annoying position hint while selecting
  (setq meow-keypad-message nil
        meow-keypad-self-insert-undefined nil
        meow-use-clipboard t)

  (defun rh/go-normal-state ()
    "Type 'ntn' to go to normal state from insert state."
    (when (and (eq meow--current-state 'insert)
               (>= (point) 3)
               (string= (buffer-substring-no-properties (-(point) 3) (point)) "ntn"))
      (delete-region (- (point) 3) (point)) ; Delete "ntn"
      (meow-normal-mode)
      t))

  (defcustom rh/esc-timeout 0.125
    "Timeout (seconds) to wait after ESC in insert state for a following key.
If no key arrives within this interval, ESC will switch to normal (meow)."
    :type 'number
    :group 'rh)

  (defun rh/esc-in-insert-state ()
    "If another key follows within `rh/esc-timeout`, let ESC act as Meta prefix.
If nothing follows within the timeout, switch to meow normal state."
    (interactive)
    ;; read an event with timeout; returns nil if none
    (let ((evt (read-event nil nil rh/esc-timeout)))
      (if evt
          ;; A key was pressed: push ESC then the key back onto unread events, so process ESC followed by that key (i.e. Meta+key).
          (let ((esc-events (listify-key-sequence (kbd "ESC")))
                (evt-list (if (vectorp evt)
                              (append (listify-key-sequence evt) nil) (list evt))))
            (setq unread-command-events
                  (append esc-events evt-list unread-command-events)))

        (meow-normal-mode))))

  (define-key meow-insert-state-keymap (kbd "<escape>") #'rh/esc-in-insert-state))

(provide 'knot-editor)

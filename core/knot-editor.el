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

;;; Concerning enclosing and removing delimiters
(use-package emacs
  :bind (("C-c s e" . rh/enclose-region)
         ("C-c s u" . rh/delete-surrounding-chars))
  :config
  (defun rh/enclose-region (beg end char)
    "Enclose region with CHAR and its matching pair."
    (interactive "r\ncEnter opener: ")
    (let* ((open (string char))
           (close (pcase open
                    ("(" ")") ("[" "]") ("{" "}") ("<" ">")
                    ("'" "'") ("`" "'") ("\"" "\"")
                    (_ open)))) ;; fallback: repeat same char
      (save-excursion
        (goto-char end)
        (insert close)
        (goto-char beg)
        (insert open))))
  (defun rh/delete-surrounding-chars (beg end)
    "Remove the first and last character in the active region."
    (interactive "r")
    (when (> (- end beg) 1) ;; only if region has at least 2 chars
      (save-excursion
        (goto-char end)
        (backward-delete-char 1)
        (goto-char beg)
        (delete-char 1)))))

;; dependency on meow library
(defun rh/contextual-line-expand ()
  "Select current line. If region already exists, expand line."
  (interactive)
  (if (use-region-p)
      (call-interactively 'meow-line-expand)
    (call-interactively 'meow-line)))

;;; My modal design built on `meow'
;; Requires `avy' and `consult'
(defun rh/modal-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-colemak-dh)

  (meow-motion-define-key
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
   '(":" . align-regexp)
   '("," . meow-inner-of-thing)         '("." . meow-bounds-of-thing)
   '("<" . meow-beginning-of-thing)     '(">" . meow-end-of-thing)
   '("/" . meow-visit)
   '("a" . er/expand-region)            '("A" . er/contract-region)
   '("b" . meow-block)                  '("B" . meow-to-block)
   '("c" . meow-change)
   '("d" . rh/delete-in-context)        '("D" . delete-all-space)
   '("e" . meow-prev-expand)            '("E" . backward-sentence)
   '("f" . forward-word)                '("F" . forward-paragraph)
   '("g" . meow-cancel-selection)       '("G" . meow-grab)
   '("h" . meow-mark-word)              '("H" . meow-mark-symbol)
   '("i" . meow-right-expand)
   '("j" . rh/join-line)                '("J" . meow-join)
   '("k" . rh/kill-in-context)          '("K" . avy-move-region)
   '("l" . meow-line)                   '("L" . consult-goto-line)
   '("m" . meow-left-expand)
   '("n" . meow-next-expand)            '("N" . forward-sentence)
   '("o" . meow-open-below)             '("O" . meow-open-above)
   '("p" . rh/put-into-kill-ring)       '("P" . avy-copy-region)
   '("q" . meow-quit)                   '("Q" . delete-window)
   '("r" . meow-replace)
   '("s" . meow-insert-mode)
   '("t" . meow-till-expand)            '("T" . meow-swap-grab)
   '("u" . meow-undo)                   '("U" . vundo)
   '("v" . meow-search)
   '("w" . backward-word)               '("W" . backward-paragraph)
   '("x" . delete-char)
   '("y" . yank)                        '("Y" . yank-pop)
   '("z" . meow-pop-selection)
   '("'" . repeat)))

;;; `meow'
(use-package meow
  :demand t
  :config
  (rh/modal-setup)
  (meow-global-mode 1)
  (define-key meow-motion-state-keymap [escape] nil)
  (setq meow-cursor-type-motion '(bar . 0))
  (setq meow-expand-hint-remove-delay 0)
  (setq meow-keypad-message nil
        meow-keypad-self-insert-undefined nil
        meow-use-clipboard t))

(provide 'knot-editor)

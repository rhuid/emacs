;;; knot-editing.el --- Some quality of life editing commands that attempt to elevate vanilla Emacs editing experience -*- lexical-binding: t; -*-

(defun rh/act-inside (&optional arg)
  "Kill or copy the content inside the current balanced expression.
With \\[universal-argument] prefix, it copies. Otherwise, it kills."
  (interactive "P")
  (save-mark-and-excursion
    (backward-up-list 1 t t)
    (forward-char 1)
    (let ((beg (point)))
      (backward-up-list -1 t t)
      (backward-char 1)
      (if arg
          (kill-ring-save beg (point))
        (kill-region beg (point))))))

(defun rh/visit-next-sexp (&optional arg)
  "Move the point to the inside of the next sexp of the same level.
With ARG, it moves forward ARG times.
With negative ARG, it moves backward that many times."
  (interactive "p")
  (backward-up-list -1 t t)
  (forward-sexp arg nil)
  (backward-sexp 1 nil)
  (forward-char 1))

(defun rh/visit-previous-sexp (&optional arg)
  "Move the point to the inside of the previous sexp of the same level.
With ARG, it moves backward ARG times.
With negative ARG, it moves forward that many times."
  (interactive "p")
  (rh/visit-next-sexp (- arg)))

(defun rh/forward-opening-delimiter (&optional arg)
  "Jump forward to the next opening delimiter.
Opening delimiters are one of: ( [ { < ` \" '.
With optional ARG, jump |ARG| times; negative ARG means jump backward."
  (interactive "p")
  (let* ((arg (or arg 1))
         (delimiters "(\\|\\[\\|{\\|<\\|[`\"']"))
    (if (< arg 0)
        (dotimes (_ (- arg))
          (unless (re-search-backward delimiters nil t)
            (cl-return)))
      (dotimes (_ arg)
        (unless (re-search-forward delimiters nil t)
          (cl-return))))))

(defun rh/backward-opening-delimiter (&optional arg)
  "Jump backward to the previous opening delimiter.
Same as calling `rh/forward-opening-delimiter` with negative ARG."
  (interactive "p")
  (rh/forward-opening-delimiter (- (or arg 1))))

(defun rh/change-inside-forward (&optional arg)
  "Jump to the next delimiter and kill contents of the parent balanced expression.
Prefix argument \\[universal-argument] does the same but on the previous delimiter."
  (interactive "P")
  (if arg
      (progn
        (rh/backward-opening-delimiter 2)
        (forward-char 1))
    (rh/forward-opening-delimiter 1))
  (rh/act-inside))

(defun rh/forward-up-list (&optional arg)
  "Move forward out of one level of parentheses.
This command will also work on other parentheses-like expressions
defined by the current language mode.  With ARG, do this that
many times.  A negative argument means move backward."
  (interactive "p")
  (backward-up-list (- arg) t t))

(defun rh/unwrap-parent-sexp ()
  "Remove delimiters of the parent sexp."
  (interactive)
  (save-excursion
    (backward-up-list 1 t t)
    (delete-pair 1)))

(defun rh/chop-off-sexp (&optional arg)
  "Chop off the rest of the higher level sexp.
With \\[universal-argument], it chops backward."
  (interactive "P")
  (set-mark (point))
  (if arg
      (progn
        (backward-up-list 1 t t)
        (forward-char 1))
    (backward-up-list -1 t t)
    (backward-char 1))
  (when (use-region-p)
    (kill-region (region-beginning) (region-end))))

(defun rh/backward-chop-off-sexp (&optional arg)
  "Chop off backward the rest of the higher level sexp."
  (interactive "P")
  (rh/chop-off-sexp (not arg)))

(defun rh/kill-word (&optional arg)
  "Kill the whole word and tries to fix up whitespace after killing.
With ARG, perform this action that many times.
Negative ARG kills that many previous words.
Also kills word backward if the point is at the end of the word."
  (interactive "p")
  (let ((arg (or arg 1)))
    (if (< arg 0)
        (progn
          (let ((b (bounds-of-thing-at-point 'word)))
            (unless (and b (= (point) (cdr b)))
              (forward-word 1)))
          (kill-word arg))
      (let ((b (bounds-of-thing-at-point 'word)))
        (unless (and b (<= (point) (car b)))
          (if b
              (goto-char (car b))
            (backward-word 1))))
      (kill-word arg)))
  (when (looking-at " +")
    (just-one-space)))

(defun rh/copy-word (&optional arg)
  "Copy the current word without moving point.
With ARG, copy that many words; negative ARG copies backward."
  (interactive "p")
  (let ((arg (or arg 1)))
    (save-excursion
      (let* ((start (point))
             (bounds (bounds-of-thing-at-point 'word)))
        (if (< arg 0)
            (progn
              (when (and bounds (< (point) (cdr bounds)))
                (goto-char (cdr bounds)))
              (kill-ring-save (point)
                              (progn (backward-word (- arg)) (point))))
          (progn
            (when (and bounds (> (point) (car bounds)))
              (goto-char (car bounds)))
            (kill-ring-save (point)
                            (progn (forward-word arg) (point)))))))))

(defun rh/isearch-remote-copy (&optional arg)
  "In `isearch', copy ARG words and return to the original point.
ARG defaults to 1. Negative ARG copies backward.

Uses `rh/copy-word' under the hood."
  (interactive "p")
  (let* ((count (or arg 1))
         (in-isearch (bound-and-true-p isearch-mode)))
    (if in-isearch
        (let* ((isearch-pos (point))
               (other-pos (or (and (boundp 'isearch-other-end) isearch-other-end)
                              isearch-pos))
               (match-start (min isearch-pos other-pos))
               (match-end   (max isearch-pos other-pos))
               (copy-pos (if (< count 0) match-end match-start))
               (orig-point (or (and (boundp 'isearch-opoint) isearch-opoint)
                               (and (boundp 'isearch--opoint) isearch--opoint)
                               (point))))
          (save-excursion
            (goto-char copy-pos)
            (rh/copy-word count))
          (isearch-exit)
          (goto-char orig-point))
      (message "This command should be invoked in isearch mode."))))

(defun rh/isearch-remote-yank (&optional arg)
  "While in `isearch-mode', yanks the word in search match at the original point.
With ARG, yank that many words; negative ARG yanks that many previous words."
  (interactive "p")
  (if isearch-mode
      (progn
        (rh/isearch-remote-copy arg)
        (yank 1))
    (message "This command should be invoked in isearch-mode.")))

(defun rh/kill-sentence (&optional arg)
  "Kill the current sentence.
With ARG, perform this action that many times.
Negative ARG kills that many previous sentences."
  (interactive "p")
  (unless (bobp)
    (backward-char 1))
  (forward-sentence 1)
  (backward-sentence 1)
  (kill-sentence arg)
  (just-one-space))

(defun rh/copy-sentence (&optional arg)
  "Copy the current sentence.
With ARG, perform this action that many times.
Negative ARG copies that many previous sentences."
  (interactive "p")
  (save-excursion
    (unless (bobp)
      (backward-char 1))
    (forward-sentence 1)
    (backward-sentence 1)
    (mark-end-of-sentence arg)
    (kill-ring-save (region-beginning) (region-end))))

(defun rh/break-sentence (&optional arg)
  "Start the next sentence in a new line and move the cursor there.
With ARG, perform this action that many times."
  (interactive "p")
  (dotimes (_ arg)
    (forward-sentence 1)
    (newline 1)
    (delete-horizontal-space nil)))

(defun rh/chop-off-buffer (&optional arg)
  "Kill the rest of the buffer after point.
With ARG, it deletes instead (does not save to the kill-ring)."
  (interactive "P")
  (if arg
      (delete-region (point) (point-max))
    (kill-region (point) (point-max))))

(defun rh/backward-chop-off-buffer (&optional arg)
  "Kill the rest of the buffer before point.
  With ARG, it deletes instead (does not save to the kill-ring)."
  (interactive "P")
  (if arg
      (delete-region (point-min) (point))
    (kill-region (point-min) (point))))

(defun rh/select-line (&optional arg)
  "Select the current line.
  With ARG, select that many lines; negative ARG selects previous lines."
  (interactive "p")
  (beginning-of-line)
  (set-mark (point))
  (forward-line (1- arg))
  (end-of-line)
  (exchange-point-and-mark nil))

(defun rh/open-line-below (&optional arg)
  "Create a new line below and move the cursor there.
With ARG, perform this action that many times."
  (interactive "p")
  (move-end-of-line 1)
  (newline-and-indent arg))

(defun rh/open-line-above (&optional arg)
  "Create a new line above and move the cursor there.
With ARG, perform this action that many times."
  (interactive "p")
  (back-to-indentation)
  (open-line arg))

(defun rh/join-line (&optional arg)
  "Join the current line to the following line.
With ARG, perform this action that many times."
  (interactive "p")
  (save-excursion
    (dotimes (_ arg)
      (join-line -1))))

;; Global keybindings for the commands defined in this module
(bind-key "M-i" 'rh/act-inside)
(bind-key "M-I" 'rh/change-inside-forward)
(bind-key "H-f" 'rh/visit-next-sexp)
(bind-key "H-b" 'rh/visit-previous-sexp)
(bind-key "C-H-f" 'rh/forward-opening-delimiter)
(bind-key "C-H-b" 'rh/backward-opening-delimiter)
(bind-key "C-M-y" 'rh/forward-up-list)
(bind-key "C-M-)" 'rh/chop-off-sexp)
(bind-key "C-M-(" 'rh/backward-chop-off-sexp)
(bind-key "M-D" 'rh/kill-word)
(bind-key "C-;" 'rh/copy-word)
(bind-key "C-H-d" 'rh/kill-sentence)
(bind-key "C-H-w" 'rh/copy-sentence)
(bind-key "C-M-S-k" 'rh/chop-off-buffer)
(bind-key "C-M-S-h" 'rh/backward-chop-off-buffer)
(bind-key "C-'" 'rh/select-line)
(bind-key "C-<return>" 'rh/open-line-below)
(bind-key "C-S-<return>" 'rh/open-line-above)
(bind-key "C-j" 'rh/join-line)
(bind-key "M-r M-s" 'rh/unwrap-parent-sexp)
(bind-key "M-j" 'rh/break-sentence)

;; `isearch-mode' keybindings
(define-key isearch-mode-map (kbd "C-;") 'rh/isearch-remote-copy)
(define-key isearch-mode-map (kbd "C-Y") 'rh/isearch-remote-yank)

;; A prefix key for more sexp operations
(define-prefix-command 'rh/sexp-map)
(bind-key "C-M-c" 'rh/sexp-map)
(keymap-set rh/sexp-map (kbd "u") 'rh/unwrap-parent-sexp)

(provide 'knot-editing)

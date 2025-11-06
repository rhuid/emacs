;;; knot-editing.el --- Some commands for more efficient editing -*- lexical-binding: t; -*-

(defun rh/join-line (&optional ARG)
  "Join the current line to the following line.
With ARG, perform this action that many times."
  (interactive "p")
  (save-excursion
    (dotimes (_ ARG)
      (join-line -1))))

(bind-key "C-j" 'rh/join-line)

(defun rh/chop-off-buffer (&optional ARG)
  "Kill the rest of the buffer after point.
With ARG, it deletes instead (does not save to the kill-ring)."
  (interactive "P")
  (if ARG
      (delete-region (point) (point-max))
    (kill-region (point) (point-max))))

(defun rh/backward-chop-off-buffer (&optional ARG)
  "Kill the rest of the buffer before point.
With ARG, it deletes instead (does not save to the kill-ring)."
  (interactive "P")
  (if ARG
      (delete-region (point-min) (point))
    (kill-region (point-min) (point))))

(bind-key "C-M-S-k" 'rh/chop-off-buffer)
(bind-key "C-M-S-h" 'rh/backward-chop-off-buffer)

(defun rh/kill-word (&optional ARG)
  "Kill the whole word and tries to fix up whitespace after killing.
With ARG, perform this action that many times.
Negative ARG kills that many previous words.

This command is best used with `isearch'.
However, in most cases, the built-in `kill-word' might be better suited."
  (interactive "p")
  (let ((ARG (or ARG 1)))
    (if (< ARG 0)
        (progn
          (let ((b (bounds-of-thing-at-point 'word)))
            (unless (and b (= (point) (cdr b)))
              (forward-word 1)))
          (kill-word ARG))
      (let ((b (bounds-of-thing-at-point 'word)))
        (unless (and b (<= (point) (car b)))
          (if b
              (goto-char (car b))
            (backward-word 1))))
      (kill-word ARG)))
  (when (looking-at " " nil)
    (cycle-spacing t)))

(bind-key "H-d" 'rh/kill-word)

(defun rh/kill-sentence (&optional ARG)
  "Kill the current sentence.
With ARG, perform this action that many times.
Negative ARG kills that many previous sentences."
  (interactive "p")
  (unless (looking-back (sentence-end) (line-beginning-position))
    (backward-sentence 1))
  (kill-sentence ARG)
  (cycle-spacing t))

(bind-key "C-#" 'rh/kill-sentence)

(defun rh/copy-sentence (&optional ARG)
  "Copy the current sentence.
With ARG, perform this action that many times.
Negative ARG copies that many previous sentences."
  (interactive "p")
  (save-excursion
    (unless (looking-back (sentence-end) (line-beginning-position))
      (backward-sentence 1))
    (set-mark (point))
    (forward-sentence ARG)
    (kill-ring-save (region-beginning) (region-end))))

(bind-key "C-@" 'rh/copy-sentence)

(defun rh/open-line-below (&optional ARG)
  "Create a new line below and move the cursor there.
With ARG, perform this action that many times."
  (interactive "p")
  (move-end-of-line 1)
  (newline-and-indent ARG))

(defun rh/open-line-above (&optional ARG)
  "Create a new line above and move the cursor there.
With ARG, perform this action that many times."
  (interactive "p")
  (back-to-indentation)
  (open-line ARG))

(bind-key "C-<return>" 'rh/open-line-below)
(bind-key "C-S-<return>" 'rh/open-line-above)

(provide 'knot-editing)

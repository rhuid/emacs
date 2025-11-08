;;; knot-editing.el --- Some quality of life editing commands that attempt to elevate vanilla Emacs editing experience -*- lexical-binding: t; -*-

(defun rh/act-inside (&optional ARG)
  "Kill or copy the content inside the current balanced expression.
With \\[universal-argument] prefix, it copies. Otherwise, it kills.

Its behavior is major mode specific as it uses sexp under the hood.
Also, it may not work inside comments."
  (interactive "P")
  (save-excursion
    (backward-up-list 1 t t)
    (mark-sexp 1 nil)
    (when (use-region-p)
      (forward-char 1)
      (exchange-point-and-mark nil)
      (backward-char 1)
      (if ARG
          (kill-ring-save (region-beginning) (region-end))
        (kill-region (region-beginning) (region-end))))))

(bind-key "M-i" 'rh/act-inside)

(defun rh/visit-next-sexp (&optional ARG)
  "Move the point to the inside of the next sexp of the same level.
With ARG, it moves forward ARG times.
With negative ARG, it moves backward that many times."
  (interactive "p")
  (backward-up-list 1 t t)
  (forward-sexp (+ 1 ARG) nil)
  (backward-sexp 1 nil)
  (forward-char 1))

(defun rh/visit-previous-sexp (&optional ARG)
  "Move the point to the inside of the previous sexp of the same level.
With ARG, it moves backward ARG times.
With negative ARG, it moves forward that many times."
  (interactive "p")
  (rh/visit-next-sexp (- ARG)))

(bind-key "H-f" 'rh/visit-next-sexp)
(bind-key "H-b" 'rh/visit-previous-sexp)

(defun rh/kill-word (&optional ARG)
  "Kill the whole word and tries to fix up whitespace after killing.
With ARG, perform this action that many times.
Negative ARG kills that many previous words.
Also kills word backward if the point is at the end of the word."
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
  (when (looking-at " +")
    (just-one-space)))

(bind-key "<Ci>" 'rh/kill-word)

(defun rh/copy-word (&optional ARG)
  "Copy the current word without moving point.
With ARG, copy that many words; negative ARG copies backward."
  (interactive "p")
  (let ((ARG (or ARG 1)))
    (save-excursion
      (let* ((start (point))
             (bounds (bounds-of-thing-at-point 'word)))
        (if (< ARG 0)
            (progn
              (when (and bounds (< (point) (cdr bounds)))
                (goto-char (cdr bounds)))
              (kill-ring-save (point)
                              (progn (backward-word (- ARG)) (point))))
          (progn
            (when (and bounds (> (point) (car bounds)))
              (goto-char (car bounds)))
            (kill-ring-save (point)
                            (progn (forward-word ARG) (point)))))))))

(defun rh/isearch-remote-copy (&optional ARG)
  "In `isearch', copy ARG words and return to the original point.
ARG defaults to 1. Negative ARG copies backward.

Uses `rh/copy-word' under the hood."
  (interactive "p")
  (let* ((count (or ARG 1))
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

(defun rh/isearch-remote-yank (&optional ARG)
  "While in `isearch-mode', yanks the word in search match at the original point.
With ARG, yank that many words; negative ARG yanks that many previous words."
  (interactive "p")
  (if isearch-mode
      (progn
        (rh/isearch-remote-copy ARG)
        (yank 1))
    (message "This command should be invoked in isearch-mode.")))

(bind-key "C-;" 'rh/copy-word)
(define-key isearch-mode-map (kbd "C-;") 'rh/isearch-remote-copy)
(define-key isearch-mode-map (kbd "C-Y") 'rh/isearch-remote-yank)

(defun rh/kill-sentence (&optional ARG)
  "Kill the current sentence.
With ARG, perform this action that many times.
Negative ARG kills that many previous sentences."
  (interactive "p")
  (unless (looking-back (sentence-end) (line-beginning-position))
    (backward-sentence 1))
  (kill-sentence ARG)
  (cycle-spacing t))

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

(bind-key "C-#" 'rh/kill-sentence)
(bind-key "C-@" 'rh/copy-sentence)

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

(defun rh/select-line (&optional ARG)
  "Select the current line.
With ARG, select that many lines; negative ARG selects previous lines."
  (interactive "p")
  (beginning-of-line)
  (set-mark (point))
  (forward-line (1- ARG))
  (end-of-line)
  (exchange-point-and-mark nil))

(bind-key "C-'" 'rh/select-line)

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

(defun rh/join-line (&optional ARG)
  "Join the current line to the following line.
With ARG, perform this action that many times."
  (interactive "p")
  (save-excursion
    (dotimes (_ ARG)
      (join-line -1))))

(bind-key "C-j" 'rh/join-line)

(provide 'knot-editing)

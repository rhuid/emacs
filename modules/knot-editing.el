;;; knot-editing.el --- Some commands for more efficient editing -*- lexical-binding: t; -*-

(defun rh/join-line (&optional ARG)
  "Join the current line to the following ARG lines and fix up whitespace at join. ARG defaults to 1."
  (interactive "p")
  (save-excursion
    (dotimes (i ARG)
      (join-line -1))))

(bind-key "C-j" 'rh/join-line)

(defun rh/chop-off-buffer (&optional ARG)
  "Kill the rest of the buffer after point (or before point with prefix ARG)."
  (interactive "P")
  (if ARG
      (kill-region (point-min) (point))
    (kill-region (point) (point-max))))

(defun rh/backward-chop-off-buffer (&optional ARG)
  "Kill the rest of the buffer before point (or after point with prefix ARG)."
  (interactive "P")
  (if ARG
      (kill-region (point) (point-max))
    (kill-region (point-min) (point))))

(bind-key "C-M-S-k" 'rh/chop-off-buffer)
(bind-key "C-M-S-h" 'rh/backward-chop-off-buffer)

(defun rh/copy-sentence (&optional ARG)
  "Copy ARG sentences. ARG defaults to 1 (copy the current sentence). With negative ARG, copy the previous sentence(s)."
  (interactive "p")
  (save-excursion
    (backward-sentence 1)
    (set-mark (point))
    (forward-sentence ARG)
    (kill-ring-save (region-beginning) (region-end))))

(bind-key "C-@" 'rh/copy-sentence)

(defun rh/open-line-below ()
  "Create a new line below and move the cursor there."
  (interactive)
  (move-end-of-line 1)
  (newline t t))

(defun rh/open-line-above ()
  "Create a new line above and move the cursor there."
  (interactive)
  (mwim-beginning-of-code-or-line)
  (open-line 1))

(bind-key "C-<return>" 'rh/open-line-below)
(bind-key "C-S-<return>" 'rh/open-line-above)

(provide 'knot-editing)

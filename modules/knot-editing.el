;;; knot-editing.el --- Some commands for more efficient editing -*- lexical-binding: t; -*-

(defun rh/join-line (&optional ARG)
  "Join the current line to the following ARG lines and fix up whitespace at join. ARG defaults to 1."
  (interactive "p")
  (save-excursion
    (dotimes (i ARG)
      (join-line -1))))

(bind-key "C-j" 'rh/join-line)

(defun rh/chop-off-buffer (&optional ARG)
  "Kill the rest of the buffer after point. With ARG, it deletes instead (does not save to the kill-ring)."
  (interactive "P")
  (if ARG
      (kill-region (point) (point-max))
    (delete-region (point) (point-max))))

(defun rh/backward-chop-off-buffer (&optional ARG)
  "Kill the rest of the buffer before point. With ARG, it deletes instead (does not save to the kill-ring)."
  (interactive "P")
  (if ARG
      (kill-region (point-min) (point))
    (delete-region (point-min) (point))))

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

(defun rh/open-line-below (&optional ARG)
  "Create a new line below and move the cursor there. With ARG, perform this action that many times."
  (interactive "p")
  (move-end-of-line 1)
  (newline-and-indent ARG))

(defun rh/open-line-above (&optional ARG)
  "Create a new line above and move the cursor there. With ARG, perform this action that many times."
  (interactive "p")
  (mwim-beginning-of-code-or-line)
  (open-line ARG))

(bind-key "C-<return>" 'rh/open-line-below)
(bind-key "C-S-<return>" 'rh/open-line-above)

(provide 'knot-editing)

;;; knot-editing.el --- Some commands for more efficient editing -*- lexical-binding: t; -*-

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

(defun rh/chop-off-buffer (&optional arg)
  "Kill the rest of the buffer after point (or before point with prefix ARG)."
  (interactive "P")
  (if arg
      (kill-region (point-min) (point))
    (kill-region (point) (point-max))))

(defun rh/backward-chop-off-buffer (&optional arg)
  "Kill the rest of the buffer before point (or after point with prefix ARG)."
  (interactive "P")
  (if arg
      (kill-region (point) (point-max))
    (kill-region (point-min) (point))))

(bind-key "C-M-S-k" 'rh/chop-off-buffer)
(bind-key "C-M-S-h" 'rh/backward-chop-off-buffer)

(provide 'knot-editing)

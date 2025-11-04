;;; knot-editing.el --- Some commands for more efficient editing -*- lexical-binding: t; -*-

(defun rh/open-line-below ()
  "Create a new line below and move the cursor there."
  (interactive)
  (move-end-of-line 1)
  (call-interactively 'newline))

(defun rh/open-line-above ()
  "Create a new line above and move the cursor there."
  (interactive)
  (mwim-beginning-of-code-or-line)
  (call-interactively 'open-line))

(bind-key "C-<return>" 'rh/open-line-below)
(bind-key "C-S-<return>" 'rh/open-line-above)

(defun rh/backward-chop-off-buffer ()
  "Delete the rest of the buffer before the point."
  (interactive)
  (call-interactively 'set-mark-command)
  (call-interactively 'beginning-of-buffer)
  (call-interactively 'delete-region))

(defun rh/chop-off-buffer ()
  "Delete the rest of the buffer after the point."
  (interactive)
  (call-interactively 'set-mark-command)
  (call-interactively 'end-of-buffer)
  (call-interactively 'delete-region))

(bind-key "C-M-S-k" 'rh/chop-off-buffer)
(bind-key "C-M-S-h" 'rh/backward-chop-off-buffer)

(provide 'knot-editing)

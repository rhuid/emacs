;;; knot-editing.el --- Some commands for better editing -*- lexical-binding: t; -*-

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

(bind-key "C-S-o" 'rh/open-line-above)                     ; a counterpart to `C-o'
(bind-key "C-<return>" 'rh/open-line-below)                ; like in org-mode, so you don't have to learn a new keybinding

(provide 'knot-editing)

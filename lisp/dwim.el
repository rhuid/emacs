;;; dwim.el --- 'Do What I Mean' commands for killing, copying and commenting -*- lexical-binding: t; -*-
;; This is like `whole-line-or-region' but minimal (just about 30 lines)
;; and doesn't do anything unnecessary (like remapping built-in commands)

;;; Code:

(defun rh/kill-region-dwim (&optional arg)
  "Kill ARG whole lines or region.
ARG can be either positive or negative."
  (interactive "p")
  (if (use-region-p)
      (kill-region nil nil t)
    (kill-whole-line arg)))

(defun rh/kill-ring-save-dwim (&optional arg)
  "Copy ARG whole lines or region.
ARG can be either positive or negative."
  (interactive "p")
  (if (use-region-p)
      (kill-ring-save nil nil t)
    (save-mark-and-excursion
      (beginning-of-line)
      (let ((beg (point)))
        (end-of-line arg)
        (kill-ring-save beg (point) nil)))))

(defun rh/comment-whole-line-or-region (&optional arg)
  "Comment ARG whole lines or region.
ARG can be either positive or negative."
  (interactive "p")
  (if (use-region-p)
      (comment-dwim nil)
    (save-mark-and-excursion
      (beginning-of-line)
      (set-mark (point))
      (end-of-line arg)
      (comment-dwim nil))))

(bind-key "C-w"   'rh/kill-region-dwim)
(bind-key "M-w"   'rh/kill-ring-save-dwim)
(bind-key "C-M-;" 'rh/comment-whole-line-or-region)

(provide 'dwim)

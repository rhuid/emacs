;;; rh-line-or-region.el --- context-sensitive killing, copying, commenting, replacing -*- lexical-binding: t; -*-

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

(defun rh/replace-line-or-region (&optional arg)
  "Replace current line with the ARGth most recent kill.
If there is active region, replace the active region instead.
ARG defaults to 1."
  (interactive "p")
  (if (use-region-p)
      (progn
        (kill-region (region-beginning) (region-end) nil)
        (yank (1+ arg)))
    (kill-whole-line 1)
    (yank (1+ arg))
    (insert "\n")))

(global-set-key (kbd "C-w") 'rh/kill-region-dwim)
(global-set-key (kbd "M-w") 'rh/kill-ring-save-dwim)
(global-set-key (kbd "C-M-;") 'rh/comment-whole-line-or-region)
(global-set-key (kbd "C-S-y") 'rh/replace-line-or-region)

(provide 'rh-line-or-region)

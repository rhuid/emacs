;;; knot-misc.el --- Miscellaneous stuffs, that do not fit in other modules -*- lexical-binding: t; -*-

;; An alternative to invoking `C-M-w' and then `C-w'.
(defun rh/append-kill-region ()
  "Kill but append to the previous kill."
  (interactive)
  (append-next-kill)
  (call-interactively 'whole-line-or-region-kill-region))

(global-set-key (kbd "C-S-w") 'rh/append-kill-region)

;; An alternative to invoking `C-M-w' and then `M-w'.
(defun rh/append-kill-ring-save ()
  "Kill-ring-save but append to the previous kill."
  (interactive)
  (append-next-kill)
  (call-interactively 'whole-line-or-region-kill-ring-save))

(global-set-key (kbd "M-W") 'rh/append-kill-ring-save)

;; Date Formats for use in `yasnippet'
(defun rh/date-format-candidates ()
  "Return an alist of (display . format-string) for yasnippet date choices."
  (mapcar (lambda (fmt)
            (cons (format "%-20s → %s" fmt (format-time-string fmt)) fmt))
          '("%Y-%m-%d"          ;; 2025-09-19
            "%d/%m/%Y"          ;; 19/09/2025
            "%A, %B %d, %Y"     ;; Friday, September 19, 2025
            "%b %d, %Y"         ;; Sep 19, 2025
            "%Y-%m-%d %H:%M"))) ;; 2025-09-19 20:31

(provide 'knot-misc)

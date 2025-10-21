;;; knot-misc.el --- Miscellaneous stuffs, that do not fit in other modules -*- lexical-binding: t; -*-

;; Custom scratch buffers
(use-package emacs
  :bind (("C-c u o" . rh/toggle-org-scratch)
         ("C-c u e" . rh/toggle-lisp-scratch))
  :config
  (defun rh/toggle-scratch-buffer (name mode initial-content)
    "Toggle a named scratch buffer NAME in MODE with optional INITIAL-CONTENT."
    (let ((buf (get-buffer name)))
      (if buf
          (if (eq (current-buffer) buf)
              (switch-to-buffer (other-buffer))
            (switch-to-buffer buf))
        (let ((new-buf (get-buffer-create name)))
          (with-current-buffer new-buf
            (funcall mode)
            (unless (string= (buffer-string) "") (erase-buffer))
            (insert initial-content)
            (goto-char (point-max)))
          (switch-to-buffer new-buf)))))
  (defun rh/toggle-org-scratch ()
    "Toggle `*org-scratch*` buffer."
    (interactive)
    (rh/toggle-scratch-buffer "*org-scratch*" #'org-mode "#+TITLE: Org Scratch\n\n"))
  (defun rh/toggle-lisp-scratch ()
    "Toggle `*lisp-scratch*'."
    (interactive)
    (rh/toggle-scratch-buffer "*lisp-scratch*" #'lisp-interaction-mode ";; Lisp Scratch\n\n")))

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

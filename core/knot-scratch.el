;;; knot-scratch.el --- Why have only one scratch buffer when you can have many, for different needs?  -*- lexical-binding: t; -*-

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

(defun rh/toggle-lean-scratch ()
  "Toggle `*lean-scratch*` buffer."
  (interactive)
  (require 'lean4-mode)
  (rh/toggle-scratch-buffer "*lean-scratch*" #'lean4-mode "/- Lean Scratch Buffer -/\n\n"))

;;; Optionally redefine default *scratch* as Emacs Lisp
;; (setq initial-major-mode 'emacs-lisp-mode)
;; (setq initial-scratch-message ";; Emacs Lisp Scratch Buffer\n\n")

(provide 'knot-scratch)

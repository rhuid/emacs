;;;; Extra code dumped here
;;; Basically the things I don't need right now but may need someday

;; Concerning themes
(setq custom-safe-themes t)                                             ; don't ask for confirmation for loading themes
(mapc #'disable-theme custom-enabled-themes)                            ; clean up all debris (before loading a new theme)

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

;;; knot-special-buffers.el --- For special buffers like ibuffer, magit-section, custom scratch buffers, etc. -*- lexical-binding: t; -*-

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

;; Custom scratch buffers
(use-package emacs
  :bind ("C-c u o" . rh/toggle-org-scratch)
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
    (rh/toggle-scratch-buffer "*org-scratch*" #'org-mode "#+TITLE: Org Scratch\n\n")))

;; Lean playground (a overpowered scratch buffer)
(use-package emacs
  :bind ("C-c u l" . rh/open-lean-playground)
  :config
  (defun rh/open-lean-playground ()
    "Lean playground is like a scratch buffer for Lean."
    (interactive)
    (let* ((scratch-dir (expand-file-name "lib/lean-playground/" user-emacs-directory))
           (scratch-file (expand-file-name "*lean-playground*" scratch-dir))
           (template "import LeanPlayground\n\n/- Play with Lean 4 here. Start proving theorems. -/\n\n"))
      (with-current-buffer (find-file scratch-file)
        (erase-buffer)
        (insert template)
        (let ((inhibit-message t)) (save-buffer))
        (setq buffer-offer-save nil
              make-backup-files nil
              auto-save-default nil
              buffer-save-without-query t)
        (lean4-mode)
        (rh/lean4-lsp-toggle)
        (switch-to-buffer (current-buffer))))))

(provide 'knot-special-buffers)

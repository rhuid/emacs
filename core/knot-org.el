;;; knot-org.el --- Here comes the org..org..org-mode -*- lexical-binding: t; -*-

(use-package org
  :ensure nil
  :hook (org-mode . (lambda () (display-line-numbers-mode -1)))
  :custom
  (org-hide-leading-stars t)
  (org-log-done 'time)
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-special-ctrl-a/e t)
  (org-pretty-entities t)
  (org-catch-invisible-edits 'show-and-error)
  (org-insert-heading-respect-content t)
  (org-ellipsis "…")
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-agenda-tags-column 0)
  (org-table-auto-align t)
  (org-startup-folded 'content)
  (org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)))
  (org-directory "~/org")
  (org-todo-keywords
   '((sequence "TODO" "WAITING" "IN-PROGRESS" "|" "DONE" "CANCELLED")))
  :config
  (add-to-list 'org-export-backends 'md)
  ;; Set faces for the headings
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.7 :weight bold :foreground "#50fa7b"))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.5 :weight bold :foreground "#8be9fd"))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.3 :weight bold :foreground "#ff79c6"))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.1 :weight bold :foreground "#f1fa8c")))))
  :bind (:map org-mode-map
              ("C-," . nil)
              ("C-'" . nil)
              ("C-j" . rh/join-line)
              ("C-S-o" . org-shifttab)))

(use-package org-modern
  :after org
  :init (global-org-modern-mode)
  :custom (org-modern-star t))

(use-package ox-reveal
  :after org
  :init (load-library "ox-reveal")
  :config
  (setq org-re-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
  (add-to-list 'org-export-backends 'reveal))

(use-package org-agenda
  :after org
  :ensure nil
  :commands (org-agenda org-todo-list))

(provide 'knot-org)

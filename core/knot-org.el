;;; knot-org.el --- For all things org? -*- lexical-binding: t; -*-

(use-package org
  :ensure nil
  :commands (org-mode)
  :hook ((org-mode . rh/org-custom-faces)
         (org-mode . (lambda () (display-line-numbers-mode -1))))
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
  (org-directory "~/org")
  (org-todo-keywords
   '((sequence "TODO" "WAITING" "IN-PROGRESS" "|" "DONE" "CANCELLED")))
  :config
  (defun rh/org-custom-faces ()
    "Set faces of the headings."
    (custom-set-faces
     '(org-level-1 ((t (:inherit outline-1 :height 1.7 :weight bold :foreground "#50fa7b"))))
     '(org-level-2 ((t (:inherit outline-2 :height 1.5 :weight bold :foreground "#8be9fd"))))
     '(org-level-3 ((t (:inherit outline-3 :height 1.3 :weight bold :foreground "#ff79c6"))))
     '(org-level-4 ((t (:inherit outline-4 :height 1.1 :weight bold :foreground "#f1fa8c"))))))
  :bind (:map org-mode-map
              ("C-," . nil)
              ("C-'" . nil)
              ("C-S-o" . org-shifttab)))

(use-package org-modern
  :after org
  :init (global-org-modern-mode)
  :custom
  (org-modern-star t)
  (org-modern-block-fringe t))

;; To set variable-width characters instead of monospace, but preserve monospace for tables (for alignment) and code blocks
;;(set-face-attribute 'variable-pitch nil	:family "DejaVu Sans" :height 120)
;;(add-hook 'org-mode-hook
;;          (lambda ()
;;            (variable-pitch-mode 1)
;;            (face-remap-add-relative 'org-table 'fixed-pitch)
;;            (face-remap-add-relative 'org-code 'fixed-pitch)))

;; (use-package org-preview
;;   :vc (:url "https://github.com/karthink/org-preview"))

;; Automatically toggle Org mode LaTeX fragment previews as the cursor enters and exits them
;; (use-package org-fragtog
;;   :after org
;;   :hook (org-mode . org-fragtog-mode))

;; (use-package org-latex-preview :ensure nil :after org
;;   :hook (org-mode. org-latex-preview-auto-mode))

(use-package ox-reveal
  :after org
  :config
  (setq org-re-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
  (add-to-list 'org-export-backends 'reveal))

(use-package org-agenda
  :ensure nil
  :after org
  :commands (org-agenda org-todo-list))

;; (with-eval-after-load 'org
;;   ;; Define header function; to insert a template header to a new org file
;;   (defun rh/insert-org-header ()
;;     "Insert standard Org-mode header template at point."
;;     (interactive)
;;     (insert "#+TITLE: \n")
;;     (insert "#+AUTHOR: \n")
;;     (insert "#+EMAIL: \n")
;;     (insert "#+DATE: \n")
;;     (insert "#+STARTUP: overview\n")             ; Collapse headings on open
;;     (insert "#+STARTUP: indent\n\n")             ; Indent headings visually
;;     (insert "#+OPTIONS: toc:2\n")                ; Table of contents to depth 2
;;     (insert "#+LATEX_CLASS: article\n")
;;     (insert "#+LATEX_CLASS_OPTIONS: [a4paper,12pt]\n")
;;     (insert "#+LATEX_HEADER: \\usepackage[margin=1.2in]{geometry}\n")
;;     (insert "#+REVEAL_ROOT: https://cdn.jsdelivr.net/npm/reveal.js\n"))
;;   )

;; (with-eval-after-load 'org
;;   (define-key org-mode-map (kbd "C-c h") #'rh/insert-org-header))

;; The following are default keybindings
;;
;; C-c C-n    Next heading
;; C-c C-p    Previous heading
;; C-c C-f    Next heading at the same level
;; C-c C-b    Previqous heading at the same level
;; C-c C-u    Move up to the parent heading
;;
;; C-c C-l    Insert a link
;; C-c C-l *HeadingName    Insert an internal link to a heading: [[*Heading]]
;; C-c C-o    Open a link
;; C-c C-l    (on a link) Edit link
;;
;; C-c C-e    Export menu

(provide 'knot-org)

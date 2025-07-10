;;; rh-org.el --- description -*- lexical-binding: t; -*-

(require 'rh-snip)

(defvar rh/org-snippet-alist
  '(("src" . "#+BEGIN_SRC ?\n?\n#+END_SRC")
   ;; ... add more when needed
    ))

(defun rh/org-tab-hook ()
  "Setup Org snippet and placeholder support on SPACE."
  (local-set-key (kbd "C-M-i")
                 (lambda ()
                   (interactive)
                   (setq rh/snippet-placeholder-positions
                         (rh/jump-or-indent
                          rh/org-snippet-alist
                          rh/snippet-placeholder-positions)))))

;; Define header function; to insert a template header to a new org file

(defun org-header ()
  "Insert standard Org-mode header template at point."
  (interactive)
  (insert "#+TITLE: \n")
  (insert "#+AUTHOR: \n")
  (insert "#+EMAIL: ronhuidrom@gmail.com\n")
  (insert "#+DATE: \n")
  (insert "#+STARTUP: overview\n")             ; Collapse headings on open
  (insert "#+STARTUP: indent\n\n")             ; Indent headings visually
  (insert "#+OPTIONS: toc:2\n")                ; Table of contents to depth 2
  (insert "#+LATEX_CLASS: article\n")
  (insert "#+LATEX_CLASS_OPTIONS: [a4paper,12pt]\n")
  (insert "#+LATEX_HEADER: \\usepackage[margin=1.2in]{geometry}\n")
  (insert "#+REVEAL_ROOT: https://cdn.jsdelivr.net/npm/reveal.js\n"))

;; Bind C-c h in Org mode
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c h") #'org-header))

(provide 'rh-org)

;; More customizations for org

;; Define header function; to insert a template header to a new org file
(defun org-header()
  "to insert a line"
  (interactive)
  (insert "#+TITLE: \n")
  (insert "#+AUTHOR: \n")
  (insert "#+EMAIL: ronhuidrom@gmail.com")
  (insert "#+DATE: \n")
  (insert "#+STARTUP: overview\n")          ; start with all headings collapsed, not "showall"
  (insert "#+STARTUP: indent\n\n")          ; visually indent headings for readability
  (insert "#+OPTIONS: toc: 2\n")            ; table of contents, level 2
  (insert "#+LATEX_CLASS: article\n")
  (insert "#+LATEX_CLASS_OPTIONS: [a4paper, 12pt]\n")
  (insert "#+LATEX_HEADER: \\usepackage[margin=1.2in]{geometry}\n")
  (insert "#+REVEAL_ROOT: https://cdn.jsdelivr.net/npm/reveal.js\n"))   ; to use Reveal.js

;; Keybindings
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c h")
    'org-header))

(provide '15-org-custom)

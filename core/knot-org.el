;;; knot-org.el --- For all things org? -*- lexical-binding: t; -*-

(straight-use-package '(org :type built-in))

(use-package org :straight nil :defer t
  :commands (org-mode)
  :mode (("\\.org\\'" . org-mode))
  :hook ((org-mode . rh/org-init)
	 (org-mode . rh/org-custom-faces)
	 (org-mode . rh/org-tab-hook))
  :init
  (defun rh/org-init ()
    (require 'org)
    (setq display-line-numbers nil)
    (setq org-startup-indented t
	  org-hide-emphasis-markers t
	  org-ellipsis " ▾ "                                      ; folding symbol
	  org-pretty-entities t                                   ; pretty TeX symbols
	  org-log-done 'time                                      ; log time when a task is marked done
	  org-hide-leading-stars t                                ; important for org-superstar
	  org-startup-folded 'content))

  ;; Customize the face of the headings
  (defun rh/org-custom-faces ()
    (custom-set-faces
     '(org-level-1 ((t (:inherit outline-1 :height 1.8 :weight bold :foreground "#ff79c6"))))
     '(org-level-2 ((t (:inherit outline-2 :height 1.5 :weight bold :foreground "#8be9fd"))))
     '(org-level-3 ((t (:inherit outline-3 :height 1.3 :weight bold :foreground "#50fa7b"))))
     '(org-level-4 ((t (:inherit outline-4 :height 1.1 :weight bold :foreground "#f1fa8c")))))

    ;; prettify code blocks (SRC) 
    ;;(custom-set-faces                                             
    ;; '(org-block ((t (:background "#282a36" :foreground "#f8f8f2")))))
    )

  :config
  (setq org-directory "~/org")
  )

(use-package evil-org :straight t :after (evil org)
  :hook (org-mode . evil-org-mode))

(use-package org-modern :straight t :after org
  ;; visual improvements for org mode
  :hook (org-mode . org-modern-mode)
  :config

  ;; disable org-modern's heading stars (let org-superstar handle them)
  (setq org-modern-star nil))                                

;; To set variable-width characters instead of monospace, but preserve monospace for tables (for alignment) and code blocks
;;(set-face-attribute 'variable-pitch nil	:family "DejaVu Sans" :height 120)
;;(add-hook 'org-mode-hook
;;          (lambda ()
;;            (variable-pitch-mode 1)                             
;;            (face-remap-add-relative 'org-table 'fixed-pitch)
;;            (face-remap-add-relative 'org-code 'fixed-pitch)))

(use-package org-superstar :straight t :after org
  ;; prettify headings
  :hook (org-mode . org-superstar-mode)
  :config

  ;; replace * of headings with these emojis
  (setq org-superstar-headline-bullets-list                   
	'("✿" "❀" "✦" "❂"))

  ;; prettify list bullets
  (setq org-superstar-prettify-item-bullets t))

(use-package ox-reveal :straight t :after org
  ;; nice looking HTML presentations
  :config
  (setq org-re-reveal-root
	"https://cdn.jsdelivr.net/npm/reveal.js")
  (add-to-list 'org-export-backends 'reveal))

(use-package org-agenda :straight nil :after org
  :commands (org-agenda org-todo-list))

(with-eval-after-load 'org
  ;; Define header function; to insert a template header to a new org file
  (defun insert-header ()
    "Insert standard Org-mode header template at point."
    (interactive)
    (insert "#+TITLE: \n")
    (insert "#+AUTHOR: \n")
    (insert "#+EMAIL: \n")
    (insert "#+DATE: \n")
    (insert "#+STARTUP: overview\n")             ; Collapse headings on open
    (insert "#+STARTUP: indent\n\n")             ; Indent headings visually
    (insert "#+OPTIONS: toc:2\n")                ; Table of contents to depth 2
    (insert "#+LATEX_CLASS: article\n")
    (insert "#+LATEX_CLASS_OPTIONS: [a4paper,12pt]\n")
    (insert "#+LATEX_HEADER: \\usepackage[margin=1.2in]{geometry}\n")
    (insert "#+REVEAL_ROOT: https://cdn.jsdelivr.net/npm/reveal.js\n"))
  )

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c h") #'insert-header))

(with-eval-after-load 'org
  (require 'rh-snip)

  (defvar rh/org-snippet-alist
    '(("src" . "#+BEGIN_SRC ?\n?\n#+END_SRC")
      ;; ... add more when needed
      ))

  (defun rh/org-tab-hook ()
    "Setup Org snippet and placeholder support on TAB."
    (local-set-key (kbd "TAB")
                   (lambda ()
                     (interactive)
                     (setq rh/snippet-placeholder-positions
                           (rh/jump-or-indent
                            rh/org-snippet-alist
                            rh/snippet-placeholder-positions))))))

;; The following are default keybindings
;;
;; C-c C-n    Next heading
;; C-c C-p    Previous heading
;; C-c C-f    Next heading at the same level
;; C-c C-b    Previous heading at the same level
;; C-c C-u    Move up to the parent heading
;;
;; C-c C-l    Insert a link
;; C-c C-l *HeadingName    Insert an internal link to a heading: [[*Heading]]
;; C-c C-o    Open a link
;; C-c C-l    (on a link) Edit link
;;
;; C-c C-e    Export menu

;; (use-package rh-org :straight nil :after org
;;   :hook (org-mode . rh/org-tab-hook))

(provide 'knot-org)

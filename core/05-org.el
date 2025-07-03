;; Org

(use-package org
  :straight (:type built-in))

(require 'org)

(add-hook 'org-mode-hook
	  (lambda () (display-line-numbers-mode -1)))

(setq org-startup-indented t
      org-hide-emphasis-markers t                             ; hide /, *, etc when used for emphasis, bold, etc
      org-ellipsis " ▾ "                                     ; folding symbol
      org-pretty-entities t                                   ; pretty TeX symbols
      org-log-done 'time                                      ; log time when a task is marked done
      org-hide-leading-stars t                                ; important for org-superstar
      org-startup-folded 'content)                            ; show content folded by default

(use-package org-modern                                       ; visual improvements for org mode
  :ensure t
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star nil))                                 ; disable org-modern's heading stars (let org-superstar handle them)

;; To set variable-width characters instead of monospace, but preserve monospace for tables (for alignment) and code blocks
;;(set-face-attribute 'variable-pitch nil :family "DejaVu Sans" :height 120)
;;(add-hook 'org-mode-hook
;;          (lambda ()
;;            (variable-pitch-mode 1)                             
;;            (face-remap-add-relative 'org-table 'fixed-pitch)
;;            (face-remap-add-relative 'org-code 'fixed-pitch)))

(use-package org-superstar                                    ; prettify headings
  :ensure t
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list                   ; replace * of headings with these emojis
	'("✿" "❀" "✦" "❂"))
  (setq org-superstar-prettify-item-bullets t))               ; prettify list bullets

;; Customize the headings
(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.8 :weight bold :foreground "#ff79c6"))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.5 :weight bold :foreground "#8be9fd"))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.3 :weight bold :foreground "#50fa7b"))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.1 :weight bold :foreground "#f1fa8c")))))

;;(custom-set-faces                                             ; prettify code blocks (SRC) 
;; '(org-block ((t (:background "#282a36" :foreground "#f8f8f2")))))

(use-package ox-reveal                                        ; nice looking HTML presentations
  :ensure t
  :after org
  :config
  (setq org-re-reveal-root
	"https://cdn.jsdelivr.net/npm/reveal.js")
  (add-to-list 'org-export-backends 'reveal))

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

(require '15-org-custom)
(provide '05-org)

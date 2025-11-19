;; This is my custom file (anyone else can ignore this)
;; I use this for two purposes
;; One, to scrap some DIY stuff that are specific to my computer without polluting the rest of my configuration
;; Two, as a traditional custom file for Emacs to infringe upon (although I almost never use interactive customization)

(load-library "~/.emacs.d/elpa/puni-20241007.1609/puni.el")          ; for some reason, `puni' won't fully load without this
(load-library "~/.emacs.d/.archives/dictrus.el")                     ; `dictrus' --- https://github.com/rhuid/dictrus
(bind-key "C-c d d" 'rh/dictrus-lookup)

;; Because `M-r' is on home row in my keyboard
(define-key key-translation-map (kbd "M-r M-b") (kbd "C-x b"))
(define-key key-translation-map (kbd "M-r M-p") (kbd "C-x p f"))

;; Silently (without output) invoke external programs from within Emacs
(bind-key "C-s-n" (lamb (start-process-shell-command "Nemo" nil "nemo .")))  ; GUI file manager in the current directory
(bind-key "C-s-g" (lamb (start-process-shell-command "GIMP" nil "gimp")))    ; GIMP

(use-package achievements :init (achievements-mode))
(use-package wordel)

;; Here comes the mess!
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-fold-catch-invisible-edits 'show-and-error nil nil "Customized with use-package org")
 '(package-selected-packages
   '(achievements aggressive-indent all-the-icons-dired auctex-latexmk avy cape
                  cdlatex consult-dir corfu csv-mode dired-git-info diredfl eat
                  ef-themes embark-consult esh-autosuggest
                  eshell-syntax-highlighting eshell-toggle exec-path-from-shell
                  expand-region flycheck-rust gcmh haskell-mode hide-mode-line
                  highlight-numbers-mode jinx julia-mode key-chord
                  latex-preview-pane lean4-mode lsp-ui magit marginalia
                  mood-line move-text multiple-cursors mwim nix-mode orderless
                  org-modern ox-reveal puni rainbow-delimiters rainbow-mode
                  rust-mode spacious-padding sudo-edit systemd tree-sitter-langs
                  typst-ts-mode vertico visual-fill-column vundo
                  whole-line-or-region wordel yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :height 1.7 :weight bold :foreground "#50fa7b"))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.5 :weight bold :foreground "#8be9fd"))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.3 :weight bold :foreground "#ff79c6"))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.1 :weight bold :foreground "#f1fa8c")))))

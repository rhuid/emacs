;;; init.el --- The main init.el file -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(setq-default default-directory "~/")
(setq vc-follow-symlinks t)                                             ; always follow symlinks without asking

;; Set up packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") 'append)
(require 'use-package)
(setq use-package-always-ensure    t                                    ; always install packages if not found
      use-package-always-defer     t                                    ; always defer packages by default
      use-package-vc-prefer-newest t                                    ; :rev :newest by default
      load-prefer-newer t                                               ; prefer .el over .eln or .elc if it's more recent
      use-package-enable-imenu-support t)

;; Inherit shell variables (could be important for daemon)
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize)
  (setq exec-path-from-shell-variables '("PATH" "MANPATH")))

;; Garbage collector tweaks to make Emacs more responsive
(use-package gcmh
  :init (gcmh-mode)
  :custom
  (gcmh-idle-delay 20)                                                  ; run GC after 20 secs idle
  (gcmh-high-cons-threshold (* 256 1024 1024)))                         ; while typing, don't run GC until (threshold 256 MB)

;; Set up appearance
(load-theme 'modus-operandi)                                            ; now built-in with Emacs
(use-package mood-line :init (mood-line-mode))                          ; a minimalist mode-line

;; Local modules
(require 'knot-macros)
(require 'knot-keys)
(require 'knot-defaults)
(require 'knot-packages)
(require 'knot-editing)
(require 'knot-completion)
(require 'knot-writing)
(require 'knot-programming)
(require 'knot-dired)
(require 'knot-eshell)
(require 'knot-misc)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))  ; don't mess up my init, use a custom file
(load custom-file)

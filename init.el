;;; init.el --- The main init.el file -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "experimental" user-emacs-directory))

;; Inherit shell variables (could be important for daemon)
(when (or (daemonp) (display-graphic-p))
  (use-package exec-path-from-shell
    :demand t
    :config
    (setq exec-path-from-shell-variables '("PATH" "MANPATH"))
    (exec-path-from-shell-initialize)))

(setq custom-file (make-temp-file "temp_custom")) ; Don't mess up my init. Use a temporary custom file
(setq vc-follow-symlinks t) ; Always follow symlikes without asking
(setq-default default-directory "~/")

;;;; Set up packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") 'append)

;; Native compilation tweaks
(setq package-native-compile t
      package-install-upgrade-built-in t
      native-comp-deferred-compilation t
      native-comp-warning-on-missing-source nil
      native-comp-async-report-warnings-errors nil
      native-comp-speed 3
      native-comp-defer t)

;; Prefer .el over .eln or .elc if it's more recent
(setq load-prefer-newer t)

;; For native-compiling manually with make, temporarily not defer
(setq use-package-always-defer (not (bound-and-true-p byte-compile-current-file)))

(setq use-package-enable-imenu-support t)
(require 'use-package)
(setq use-package-always-ensure    t
      use-package-always-defer     t
      use-package-vc-prefer-newest t) ; :rev :newest by default

;; Tweaks for the garbage collector to make Emacs more responsive
(use-package gcmh
  :demand t
  :custom
  (gcmh-idle-delay 20) ; run GC after 20 secs idle
  (gcmh-high-cons-threshold (* 256 1024 1024)) ;; while typing, don't run GC until (threshold 256 MB)
  :config (gcmh-mode 1))

;; Local modules
(require 'knot-macros)
(require 'knot-defaults)
(require 'knot-keys)
(require 'knot-startup-page)
(require 'knot-visuals)
(require 'knot-org)
(require 'knot-programming)
(require 'knot-latex)
(require 'knot-dired)
(require 'knot-packages)
(require 'knot-eshell)
(require 'knot-email)
(require 'knot-completion)
(require 'knot-special-buffers)

;;; init.el --- The main init.el file -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "core"   user-emacs-directory))
(add-to-list 'load-path (expand-file-name "engine" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modes"  user-emacs-directory))

;; Inherit shell variables (could be important for daemon)
(when (or (daemonp) (display-graphic-p))
  (use-package exec-path-from-shell
    :demand t
    :config
    (setq exec-path-from-shell-variables '("PATH" "MANPATH"))
    (exec-path-from-shell-initialize)))

;; Don't mess up my init. Use a temporary custom file
(setq custom-file (make-temp-file "temp_custom"))
;; Always follow symlikes without asking
(setq vc-follow-symlinks t)

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

;; To track startup time for different packages (currently unused but keeping it here disabled)
(use-package benchmark-init
  :disabled t
  :ensure nil
  :init (benchmark-init/activate)
  :hook (after-init . benchmark-init/deactivate))

;; Tweaks for the garbage collector to make Emacs more responsive
(use-package gcmh
  :demand t
  :custom
  ;; Run GC after 20 secs idle
  (gcmh-idle-delay 20)
  ;; During typing/active peroid, don't run GC until (threshold 256 MB)
  (gcmh-high-cons-threshold (* 256 1024 1024))
  :config
  (gcmh-mode 1))

;;;; Local modules

(use-package knot-startup-page      :ensure nil :defer nil)
(use-package knot-visuals           :ensure nil :defer nil)
(use-package knot-defaults          :ensure nil :defer nil)
(use-package knot-org               :ensure nil :defer nil)
(use-package knot-programming       :ensure nil :defer nil)
(use-package knot-latex	            :ensure nil :defer nil)
(use-package knot-dired	            :ensure nil :defer nil)
(use-package knot-completion        :ensure nil :defer nil)
(use-package knot-extra-tools       :ensure nil :defer nil)
(use-package knot-shells            :ensure nil :defer nil)
(use-package knot-email             :ensure nil :defer nil)
(use-package knot-teleport          :ensure nil :defer nil)
(use-package knot-editor            :ensure nil :defer nil)
(use-package knot-window-management :ensure nil :defer nil)
(use-package knot-scratch           :ensure nil
  :commands (rh/toggle-org-scratch rh/toggle-lean-scratch))

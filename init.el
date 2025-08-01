;;; init.el --- The main init.el file -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "engine" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modes" user-emacs-directory))

;;;; Set up packages

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") 'append)
(setq package-install-upgrade-built-in t
      package-native-compile t
      native-comp-async-report-warnings-errors nil)

(setq use-package-enable-imenu-support t)
(require 'use-package)
(setq use-package-always-ensure    t  ; :ensure t by default
      use-package-always-defer     t  ; :defer  t by default
      use-package-vc-prefer-newest t) ; :rev :newest by default

;; For native-compiling manually with make, temporarily not defer
(setq use-package-always-defer (not (bound-and-true-p byte-compile-current-file)))

;; Inherit shell variables (could be important for daemon)
(when (or (daemonp) (display-graphic-p))
  (use-package exec-path-from-shell
    :demand t
    :config
    (setq exec-path-from-shell-variables '("PATH" "MANPATH"))
    (exec-path-from-shell-initialize)))

(use-package benchmark-init    :ensure nil :disabled t
  :init (benchmark-init/activate)
  :hook (after-init . benchmark-init/deactivate))

;;;; Modules

(use-package knot-theme-stuff  :ensure nil :defer nil)
(use-package knot-dashboard    :ensure nil :defer nil)
(use-package knot-built-ins    :ensure nil :defer nil)
(use-package knot-keys         :ensure nil :defer nil)
(use-package knot-org	       :ensure nil :defer nil)
(use-package knot-programming  :ensure nil :defer nil)
(use-package knot-latex	       :ensure nil :defer nil)
(use-package knot-dired	       :ensure nil :defer nil)
(use-package knot-completion   :ensure nil :defer nil)
(use-package knot-extra-tools  :ensure nil :defer nil)
(use-package knot-shells       :ensure nil :defer nil)
(use-package knot-scratch      :ensure nil :commands (rh/toggle-org-scratch rh/toggle-lean-scratch))

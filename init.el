;;; init.el --- The main init.el file -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "engine" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modes" user-emacs-directory))

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  ;; This must be outside the `unless` to load straight on every Emacs startup
  (load bootstrap-file nil 'nomessage))

;; ;; Setup use-package to use straight.el by default
;; (straight-use-package 'use-package)
;; ;; (setq straight-use-package-by-default t)
;; (require 'use-package)

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



;; (package-initialize)

;; ;; Optionally refresh package list if it's empty
;; (unless package-archive-contents
;;   (package-refresh-contents))

;; (unless (package-installed-p 'use-package)
;;   (package-install 'use-package))




;; For native-compiling manually with make, temporarily not defer
(setq use-package-always-defer (not (bound-and-true-p byte-compile-current-file)))

;; Make sure Emacs inherits shell's $PATH (esp. for daemon mode)
;; (use-package exec-path-from-shell :demand t
;;   :if (display-graphic-p) ;; Only needed in GUI sessions
;;   :config
;;   (setq exec-path-from-shell-variables '("PATH" "MANPATH"))
;;   (exec-path-from-shell-initialize))

;; (use-package use-package-vc
;;   :init
;;   (use-package-vc-install))

(when (or (daemonp) (display-graphic-p))
  (use-package exec-path-from-shell
    :demand t
    :config
    (setq exec-path-from-shell-variables '("PATH" "MANPATH"))
    (exec-path-from-shell-initialize)))

(use-package benchmark-init    :ensure nil :disabled t
  :init (benchmark-init/activate)
  :hook (after-init . benchmark-init/deactivate))

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

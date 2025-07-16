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

;; Setup use-package to use straight.el by default
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(require 'use-package)

;; For native-compiling manually with make, temporarily not defer
(setq use-package-always-defer (not (bound-and-true-p byte-compile-current-file)))

(setq use-package-always-defer t)

;; (setq use-package-always-ensure    t  ; :ensure t by default
;;       use-package-always-defer     t  ; :defer  t by default
;;       use-package-vc-prefer-newest t) ; :rev :newest by default

;; Make sure Emacs inherits shell's $PATH (esp. for daemon mode)
;; (use-package exec-path-from-shell :demand t
;;   :if (display-graphic-p) ;; Only needed in GUI sessions
;;   :config
;;   (setq exec-path-from-shell-variables '("PATH" "MANPATH"))
;;   (exec-path-from-shell-initialize))

(when (or (daemonp) (display-graphic-p))
  (use-package exec-path-from-shell
    :demand t
    :config
    (setq exec-path-from-shell-variables '("PATH" "MANPATH"))
    (exec-path-from-shell-initialize)))


(use-package benchmark-init      :straight t :disabled t
  :init (benchmark-init/activate)
  :hook (after-init . benchmark-init/deactivate))

(use-package ui-and-themes       :straight nil :defer nil)
(use-package welcome-home        :straight nil :defer nil)
(use-package editing-environment :straight nil :defer nil)
(use-package 30-abbrev	         :straight nil :defer nil)
(use-package knot-org	         :straight nil :defer nil)
(use-package knot-programming	 :straight nil :defer nil)
(use-package 70-typeset	         :straight nil :defer nil)
(use-package knot-latex	         :straight nil :defer nil)
(use-package 80-dired	         :straight nil :defer nil)
(use-package knot-minibuffer     :straight nil :defer nil)
(use-package 90-tools	         :straight nil :defer nil)
(use-package knot-keybindings    :straight nil :defer nil)

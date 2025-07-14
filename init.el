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

;; Make sure Emacs inherits shell's $PATH (esp. for daemon mode)
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))


(use-package benchmark-init :straight t :defer t
  :init (benchmark-init/activate)
  :hook (after-init . benchmark-init/deactivate))

(use-package 10-ui	    :straight nil :demand t)
(use-package 20-kbd	    :straight nil :demand t)
(use-package 30-abbrev	    :straight nil :demand t)
(use-package 40-dashboard   :straight nil :demand t)
(use-package 50-org	    :straight nil :demand t)
(use-package 60-src	    :straight nil :demand t)
(use-package 70-typeset	    :straight nil :demand t)
(use-package 80-dired	    :straight nil :demand t)
(use-package 90-tools	    :straight nil :demand t)

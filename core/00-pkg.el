;;; 00-pkg.el --- Package manager related stuff -*- lexical-binding: t; -*-

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

(provide '00-pkg)

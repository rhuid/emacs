;;; early-init.el --- Load this before init.el -*- lexical-binding: t; -*-

;; Defer garbage collection to speed up startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; To set the size and positon of Emacs window at startup (may be required in Emacs client mode)
(setq default-frame-alist
      '((width . 120)                                        ; in characters
        (height . 90)                                        ; in lines
	(left . 0)                                           ; position
	(top . 0)
	(font . "Iosevka Term-12")))

;; Prevent frame resizing (very expensive)
(setq frame-inhibit-implied-resize t)

;; Hard disable default startup
(setq inhibit-startup-screen t)
(fset 'display-startup-echo-area-message #'ignore)
(fset 'display-startup-screen #'ignore)

;; Disable UI elements
(menu-bar-mode   -1)
(tool-bar-mode   -1)
(scroll-bar-mode -1)
(tooltip-mode    -1)
(set-fringe-mode -1)

;; Don't resize fonts or frames implicitly at startup
(setq inhibit-default-init t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name)

;; Inhibit loading X resources (faster on X11)
(setq inhibit-x-resources t)

;; Prevent default font recalculations
(setq inhibit-compacting-font-caches t)

;; Record startup time
(setq emacs-start-time (current-time))

(setq server-client-instructions nil)

;; Don't pop up a warning buffer unless it's an emergency
(setq warning-minimum-level :emergency)

(provide 'early-init)

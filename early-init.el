;;; early-init.el --- Load this before init.el -*- lexical-binding: t; -*-

;; Disable package.el auto-init (using straight.el)
(setq package-enable-at-startup nil)

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

;; Disable UI elements before they get drawn
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 0)  ;; remove fringe padding
(setq-default cursor-type 'bar)
(blink-cursor-mode 1)

;; Don't resize fonts or frames implicitly at startup
(setq inhibit-default-init t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name)

;; (setq initial-scratch-message
;;       (with-temp-buffer
;;         (insert-file-contents "~/.emacs.d/logo/Emacs-Bloody.txt")
;;         (buffer-string)))

;; Inhibit loading X resources (faster on X11)
(setq inhibit-x-resources t)

;; Prevent default font recalculations
(setq inhibit-compacting-font-caches t)

;; Native comp tweaks (optional but safe)
(setq native-comp-async-report-warnings-errors nil
      native-comp-speed 3
      native-comp-defer t)

;; Record startup time
(setq emacs-start-time (current-time))

(setq server-client-instructions nil)

(provide 'early-init)

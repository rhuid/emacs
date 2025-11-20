;;; early-init.el --- Load this before init.el -*- lexical-binding: t; -*-

;; Defer garbage collection
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(setq default-frame-alist '((font . "Iosevka Term-15") (fullscreen . maximized)))

;; Disable UI elements
(menu-bar-mode   -1)
(tool-bar-mode   -1)
(scroll-bar-mode -1)
(tooltip-mode    -1)
(set-fringe-mode -1)

;; Inhibit startup annoyances
(setq
 inhibit-startup-screen  t
 inhibit-default-init    t
 inhibit-startup-message t
 inhibit-x-resources     t
 inhibit-compacting-font-caches t
 inhibit-startup-echo-area-message t
 frame-inhibit-implied-resize t
 server-client-instructions nil
 emacs-start-time (current-time)
 warning-minimum-level :error)

(fset 'display-startup-echo-area-message #'ignore)

;; Native compilation tweaks
(setq package-native-compile t
      package-install-upgrade-built-in t
      native-comp-deferred-compilation t
      native-comp-warning-on-missing-source nil
      native-comp-async-report-warnings-errors nil
      native-comp-speed 3
      native-comp-defer t)

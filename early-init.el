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
 inhibit-startup-screen  t                  ; inhibit the default `Welcome to GNU Emacs' buffer
 inhibit-default-init    t                  ; don't load `default.el' from the Emacs installation directory
 inhibit-startup-message t
 inhibit-x-resources     t                  ; inhibit loading X resources (faster on X11)
 inhibit-compacting-font-caches t
 inhibit-startup-echo-area-message t
 frame-inhibit-implied-resize t             ; don't automatically resize frame when changing font or UI elements
 server-client-instructions nil             ; make client sessions cleaner
 emacs-start-time (current-time)            ; record the time Emacs started, for profiling startup time later
 warning-minimum-level :error)              ; don't pop up a warning buffer unless it's an error

(fset 'display-startup-echo-area-message #'ignore)    ; suppress the minibuffer message shown at startup

;; Native compilation tweaks
(setq package-native-compile t
      package-install-upgrade-built-in t
      native-comp-deferred-compilation t
      native-comp-warning-on-missing-source nil
      native-comp-async-report-warnings-errors nil
      native-comp-speed 3
      native-comp-defer t)

(provide 'early-init)

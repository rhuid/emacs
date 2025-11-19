;;; theme-set.el --- Automatic day/night theme switching -*- lexical-binding: t; -*-

;; Author: Ronald Huidrom
;; Maintainer: Ronald Huidrom
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://github.com/rhuid/theme-set
;; Keywords: faces, convenience

;; This file is NOT part of GNU Emacs.

;;; License:
;; MIT License. See LICENSE file.

;;; Commentary:
;;
;; theme-set automatically switches between a day-theme and a night-theme
;; based on the time of day. All behavior is configurable via defcustoms.
;;
;; To enable:
;;
;;   (use-package theme-set
;;     :demand t
;;     :custom
;;     (theme-set-day-theme 'modus-operandi)
;;     (theme-set-night-theme 'modus-vivendi)
;;     (theme-set-day-start-hour 6)
;;     (theme-set-night-start-hour 18)
;;     (theme-set-check-interval-minutes 30))
;;
;; The package initializes itself during :init so it works for deferred loading.

;;; Code:

(defgroup theme-set nil
  "Automatic theme switching based on time."
  :group 'faces)

(defcustom theme-set-day-theme 'modus-operandi
  "Theme used during daytime."
  :type 'symbol
  :group 'theme-set)

(defcustom theme-set-night-theme 'modus-vivendi
  "Theme used during nighttime."
  :type 'symbol
  :group 'theme-set)

(defcustom theme-set-day-start-hour 6
  "Hour at which daytime begins (0–23)."
  :type 'integer
  :group 'theme-set)

(defcustom theme-set-night-start-hour 18
  "Hour at which nighttime begins (0–23)."
  :type 'integer
  :group 'theme-set)

(defcustom theme-set-check-interval-minutes 30
  "How often to check whether to change themes, in minutes."
  :type 'integer
  :group 'theme-set)

(defvar theme-set--current-theme nil
  "Internal variable storing the currently active theme.")

(defvar theme-set--timer nil
  "Internal timer object for periodic theme changes.")

(defun theme-set--daytime-p ()
  "Return t if current hour is between day-start and night-start."
  (let ((hour (string-to-number (format-time-string "%H"))))
    (and (>= hour theme-set-day-start-hour)
         (< hour theme-set-night-start-hour))))

(defun theme-set-update ()
  "Update the theme according to current time."
  (let ((target (if (theme-set--daytime-p)
                    theme-set-day-theme
                  theme-set-night-theme)))
    (unless (eq theme-set--current-theme target)
      (mapc #'disable-theme custom-enabled-themes)
      (load-theme target t)
      (setq theme-set--current-theme target))))

(defun theme-set--start-timer ()
  "Start or restart the periodic theme check timer."
  (when theme-set--timer
    (cancel-timer theme-set--timer))
  (setq theme-set--timer
        (run-at-time nil
                     (* 60 theme-set-check-interval-minutes)
                     #'theme-set-update)))

;;;###autoload
(defun theme-set-init ()
  "Initialize automatic theme switching."
  (interactive)
  (theme-set-update)
  (theme-set--start-timer))

;; Run it once when this file is loaded
(theme-set-init)

(provide 'theme-set)

;;; theme-set.el ends here

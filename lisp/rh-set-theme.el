;;; rh-set-theme.el --- Automatic day/night theme switching -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; rh-set-theme automatically switches between a day-theme and a night-theme
;; based on the time of day. All behavior is configurable via defcustoms.
;;
;; To enable:
;;
;;   (use-package rh-set-theme
;;     :demand t
;;     :custom
;;     (rh/set-theme-day-theme 'modus-operandi)
;;     (rh/set-theme-night-theme 'modus-vivendi)
;;     (rh/set-theme-day-start-hour 6)
;;     (rh/set-theme-night-start-hour 18)
;;     (rh/set-theme-check-interval-minutes 30))

;;; Code:

(defgroup rh/set-theme nil
  "Automatic theme switching based on time."
  :group 'faces)

(defcustom rh/set-theme-day-theme 'modus-operandi
  "Theme used during daytime."
  :type 'symbol
  :group 'rh/set-theme)

(defcustom rh/set-theme-night-theme 'modus-vivendi
  "Theme used during nighttime."
  :type 'symbol
  :group 'rh/set-theme)

(defcustom rh/set-theme-day-start-hour 6
  "Hour at which daytime begins (0–23)."
  :type 'integer
  :group 'rh/set-theme)

(defcustom rh/set-theme-night-start-hour 18
  "Hour at which nighttime begins (0–23)."
  :type 'integer
  :group 'rh/set-theme)

(defcustom rh/set-theme-check-interval-minutes 30
  "How often to check whether to change themes, in minutes."
  :type 'integer
  :group 'rh/set-theme)

(defvar rh/set-theme--current-theme nil
  "Internal variable storing the currently active theme.")

(defvar rh/set-theme--timer nil
  "Internal timer object for periodic theme changes.")

(defun rh/set-theme--daytime-p ()
  "Return t if current hour is between day-start and night-start."
  (let ((hour (string-to-number (format-time-string "%H"))))
    (and (>= hour rh/set-theme-day-start-hour)
         (< hour rh/set-theme-night-start-hour))))

(defun rh/set-theme-update ()
  "Update the theme according to current time."
  (let ((target (if (rh/set-theme--daytime-p)
                    rh/set-theme-day-theme
                  rh/set-theme-night-theme)))
    (unless (eq rh/set-theme--current-theme target)
      (mapc #'disable-theme custom-enabled-themes)
      (load-theme target t)
      (setq rh/set-theme--current-theme target))))

(defun rh/set-theme--start-timer ()
  "Start or restart the periodic theme check timer."
  (when rh/set-theme--timer
    (cancel-timer rh/set-theme--timer))
  (setq rh/set-theme--timer
        (run-at-time nil
                     (* 60 rh/set-theme-check-interval-minutes)
                     #'rh/set-theme-update)))

;;;###autoload
(defun rh/set-theme-init ()
  "Initialize automatic theme switching."
  (interactive)
  (rh/set-theme-update)
  (rh/set-theme--start-timer))

;; Run it once when this file is loaded
(rh/set-theme-init)

(provide 'rh-set-theme)

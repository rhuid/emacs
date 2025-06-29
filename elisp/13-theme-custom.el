(defun rhuid/set-theme-based-on-time ()
  "Automatically set Emacs theme based on time of day.
Catppuccin during the day, Gotham after 6 PM."
  (let* ((hour (string-to-number (format-time-string "%H")))
         (night? (or (>= hour 18) (< hour 6))))
    (mapc #'disable-theme custom-enabled-themes)
    (if night?
        (load-theme 'gotham t)
      (load-theme 'catppuccin t))))
;; Run on Emacs startup			
(add-hook 'emacs-startup-hook #'rhuid/set-theme-based-on-time)
;; Run every time a new client frame is created
(add-hook 'server-after-make-frame-hook #'rhuid/set-theme-based-on-time)

(provide '13-theme-custom)

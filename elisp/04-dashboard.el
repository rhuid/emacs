;; Dashboard 

(use-package dashboard             
  :ensure t
  :config
  (setq dashboard-startup-banner '2
        dashboard-banner-logo-title "Ronald Huidrom"
        dashboard-center-content t
        dashboard-show-time t
        dashboard-set-heading-icons t
        dashboard-heading-font-size 1
        dashboard-set-file-icons t
        dashboard-footer-messages '("One editor to rule them all"))
  (dashboard-setup-startup-hook)
  (custom-set-faces '(dashboard-startup-banner ((t(:foreground "green"))))))

(defun rhuid/open-dashboard-if-scratch ()                    ; to open dashboard, instead of scratch, when starting new client 
  "Show dashboard if the current buffer is *scratch*."
  (when (and (equal (buffer-name) "*scratch*")
             (not (buffer-file-name)))
    (dashboard-open)))
(add-hook 'emacs-startup-hook #'rhuid/open-dashboard-if-scratch) ; show dashboard after emacs starts (initial frame)
(add-hook 'server-after-make-frame-hook #'rhuid/open-dashboard-if-scratch) ; show it when a new frame is created via emacsclient

(provide '04-dashboard)

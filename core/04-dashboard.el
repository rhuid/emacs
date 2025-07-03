;; Dashboard 

(use-package dashboard             
  :config
  (setq dashboard-startup-banner '2
        dashboard-banner-logo-title "Ronald Huidrom"
        dashboard-center-content t
        dashboard-show-time t
        dashboard-set-heading-icons t
	dashboard-set-file-icons t
        dashboard-heading-font-size 1
        dashboard-footer-messages '("One editor to rule them all"))

  ;; (setq dashboard-icon-type 'all-the-icons)

  ;; (setq dashboard-display-icons-p t)     ; display icons on both GUI and terminal
  ;; (setq dashboard-icon-type 'nerd-icons) ; use `nerd-icons' package
  
  ;; Customize which widgets to display in order
  (setq dashboard-startupify-list '(dashboard-insert-newline
				    dashboard-insert-banner
                                    dashboard-insert-footer
				    dashboard-insert-newline
                                    dashboard-insert-banner-title
                                    dashboard-insert-newline
                                    dashboard-insert-navigator
                                    dashboard-insert-newline
                                    dashboard-insert-init-info
                                    dashboard-insert-items
                                    dashboard-insert-newline
				    ))
  
  ;; Show Dashboard in frames created with emacsclient -c 
  (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  
  (dashboard-setup-startup-hook))

(provide '04-dashboard)

;;; 40-dashboard.el --- description -*- lexical-binding: t; -*-

(use-package dashboard :straight t :defer t
  :hook (emacs-startup . dashboard-setup-startup-hook)
  :config
  (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  (setq rh/dashboard-banner-path (expand-file-name "logo/Emacs-Bloody.txt" user-emacs-directory))
  (setq dashboard-startup-banner rh/dashboard-banner-path
        dashboard-banner-logo-title "🏡 Welcome Home, Ronald"
        dashboard-center-content t
        dashboard-show-time t
        dashboard-set-heading-icons t
	dashboard-set-file-icons t
        dashboard-heading-font-size 1
        dashboard-footer-messages '("The one editor to rule them all"))

  ;; (setq dashboard-icon-type 'all-the-icons)
  (setq dashboard-display-icons-p t)     ; display icons on both GUI and terminal
  (setq dashboard-icon-type 'nerd-icons) ; use `nerd-icons' package
  
  ;; Customize which widgets to display in order
  (setq dashboard-startupify-list '(dashboard-insert-newline
				    dashboard-insert-newline
				    dashboard-insert-banner
				    dashboard-insert-footer
				    dashboard-insert-newline
				    dashboard-insert-banner-title
				    dashboard-insert-newline
				    dashboard-insert-navigator
				    dashboard-insert-newline
				    dashboard-insert-init-info
				    ;; dashboard-insert-items
				    dashboard-insert-newline
				    ))
  
  ;; Show Dashboard in frames created with emacsclient -c 
  ;; (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  
  ;; (dashboard-setup-startup-hook)
  )

(provide '40-dashboard)

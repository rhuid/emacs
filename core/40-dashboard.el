;;; 40-dashboard.el --- My minimai startup page -*- lexical-binding: t; -*-

(defun rh/startup-page ()
  "Display a custom startup buffer with ASCII art and a centered welcome message."
  (let ((buf (get-buffer-create "*rh-startup*"))
        (art-file (expand-file-name "logo/Emacs-Bloody.txt" user-emacs-directory)))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)

      ;; Insert ASCII art from file
      (when (file-exists-p art-file)
        (insert-file-contents art-file))

      ;; Insert newline
      (insert "\n\n")

      ;; Centered welcome message
      (let* ((message "🏡 Welcome back, Ronald")
             (padding (/ (- (window-width) (length message)) 2)))
        (insert (make-string (max padding 0) ?\s))
        (insert (propertize message 'face 'font-lock-doc-face)))

      ;; Finalize
      (goto-char (point-min))
      (read-only-mode 1)
      ;; (setq-local mode-line-format nil) ;; Optional: hide mode-line
      ))

  (switch-to-buffer "*rh-startup*")) ;; Outside `with-current-buffer`

(setq inhibit-startup-screen t)
(setq initial-buffer-choice #'rh/startup-page)

;; (use-package dashboard :straight t :defer t
;;   :hook (emacs-startup . dashboard-setup-startup-hook)
;;   :config
;;   (setq rh/dashboard-banner-path (expand-file-name "logo/Emacs-Bloody.txt" user-emacs-directory))
;;   (setq dashboard-startup-banner rh/dashboard-banner-path
;;         dashboard-banner-logo-title "🏡 Welcome Home, Ronald"
;;         dashboard-center-content t
;;         dashboard-show-time t
;;         dashboard-set-heading-icons t
;; 	dashboard-set-file-icons t
;;         dashboard-heading-font-size 1
;;         dashboard-footer-messages '("The one editor to rule them all"))

;;   ;; (setq dashboard-icon-type 'all-the-icons)
;;   ;; (setq dashboard-display-icons-p t)     ; display icons on both GUI and terminal
;;   ;; (setq dashboard-icon-type 'nerd-icons) ; use `nerd-icons' package

;;   ;; Customize which widgets to display in order
;;   (setq dashboard-startupify-list '(dashboard-insert-banner
;; 				    dashboard-insert-footer
;; 				    dashboard-insert-newline
;; 				    dashboard-insert-banner-title
;; 				    dashboard-insert-newline
;; 				    dashboard-insert-navigator
;; 				    ;; dashboard-insert-newline
;; 				    ;; dashboard-insert-init-info
;; 				    ;; dashboard-insert-items
;; 				    ))

;;   ;; Show Dashboard in frames created with emacsclient -c 
;;   (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))

;;   ;; (dashboard-setup-startup-hook)
;;   )

(provide '40-dashboard)

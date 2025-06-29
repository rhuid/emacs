;; Emacs initialization

(add-to-list 'load-path
	     (expand-file-name "elisp" user-emacs-directory))

;; Main modules
(require '01-packages)
(require '02-looks)
(require '03-theme)
(require '04-dashboard)
(require '05-org)
(require '06-coding)
(require '07-typesetting) 
(require '08-dired)
(require '09-extras)

;; More customizations
(require '13-theme-custom)
(require '15-org-custom)

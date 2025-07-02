(add-to-list 'load-path
	     (expand-file-name "elisp" user-emacs-directory))
(add-to-list 'load-path
	     (expand-file-name "engine" user-emacs-directory))

;; Main modules
(require '00-packages)
(require '01-looks)
(require '02-keybindings)
(require '03-abbrevs)
(require '04-dashboard)
(require '05-org)
(require '06-coding)
(require '07-typesetting) 
;;(require '08-dired)
(require '09-extras)

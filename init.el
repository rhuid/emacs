(dolist (dir '("core" "engine" "lang"))
  (add-to-list 'load-path
               (expand-file-name dir user-emacs-directory)))

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

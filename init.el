;;; init.el --- The main init.el file -*- lexical-binding: t; -*-

(dolist (dir '("core"
	       "engine"
	       "modes"))
  (add-to-list 'load-path
               (expand-file-name dir user-emacs-directory)))

(dolist (mod '(00-pkg
	       10-ui
	       20-kbd
	       30-abbrev
	       40-dashboard
	       50-org
	       60-src
	       70-typeset
	       80-dired
	       90-tools))
  (require mod))

;;; knot-emms.el --- a cool, minimalist music player -*- lexical-binding: t; -*-

(use-package emms :straight t :demand t
  :init
  (require 'emms-setup)
  (emms-all) ;; or (emms-standard) for a lighter setup?
  (setq emms-player-list '(emms-player-mpv))
  (setq emms-source-file-default-directory "~/Downloads/DB Scores/")
  :config
  (require 'emms-player-mpv)
  (setq emms-mode-line-format " %s"
	emms-mode-line-titlebar-format "EMMS: %s")
  (emms-mode-line-mode 1))

(provide 'knot-emms)

;;; knot-email.el --- For all things mail -*- lexical-binding: t; -*-

;;;; Using notmuch.el
;;;; First you need to have mbsync (isync), notmuch, msmtp installed and configured
;;;; You may also want gpg installed (for encrypting passwords)

(use-package notmuch
  :commands (notmuch notmuch-search notmuch-tree notmuch-show)
  :config
  (setq sendmail-program "msmtp"
        sendmail-extra-arguments '("--read-envelope-from")
        mail-specify-envelope-from t
        message-sendmail-envelope-from 'header
        message-send-mail-function 'message-send-mail-with-sendmail)
  )




(provide 'knot-email)

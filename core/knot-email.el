;;; knot-email.el --- For all things mail -*- lexical-binding: t; -*-

;;;; Using notmuch.el
;;;; First you need to have mbsync (isync), notmuch, msmtp installed and configured
;;;; You may also want gpg installed (for encrypting passwords)

(use-package notmuch
  :commands (notmuch notmuch-search notmuch-tree notmuch-show))




(provide 'knot-email)

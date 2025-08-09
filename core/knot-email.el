;;; knot-email.el --- For all things mail (using notmuch) -*- lexical-binding: t; -*-

;;;; First you need to have mbsync (isync), notmuch, msmtp installed and configured
;;;; Additionally, you may also want gpg installed (for encrypting passwords)

(use-package notmuch
  :commands (notmuch notmuch-search notmuch-tree notmuch-show)
  :bind
  (:map notmuch-hello-mode-map
        ("<f5>" . rh/mbsync-sync))
  (:map notmuch-search-mode-map
        ("<f5>" . notmuch-search-refresh-view))
  (:map notmuch-show-mode-map
        ("d r"  . notmuch-show-reply-sender)
        ("R"    . notmuch-show-reply)
        ("<f5>" . notmuch-show-refresh-view))

  :config
  (setq sendmail-program "msmtp"
        sendmail-extra-arguments '("--read-envelope-from")
        mail-specify-envelope-from t
        message-sendmail-envelope-from 'header
        message-send-mail-function 'message-send-mail-with-sendmail)

  (defun rh/mbsync-sync ()
    "Run mbsync to update mail, then refresh notmuch."
    (interactive)
    (message "Syncing mail now...")
    (let ((proc (start-process-shell-command
                 "mbsync" "*mbsync-output*"
                 "mbsync -a && notmuch new")))
      (set-process-sentinel
       proc (lambda (_ _)
              (message "Mail sync complete")
              (notmuch-refresh-this-buffer)))))

  (defun rh/notmuch-mark-for-delete ()
    "Mark message for deletion."
    (interactive)
    (notmuch-search-add-tag '("+deleted" "-inbox")))

  :custom
  (notmuch-search-oldest-first nil) ; sort from newest to oldest
  )

(provide 'knot-email)

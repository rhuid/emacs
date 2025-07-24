;;; knot-built-ins.el --- tools which came built-in with emacs -*- lexical-binding: t; -*-

;; (require 'rh-capitalize)

(setq-default abbrev-mode t)
(define-abbrev-table 'text-mode-abbrev-table '(
					       ;; Common phrases
					       ("btw"    "by the way")
					       ("afaik"  "as far as I know")
					       ("tbh"    "to be honest")
					       ("idk"    "I don’t know")
					       ("omw"    "on my way")
					       ("imo"    "in my opinion")
					       ("imho"   "in my humble opinion")
					       ("fwiw"   "for what it's worth")
					       ("asap"   "as soon as possible")
					       ("np"     "no problem")
					       ("ty"     "thank you")
					       ("brb"    "be right back")
					       ("wfh"    "working from home")
					       ("lmk"    "let me know")
					       ("yw"     "you're welcome")

					       ;; Typing shortcuts
					       ("u"      "you")
					       ("ur"     "your")
					       ("rly"    "really")
					       ("pls"    "please")
					       ("tho"    "though")
					       ("bc"     "because")
					       ("tmr"    "tomorrow")
					       ("msg"    "message")
					       ("bd"     "birthday")

					       ;; Common words
					       ("yr"     "year")
					       ("yrs"    "years")
					       
					       ;; Mathematics
					       ("wlog"   "without loss of generality")
					       ("tfae"   "the following are equivalent")
					       ))

(use-package recentf :straight nil :demand t 
  :init
  (recentf-mode 1)
  :custom
  (recentf-max-saved-items 100)
  (recentf-max-menu-items 25)
  (recentf-save-file (expand-file-name "recentf" user-emacs-directory))
  (recentf-auto-cleanup 'never)
  :config
  ;; Save recentf list every 5 minutes
  (run-at-time nil (* 5 60) #'recentf-save-list))

(use-package calc :straight nil
  :config
  (add-hook 'calc-trail-mode-hook 'evil-insert-state))

(use-package bookmark :straight nil :demand t
  :config
  (setq bookmark-save-flag 1)
  (setq bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory))
  (setq bookmark-bmenu-toggle-filenames t))

(provide 'knot-built-ins)

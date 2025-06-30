;; Abbreviations

(setq-default abbrev-mode t)                          ; enable abbrev by default in all buffers
(add-hook 'prog-mode-hook (lambda () (abbrev-mode -1)))    ; no abbrev-mode in programming mode
(global-set-key (kbd "C-c a") 'abbrev-mode)           ; toggle abbrev-mode

(define-abbrev-table 'global-abbrev-table '(
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

(provide '03-abbrevs)

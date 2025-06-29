(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")            ; MELPA
        ("gnu" . "https://elpa.gnu.org/packages/")           ; GNU ELPA
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))     ; nonGNU ELPA
(package-initialize)

(unless (package-installed-p 'use-package)                   ; install use-package if not installed already
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)                           ; no :ensure t needed

;; Ensure Emacs inherits shell's environment variables (particularly $PATH)
;; Fix the common issue where Emacs daemon starts without full shell environment
(use-package exec-path-from-shell
  :ensure t                                                  ; redundant (see above)
  :config
  (exec-path-from-shell-initialize))                         ; copy shell's $PATH and other env vars ($MANPATH, $GOPATH, etc.) into Emacs

(provide '01-packages)

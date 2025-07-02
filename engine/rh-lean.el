;; Lean customizations

(require 'rh-snip)

(defvar rh/lean4-snippet-alist
  '(("c" . "/- ? -/?")                                     ; multi-line comment
    ("ch" . "#check (?)?")
    ("ev" . "#eval (?)?")
    ("rd" . "#reduce (?)?")
    ("st" . "structure ? where \n  ?\nderiving ?")
    ("ind" . "inductive ? where \n  | ?\nderiving ?")
    ("mt" . "match ? with\n?| ? => ?")
    ("a" . "?| ? => ?")
    ("def" . "def ? : ? := ?")
    ("fn" . "fun ? => ?")
    ("ins" . "instance ?: ? where\n  ?")
    ("cls" . "class ? where\n  ?")
    ("l" . "[?]?")
    ("arr" . "#[?]?")
    ("th" . "theorem ? : ? := ?")
    ;; ... add more when needed
    ))

(defun rh/lean4-tab-hook ()
  "Setup Lean4 snippet and placeholder support on TAB."
  (local-set-key (kbd "TAB")
                 (lambda ()
                   (interactive)
                   (setq rh/snippet-placeholder-positions
                         (rh/jump-or-indent
                          rh/lean4-snippet-alist
                          rh/snippet-placeholder-positions)))))

(add-hook 'lean4-mode-hook #'rh/lean4-tab-hook)

(provide 'rh-lean)

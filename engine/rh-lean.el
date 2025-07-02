;; Lean customizations

(require 'rh-snip)
(require 'rh-custom-faces)

(defvar rh/lean4-snippet-alist
  '(("c" . "/- ? -/?")                                     ; multi-line comment
    ("ch" . "#check (?)?")
    ("ev" . "#eval (?)?")
    ("rd" . "#reduce (?)?")
    ("st" . "structure ? where \n  ?\nderiving ?")
    ("ind" . "inductive ? where \n  | ?\nderiving ?")
    ("mt" . "match ? with\n| ? => ?")
    ("a" . "| ? => ?")
    ("def" . "def ? : ? := ?")
    ("fn" . "fun ? => ?")
    ("ins" . "instance [?]: ? where\n  ?")
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

;; Hightlighting
(defun rh/lean-highlight-types ()
  "Highlight types in `lean4-mode`."
  (let ((rh/lean-types
         '("Nat"   "Int"    "Char"    "Bool"  "Unit"  "Empty"   "PUnit"  "String"
	   "List"  "Array"  "Option"  "Prod"  "Sum"   "Except"  
	   )))
    (font-lock-add-keywords
     nil
     `((,(concat "\\<" (regexp-opt rh/lean-types t) "\\>")
        . 'rh/types-face)))))

(defun rh/lean-highlight-values ()
  "Highlight values in `lean4-mode`."
  (let ((rh/lean-values
         '("none" "some" "true" "false" 
	   )))
    (font-lock-add-keywords
     nil
     `((,(concat "\\<" (regexp-opt rh/lean-values t) "\\>")
        . 'rh/values-face)))))

(defun rh/lean-highlight-typeclasses ()
  "Highlight typeclasses in `lean4-mode`."
  (let ((rh/lean-typeclasses
         '("Repr"       "ToString"
	   "Add"        "Sub"               "Mul"          "Div"        "Mod"     "Neg"     "Inv"     "Pow"
	   "HAdd"       "HSub"              "HMul"         "HDiv"       "Mod"     "Neg"     "Inv"     "Pow"
	   "HAppend"
	   "OfNat"      "Zero"              "One"          "SMul"
	   "BEq"        "DecidableEq"       "LT"           "LE"         "Ord"
	   "Coe"        "CoeDep"            "CoeTail"      "Notation"
	   "Inhabited"  "EmptyCollection"   "Enumerable"   "Hashable"

	   "Semigroup"  "Monoid"            "Group"        "Ring"       "Field"   "Module"
	   )))
    (font-lock-add-keywords
     nil
     `((,(concat "\\<" (regexp-opt rh/lean-typeclasses t) "\\>")
        . 'rh/custom-face-1)))))

(add-hook 'lean4-mode-hook #'rh/lean-highlight-types)
(add-hook 'lean4-mode-hook #'rh/lean-highlight-values)
(add-hook 'lean4-mode-hook #'rh/lean-highlight-typeclasses)

(provide 'rh-lean)

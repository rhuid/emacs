;;; rh-lean.el --- description -*- lexical-binding: t; -*-

;; Lean

(require 'rh-snip)
(require 'rh-faces)

;; (define-abbrev-table 'lean4-abbrev-table
;;   '(("ev"    "#eval"   nil 0)
;;     ("ch"    "#check"  nil 0)
;;     ("arr"   "#["      nil 0)
;;     ("df"    ":="      nil 0)
;;     ("ty"    ":"       nil 0)
;;     ("to"    "→"       nil 0)
;;     ("ftor"  "<$>"     nil 0)
;;     ("im"    "=>"      nil 0)
;;     ("str"   "String"  nil 0)

;;     ("ex"    "example" nil 0)
;;     ("ax"    "axiom"   nil 0)
;;     ("th"    "theorem" nil 0)

;;     ("prnt"  "#print"  nil 0)
;;     ("rd"    "reduce"  nil 0)
;;     ;; ... add more when needed
;;     ))

(defvar rh/lean4-snippet-alist
  '(("c"   . "/- ? -/?")
    ("ch"  . "#check (?)?")
    ("ev"  . "#eval (?)?")
    ("rd"  . "reduce (?)?")
    ("st"  . "structure ? where \n  ?\nderiving ?")
    ("ind" . "inductive ? where \n  | ?\nderiving ?")
    ("mt"  . "match ? with\n| ? => ?")
    ("a"   . "| ? => ?")
    ("def" . "def ? : ? := ?")
    ("fn"  . "fun ? => ?")
    ("ins" . "instance [?]: ? where\n  ?")
    ("cls" . "class ? where\n  ?")
    ("l"   . "[?]?")
    ("arr" . "#[?]?")

    ("name" . "namespace ?\n\n?\n\nend ?")

    ("th"   . "theorem ? : ? := ?")
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

;; Highlighting
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

(provide 'rh-lean)

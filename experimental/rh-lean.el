;;; rh-lean.el --- description -*- lexical-binding: t; -*-

;; (lean4-mode . (lambda () (require 'rh-lean)))
;; (lean4-mode . rh/outline-lean)

(defun rh/outline-lean ()
  "Set outline regex for top-level declarations in Lean."
  (setq-local outline-regexp
              (rx line-start (* space)
                  (or  "structure" "inductive" "class" "theorem"
                       "axiom" "lemma" "def" "instance" "example"
                       "opaque" "namespace")))
  (outline-hide-body))

(require 'rh-snip)
(require 'rh-faces)

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

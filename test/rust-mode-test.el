;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)
(require 'rust-mode)


(defun helper-setup-rust-function ()
  "Set up a Rust function context."
  (insert "fn foo() -> i32 {\n\n}\n")
  (goto-char (point-min))
  (forward-line 1))


(ert-deftest rust-comments ()
  (th-fixtures #'rust-mode
    (th-type "//a comment")
    (th-should-see "// a comment")))

(ert-deftest rust-doc-comments ()
  :expected-result :failed
  (th-fixtures #'rust-mode
    (th-type "///a comment")
    (th-should-see "/// a comment")))

(ert-deftest rust-division-slash-still-works ()
  (th-fixtures #'rust-mode
    (th-type "int a = x/y")
    (th-should-see "int a = x / y")))

(ert-deftest rust-double-slash-adds-space-before-if-not-on-empty-line ()
  (th-fixtures #'rust-mode
    (th-type "expression;//")
    (th-should-see "expression; //")))

(ert-deftest rust-double-slash-does-not-add-space-before-when-at-indentation-of-line ()
  (th-fixtures #'rust-mode
    (th-type "   //")
    (th-should-see-pattern "^   // $")))

(ert-deftest rust-slash-star-adds-space-before-if-not-on-empty-line ()
  (th-fixtures #'rust-mode
    (th-type "expression;/*")
    (th-should-see "expression; /*")))

(ert-deftest rust-slash-star-does-not-add-space-before-when-at-indentation-of-line ()
  (th-fixtures #'rust-mode
    (th-type "   /*")
    (th-should-see-pattern "^   /\\* $")))

(ert-deftest rust-function-return-value ()
  (th-fixtures #'rust-mode
    (th-type "fn foo()->i32")
    (th-should-see "fn foo() -> i32")))

(ert-deftest rust-pattern-matching-fat-arrow ()
  (th-fixtures #'rust-mode
    (helper-setup-rust-function)
    (insert "match x {\n")
    (th-type "1=>pri")
    (th-should-see "1 => pri")))


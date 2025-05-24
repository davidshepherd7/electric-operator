;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)
(require 'rust-mode)

(ert-deftest rust-comments ()
  (test-with-mode rust-mode
    (electric-operator-test-type "//a comment")
    (electric-operator-test-should-see "// a comment")))

(ert-deftest rust-doc-comments ()
  :expected-result :failed
  (test-with-mode rust-mode
    (electric-operator-test-type "///a comment")
    (electric-operator-test-should-see "/// a comment")))

(ert-deftest rust-division-slash-still-works ()
  (test-with-mode rust-mode
    (electric-operator-test-type "int a = x/y")
    (electric-operator-test-should-see "int a = x / y")))

(ert-deftest rust-double-slash-adds-space-before-if-not-on-empty-line ()
  (test-with-mode rust-mode
    (electric-operator-test-type "expression;//")
    (electric-operator-test-should-see "expression; //")))

(ert-deftest rust-double-slash-does-not-add-space-before-when-at-indentation-of-line ()
  (test-with-mode rust-mode
    (electric-operator-test-type "   //")
    (electric-operator-test-should-see-pattern "^   // $")))

(ert-deftest rust-slash-star-adds-space-before-if-not-on-empty-line ()
  (test-with-mode rust-mode
    (electric-operator-test-type "expression;/*")
    (electric-operator-test-should-see "expression; /*")))

(ert-deftest rust-slash-star-does-not-add-space-before-when-at-indentation-of-line ()
  (test-with-mode rust-mode
    (electric-operator-test-type "   /*")
    (electric-operator-test-should-see-pattern "^   /\\* $")))

(ert-deftest rust-function-return-value ()
  (test-with-mode rust-mode
    (electric-operator-test-type "fn foo()->i32")
    (electric-operator-test-should-see "fn foo() -> i32")))

(ert-deftest rust-pattern-matching-fat-arrow ()
  (test-with-mode rust-mode
    (electric-operator-test-setup-rust-function)
    (insert "match x {\n")
    (electric-operator-test-type "1=>pri")
    (electric-operator-test-should-see "1 => pri")))


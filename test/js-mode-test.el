;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)

(ert-deftest js-colon-inside-objects ()
  (test-with-mode js-mode
    (electric-operator-test-type "{a:1}")
    (electric-operator-test-should-see "{a: 1}")))

(ert-deftest js-ternary-operator ()
  (test-with-mode js-mode
    (electric-operator-test-type "bool?1:2")
    (electric-operator-test-should-see "bool ? 1 : 2")))

(ert-deftest js-division ()
  (test-with-mode js-mode
    (electric-operator-test-type "x=b/a.foo")
    (electric-operator-test-should-see "x = b / a.foo")))

(ert-deftest js-regex-literals-simple ()
  (test-with-mode js-mode
    (electric-operator-test-type "/a.foo/")
    (electric-operator-test-should-see "/a.foo/")))

(ert-deftest js-regex-literals-with-spacing-before ()
  (test-with-mode js-mode
    (electric-operator-test-type "x=/a.foo/")
    (electric-operator-test-should-see "x = /a.foo/")))

(ert-deftest js-correctly-space-shebang-paths ()
  (test-with-mode js-mode
    (electric-operator-test-type "#! /usr/bin/node")
    (electric-operator-test-should-see "#! /usr/bin/node")))

;; Had some problems with this because probably-unary-operator was wrong in
;; some cases, so a " / " was inserted by the first /, combined with the fact
;; that regexes are strings this goes wrong.
(ert-deftest js-double-slash-comments ()
  (test-with-mode js-mode
    (electric-operator-test-type "//a comment")
    (electric-operator-test-should-see "// a comment")))

(ert-deftest js-double-slash-comments-in-unary-operator-context ()
  (test-with-mode js-mode
    (electric-operator-test-type "function() {//a comment")
    (electric-operator-test-should-see "// a comment")))

(ert-deftest js-double-slash-comments-inside-iifes ()
  (test-with-mode js-mode
    (electric-operator-test-type "function() {\n//a comment")
    (electric-operator-test-should-see "// a comment")))

(ert-deftest js-slash-star-comments ()
  (test-with-mode js-mode
    (electric-operator-test-type "/*a comment */")
    (electric-operator-test-should-see "/* a comment */")))

;; Known failure test (commented out)
;; (ert-deftest js-comment-after-division ()
;;   (test-with-mode js-mode
;;     (electric-operator-test-type "x=a/ // divide x")
;;     (electric-operator-test-should-see "x = a / // divide x")))

(ert-deftest js-division-slash-still-works ()
  (test-with-mode js-mode
    (electric-operator-test-type "int a = x/y")
    (electric-operator-test-should-see "int a = x / y")))

(ert-deftest js-double-slash-adds-space-before-if-not-on-empty-line ()
  (test-with-mode js-mode
    (electric-operator-test-type "expression;//")
    (electric-operator-test-should-see "expression; //")))

(ert-deftest js-double-slash-does-not-add-space-before-when-at-indentation-of-line ()
  (test-with-mode js-mode
    (electric-operator-test-type "   //")
    (electric-operator-test-should-see-pattern "^   // $")))

(ert-deftest js-slash-star-adds-space-before-if-not-on-empty-line ()
  (test-with-mode js-mode
    (electric-operator-test-type "expression;/*")
    (electric-operator-test-should-see "expression; /*")))

(ert-deftest js-slash-star-does-not-add-space-before-when-at-indentation-of-line ()
  (test-with-mode js-mode
    (electric-operator-test-type "   /*")
    (electric-operator-test-should-see-pattern "^   /\\* $")))

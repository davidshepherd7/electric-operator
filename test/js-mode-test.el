;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)

(ert-deftest js-colon-inside-objects ()
  (th-fixtures js-mode
    (th-type "{a:1}")
    (th-should-see "{a: 1}")))

(ert-deftest js-ternary-operator ()
  (th-fixtures js-mode
    (th-type "bool?1:2")
    (th-should-see "bool ? 1 : 2")))

(ert-deftest js-division ()
  (th-fixtures js-mode
    (th-type "x=b/a.foo")
    (th-should-see "x = b / a.foo")))

(ert-deftest js-regex-literals-simple ()
  (th-fixtures js-mode
    (th-type "/a.foo/")
    (th-should-see "/a.foo/")))

(ert-deftest js-regex-literals-with-spacing-before ()
  (th-fixtures js-mode
    (th-type "x=/a.foo/")
    (th-should-see "x = /a.foo/")))

(ert-deftest js-correctly-space-shebang-paths ()
  (th-fixtures js-mode
    (th-type "#! /usr/bin/node")
    (th-should-see "#! /usr/bin/node")))

;; Had some problems with this because probably-unary-operator was wrong in
;; some cases, so a " / " was inserted by the first /, combined with the fact
;; that regexes are strings this goes wrong.
(ert-deftest js-double-slash-comments ()
  (th-fixtures js-mode
    (th-type "//a comment")
    (th-should-see "// a comment")))

(ert-deftest js-double-slash-comments-in-unary-operator-context ()
  (th-fixtures js-mode
    (th-type "function() {//a comment")
    (th-should-see "// a comment")))

(ert-deftest js-double-slash-comments-inside-iifes ()
  (th-fixtures js-mode
    (th-type "function() {\n//a comment")
    (th-should-see "// a comment")))

(ert-deftest js-slash-star-comments ()
  (th-fixtures js-mode
    (th-type "/*a comment */")
    (th-should-see "/* a comment */")))

(ert-deftest js-comment-after-division ()
  :expected-result :failed
  (th-fixtures js-mode
               (th-type "x=a/ // divide x")
               (th-should-see "x = a / // divide x")))

(ert-deftest js-division-slash-still-works ()
  (th-fixtures js-mode
               (th-type "int a = x/y")
               (th-should-see "int a = x / y")))

(ert-deftest js-double-slash-adds-space-before-if-not-on-empty-line ()
  (th-fixtures js-mode
               (th-type "expression;//")
               (th-should-see "expression; //")))

(ert-deftest js-double-slash-does-not-add-space-before-when-at-indentation-of-line ()
  (th-fixtures js-mode
               (th-type "   //")
               (th-should-see-pattern "^   // $")))

(ert-deftest js-slash-star-adds-space-before-if-not-on-empty-line ()
  (th-fixtures js-mode
               (th-type "expression;/*")
               (th-should-see "expression; /*")))

(ert-deftest js-slash-star-does-not-add-space-before-when-at-indentation-of-line ()
  (th-fixtures js-mode
               (th-type "   /*")
               (th-should-see-pattern "^   /\\* $")))

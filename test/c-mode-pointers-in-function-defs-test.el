;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)

(ert-deftest c-mode-handle-pointer-type-in-function-decl ()
  (test-with-mode c-mode
    (electric-operator-test-type "void foo(my_struct*s")
    (electric-operator-test-should-see "void foo(my_struct *s")))

(ert-deftest c-mode-handle-pointer-to-pointer-type-in-function-decl ()
  (test-with-mode c-mode
    (electric-operator-test-type "void foo(my_struct**s")
    (electric-operator-test-should-see "void foo(my_struct **s")))

(ert-deftest c-mode-handle-reference-type-in-function-decl ()
  (test-with-mode c-mode
    (electric-operator-test-type "void foo(my_struct&s")
    (electric-operator-test-should-see "void foo(my_struct &s")))

(ert-deftest c-mode-handle-pointer-types-with-bracket-on-new-lines ()
  (test-with-mode c-mode
    (insert "void foo\n(\n")
    (electric-operator-test-type "my_struct*s")
    (electric-operator-test-should-see "void foo\n(\nmy_struct *s")))

(ert-deftest c-mode-handle-pointer-types-with-argument-on-new-lines ()
  (test-with-mode c-mode
    (insert "void foo(\n\n")
    (electric-operator-test-type "my_struct*s")
    (electric-operator-test-should-see "void foo(\n\nmy_struct *s")))

(ert-deftest c-mode-handle-pointer-types-with-arguments-continued-on-new-line ()
  (test-with-mode c-mode
    (insert "void foo(int a,\n\n")
    (electric-operator-test-type "my_struct*s")
    (electric-operator-test-should-see "void foo(int a,\n\nmy_struct *s")))

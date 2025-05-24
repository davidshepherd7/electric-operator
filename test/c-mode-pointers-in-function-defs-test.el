;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)

(ert-deftest c-mode-handle-pointer-type-in-function-decl ()
  (th-fixtures #'c-mode
    (th-type "void foo(my_struct*s")
    (th-should-see "void foo(my_struct *s")))

(ert-deftest c-mode-handle-pointer-to-pointer-type-in-function-decl ()
  (th-fixtures #'c-mode
    (th-type "void foo(my_struct**s")
    (th-should-see "void foo(my_struct **s")))

(ert-deftest c-mode-handle-reference-type-in-function-decl ()
  (th-fixtures #'c-mode
    (th-type "void foo(my_struct&s")
    (th-should-see "void foo(my_struct &s")))

(ert-deftest c-mode-handle-pointer-types-with-bracket-on-new-lines ()
  (th-fixtures #'c-mode
    (insert "void foo\n(\n")
    (th-type "my_struct*s")
    (th-should-see "void foo\n(\nmy_struct *s")))

(ert-deftest c-mode-handle-pointer-types-with-argument-on-new-lines ()
  (th-fixtures #'c-mode
    (insert "void foo(\n\n")
    (th-type "my_struct*s")
    (th-should-see "void foo(\n\nmy_struct *s")))

(ert-deftest c-mode-handle-pointer-types-with-arguments-continued-on-new-line ()
  (th-fixtures #'c-mode
    (insert "void foo(int a,\n\n")
    (th-type "my_struct*s")
    (th-should-see "void foo(int a,\n\nmy_struct *s")))

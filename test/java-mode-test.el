;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)

(ert-deftest java-mode-division-/-still-works ()
  (th-fixtures #'java-mode
    (th-type "int a = x/y")
    (th-should-see "int a = x / y")))

(ert-deftest java-mode-//-adds-space-before-if-not-on-empty-line ()
  (th-fixtures #'java-mode
    (th-type "expression;//")
    (th-should-see "expression; //")))

(ert-deftest java-mode-//-does-not-add-space-before-when-at-indentation-of-line ()
  (th-fixtures #'java-mode
    (let ((c-electric-flag nil))
      (th-type "   //")
      (th-should-see-pattern "^   // $"))))

(ert-deftest java-mode-/*-adds-space-before-if-not-on-empty-line ()
  (th-fixtures #'java-mode
    (th-type "expression;/*")
    (th-should-see "expression; /*")))

(ert-deftest java-mode-/*-does-not-add-space-before-when-at-indentation-of-line ()
  (th-fixtures #'java-mode
    (let ((c-electric-flag nil))
      (th-type "   /*")
      (th-should-see-pattern "^   /\\* $"))))

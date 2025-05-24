;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)
(require 'php-mode)

(ert-deftest php-mode-division-/-still-works ()
  (th-fixtures php-mode
    (th-type "int a = x/y")
    (th-should-see "int a = x / y")))

(ert-deftest php-mode-//-adds-space-before-if-not-on-empty-line ()
  (th-fixtures php-mode
    (th-type "expression;//")
    (th-should-see "expression; //")))

(ert-deftest php-mode-//-does-not-add-space-before-when-at-indentation-of-line ()
  (th-fixtures php-mode
    (let ((c-electric-flag nil))
      (th-type "   //")
      (th-should-see-pattern "^   // $"))))

(ert-deftest php-mode-/*-adds-space-before-if-not-on-empty-line ()
  (th-fixtures php-mode
    (th-type "expression;/*")
    (th-should-see "expression; /*")))

(ert-deftest php-mode-/*-does-not-add-space-before-when-at-indentation-of-line ()
  (th-fixtures php-mode
    (let ((c-electric-flag nil))
      (th-type "   /*")
      (th-should-see-pattern "^   /\\* $"))))

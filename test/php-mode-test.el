;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)
(require 'php-mode)

(ert-deftest php-mode-division-/-still-works ()
  (test-with-mode php-mode
    (electric-operator-test-type "int a = x/y")
    (electric-operator-test-should-see "int a = x / y")))

(ert-deftest php-mode-//-adds-space-before-if-not-on-empty-line ()
  (test-with-mode php-mode
    (electric-operator-test-type "expression;//")
    (electric-operator-test-should-see "expression; //")))

(ert-deftest php-mode-//-does-not-add-space-before-when-at-indentation-of-line ()
  (test-with-mode php-mode
    (let ((c-electric-flag nil))
      (electric-operator-test-type "   //")
      (electric-operator-test-should-see-pattern "^   // $"))))

(ert-deftest php-mode-/*-adds-space-before-if-not-on-empty-line ()
  (test-with-mode php-mode
    (electric-operator-test-type "expression;/*")
    (electric-operator-test-should-see "expression; /*")))

(ert-deftest php-mode-/*-does-not-add-space-before-when-at-indentation-of-line ()
  (test-with-mode php-mode
    (let ((c-electric-flag nil))
      (electric-operator-test-type "   /*")
      (electric-operator-test-should-see-pattern "^   /\\* $"))))
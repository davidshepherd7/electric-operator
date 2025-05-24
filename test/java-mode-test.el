;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)

(ert-deftest java-mode-division-/-still-works ()
  (test-with-mode java-mode
    (electric-operator-test-type "int a = x/y")
    (electric-operator-test-should-see "int a = x / y")))

(ert-deftest java-mode-//-adds-space-before-if-not-on-empty-line ()
  (test-with-mode java-mode
    (electric-operator-test-type "expression;//")
    (electric-operator-test-should-see "expression; //")))

(ert-deftest java-mode-//-does-not-add-space-before-when-at-indentation-of-line ()
  (test-with-mode java-mode
    (let ((c-electric-flag nil))
      (electric-operator-test-type "   //")
      (electric-operator-test-should-see-pattern "^   // $"))))

(ert-deftest java-mode-/*-adds-space-before-if-not-on-empty-line ()
  (test-with-mode java-mode
    (electric-operator-test-type "expression;/*")
    (electric-operator-test-should-see "expression; /*")))

(ert-deftest java-mode-/*-does-not-add-space-before-when-at-indentation-of-line ()
  (test-with-mode java-mode
    (let ((c-electric-flag nil))
      (electric-operator-test-type "   /*")
      (electric-operator-test-should-see-pattern "^   /\\* $"))))
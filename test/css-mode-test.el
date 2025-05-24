;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)

(ert-deftest css-mode-colon-for-pseudo-classes ()
  (test-with-mode css-mode
    (electric-operator-test-type "button:hover {")
    (electric-operator-test-should-see "button:hover {")))

(ert-deftest css-mode-colon-for-properties ()
  (test-with-mode css-mode
    (insert "button.foo {\n\n}")
    (goto-line 2)
    (electric-operator-test-type "text-decoration:underline;")
    (electric-operator-test-should-see "button.foo {\ntext-decoration: underline;\n}")))
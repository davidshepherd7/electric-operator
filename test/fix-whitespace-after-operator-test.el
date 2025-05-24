;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)

(ert-deftest fix-whitespace-adding-a-comma-in-the-middle-of-a-list ()
  (test-with-mode prog-mode
    (electric-operator-test-type "foo(a, b c)")
    (goto-char (- (point) 3))  ; position between "b" and " c"
    (electric-operator-test-type ",")
    (electric-operator-test-should-see "foo(a, b, c)")))

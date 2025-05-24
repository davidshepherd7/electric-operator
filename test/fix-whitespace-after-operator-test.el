;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)

(ert-deftest fix-whitespace-adding-a-comma-in-the-middle-of-a-list ()
  (th-fixtures #'prog-mode
    (th-type "foo(a, b c)")
    (goto-char (- (point) 3))  ; position between "b" and " c"
    (th-type ",")
    (th-should-see "foo(a, b, c)")))

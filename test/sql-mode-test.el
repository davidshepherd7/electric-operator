;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'electric-operator)
(require 'test-helper)

(ert-deftest sql-minus ()
  (th-fixtures #'sql-mode
    (th-type "a-b")
    (th-should-see "a - b")))

(ert-deftest sql-comment ()
  (th-fixtures #'sql-mode
    (th-type "--a comment")
    (th-should-see "-- a comment")))

(ert-deftest sql-operators-not-expanded-inside-comments ()
  (th-fixtures #'sql-mode
    (th-type "--a=b*c")
    (th-should-see "-- a=b*c")))

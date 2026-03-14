;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'electric-operator)
(require 'test-helper)

(ert-deftest sql-operators-not-expanded-inside-comments ()
  (th-fixtures #'sql-mode
    (th-type "--a=b")
    (th-should-see "--a=b")))

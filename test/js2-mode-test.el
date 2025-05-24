;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'electric-operator)
(require 'js2-mode)
(require 'test-helper)

(ert-deftest js2-mode-it-gets-the-javascript-rules ()
  (th-fixtures #'js2-mode
    (th-type "{a:1}")
    (th-should-see "{a: 1}")))

(ert-deftest js2-mode-regex-literals-simple ()
  (th-fixtures #'js2-mode
    (th-type "/a.foo")
    (js2-reparse)
    (th-type "/")
    (th-should-see "/a.foo/")))

(ert-deftest js2-mode-regex-literals-without-time-for-a-reparse ()
  :expected-result :failed
  (th-fixtures #'js2-mode
    (th-type "/a.foo/")
    (th-should-see "/a.foo/")))

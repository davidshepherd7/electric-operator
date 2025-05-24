;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)
(require 'js2-mode)

(ert-deftest js2-mode-it-gets-the-javascript-rules ()
  (test-with-mode js2-mode
    (electric-operator-test-type "{a:1}")
    (electric-operator-test-should-see "{a: 1}")))

(ert-deftest js2-mode-regex-literals-simple ()
  (test-with-mode js2-mode
    (electric-operator-test-type "/a.foo")
    (js2-reparse)
    (electric-operator-test-type "/")
    (electric-operator-test-should-see "/a.foo/")))

(ert-deftest js2-mode-regex-literals-without-time-for-a-reparse ()
  :expected-result :failed
  (test-with-mode js2-mode
    (electric-operator-test-type "/a.foo/")
    (electric-operator-test-should-see "/a.foo/")))

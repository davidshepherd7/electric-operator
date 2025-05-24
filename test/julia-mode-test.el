;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)
(require 'julia-mode)

;; Keyword argument =
(ert-deftest julia-mode-space-standard-assignment-as-normal ()
  (test-with-mode julia-mode
    (electric-operator-test-type "a=b")
    (electric-operator-test-should-see "a = b")))

(ert-deftest julia-mode-dont-space-assignment-inside-function-call ()
  (test-with-mode julia-mode
    (electric-operator-test-type "f(a=b)")
    (electric-operator-test-should-see "f(a=b)")))

(ert-deftest julia-mode-declare-functions-with-keyword-arguments ()
  (test-with-mode julia-mode
    (electric-operator-test-type "function f(x;y=0,kwargs...)")
    (electric-operator-test-should-see "function f(x; y=0, kwargs...)")))
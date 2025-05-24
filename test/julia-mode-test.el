;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)
(require 'julia-mode)

;; Keyword argument =
(ert-deftest julia-mode-space-standard-assignment-as-normal ()
  (th-fixtures julia-mode
    (th-type "a=b")
    (th-should-see "a = b")))

(ert-deftest julia-mode-dont-space-assignment-inside-function-call ()
  (th-fixtures julia-mode
    (th-type "f(a=b)")
    (th-should-see "f(a=b)")))

(ert-deftest julia-mode-declare-functions-with-keyword-arguments ()
  (th-fixtures julia-mode
    (th-type "function f(x;y=0,kwargs...)")
    (th-should-see "function f(x; y=0, kwargs...)")))

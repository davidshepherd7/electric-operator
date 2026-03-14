;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'electric-operator)
(require 'f90)
(require 'test-helper)

(ert-deftest f90-mode-dont-modify-string-literal ()
  (th-fixtures #'f90-mode
    (th-type "'var+foo-1'")
    (th-should-see "'var+foo-1'")))

(ert-deftest f90-mode-dont-modify-string-literal-after-operator ()
  (th-fixtures #'f90-mode
    (th-type "a+''")
    (th-should-see "a + ''")))

;; * and ** operators
(ert-deftest f90-mode-space-* ()
  (th-fixtures #'f90-mode
    (th-type "a*b")
    (th-should-see "a * b")))

(ert-deftest f90-mode-multiplication-after-a-function ()
  (th-fixtures #'f90-mode
    (th-type "f(x)*a")
    (th-should-see "f(x) * a")))

(ert-deftest f90-mode-exponentiation-after-a-function ()
  (th-fixtures #'f90-mode
    (th-type "f(x)**a")
    (th-should-see "f(x) ** a")))

(ert-deftest f90-mode-write-statements-with-default-formatting-and-default-output ()
  (th-fixtures #'f90-mode
    (th-type "write(*,*)")
    (th-should-see "write(*, *)")))

(ert-deftest f90-mode-write-statements-with-custom-formatting-and-default-output ()
  (th-fixtures #'f90-mode
    (th-type "write(*, '(format)')")
    (th-should-see "write(*, '(format)')")))

(ert-deftest f90-mode-write-statements-with-default-formatting-and-file-output ()
  (th-fixtures #'f90-mode
    (th-type "write(101,*)")
    (th-should-see "write(101, *)")))

(ert-deftest f90-mode-print-statements-with-default-formatting ()
  (th-fixtures #'f90-mode
    (th-type "print*,")
    (th-should-see "print *, ")))

;; Variable declaration
(ert-deftest f90-mode-declaring-a-variable ()
  (th-fixtures #'f90-mode
    (th-type "type::A")
    (th-should-see "type :: A")))

;; Custom operators with .operator. syntax
(ert-deftest f90-mode-using-a-generic-operator ()
  :expected-result :failed
  (th-fixtures #'f90-mode
    (th-type "a.operator_123.b")
    (th-should-see "a .operator_123. b")))

(ert-deftest f90-mode-number-preceeding-an-operator ()
  (th-fixtures #'f90-mode
    (th-type "1.eq.a")
    (th-should-see "1 .eq. a")))

;; Keyword argument =
(ert-deftest f90-mode-space-standard-assignment-as-normal ()
  (th-fixtures #'f90-mode
    (th-type "a=b")
    (th-should-see "a = b")))

(ert-deftest f90-mode-dont-space-assignment-inside-function-call ()
  (th-fixtures #'f90-mode
    (th-type "f(a=b)")
    (th-should-see "f(a=b)")))

;; Implicit array declaration
(ert-deftest f90-mode-dividing-numbers-as-usual ()
  (th-fixtures #'f90-mode
    (th-type "1/2")
    (th-should-see "1 / 2")))

(ert-deftest f90-mode-single-line-array-declaration ()
  (th-fixtures #'f90-mode
    (th-type "(/1,2,3/)")
    (th-should-see "(/ 1, 2, 3 /)")))

(ert-deftest f90-mode-single-line-array-declaration-with-fraction ()
  :expected-result :failed
  (th-fixtures #'f90-mode
    (th-type "(/1,2/5,3/)")
    (th-should-see "(/ 1, 2 / 5, 3 /)")))

(ert-deftest f90-mode-multi-line-array-declaration ()
  (th-fixtures #'f90-mode
    (insert "(/1, 2, 3, &\n\n")
    (th-type "4,5,6/)")
    (th-should-see "(/1, 2, 3, &\n\n4, 5, 6 /)")))

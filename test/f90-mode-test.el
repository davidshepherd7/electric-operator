;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)

(ert-deftest f90-mode-dont-modify-string-literal ()
  (test-with-mode f90-mode
    (let ((electric-operator-enable-in-docs nil))
      (electric-operator-test-type "'var+foo-1'")
      (electric-operator-test-should-see "'var+foo-1'"))))

(ert-deftest f90-mode-dont-modify-string-literal-after-operator ()
  (test-with-mode f90-mode
    (electric-operator-test-type "a+''")
    (electric-operator-test-should-see "a + ''")))

;; * and ** operators
(ert-deftest f90-mode-space-* ()
  (test-with-mode f90-mode
    (electric-operator-test-type "a*b")
    (electric-operator-test-should-see "a * b")))

(ert-deftest f90-mode-multiplication-after-a-function ()
  (test-with-mode f90-mode
    (electric-operator-test-type "f(x)*a")
    (electric-operator-test-should-see "f(x) * a")))

(ert-deftest f90-mode-exponentiation-after-a-function ()
  (test-with-mode f90-mode
    (electric-operator-test-type "f(x)**a")
    (electric-operator-test-should-see "f(x) ** a")))

(ert-deftest f90-mode-write-statements-with-default-formatting-and-default-output ()
  (test-with-mode f90-mode
    (electric-operator-test-type "write(*,*)")
    (electric-operator-test-should-see "write(*, *)")))

(ert-deftest f90-mode-write-statements-with-custom-formatting-and-default-output ()
  (test-with-mode f90-mode
    (electric-operator-test-type "write(*, '(format)')")
    (electric-operator-test-should-see "write(*, '(format)')")))

(ert-deftest f90-mode-write-statements-with-default-formatting-and-file-output ()
  (test-with-mode f90-mode
    (electric-operator-test-type "write(101,*)")
    (electric-operator-test-should-see "write(101, *)")))

(ert-deftest f90-mode-print-statements-with-default-formatting ()
  (test-with-mode f90-mode
    (electric-operator-test-type "print*,")
    (electric-operator-test-should-see "print *, ")))

;; Variable declaration
(ert-deftest f90-mode-declaring-a-variable ()
  (test-with-mode f90-mode
    (electric-operator-test-type "type::A")
    (electric-operator-test-should-see "type :: A")))

;; Custom operators with .operator. syntax
;; Known failure test commented out
;; (ert-deftest f90-mode-using-a-generic-operator ()
;;   (test-with-mode f90-mode
;;     (electric-operator-test-type "a.operator_123.b")
;;     (electric-operator-test-should-see "a .operator_123. b")))

(ert-deftest f90-mode-number-preceeding-an-operator ()
  (test-with-mode f90-mode
    (electric-operator-test-type "1.eq.a")
    (electric-operator-test-should-see "1 .eq. a")))

;; Keyword argument =
(ert-deftest f90-mode-space-standard-assignment-as-normal ()
  (test-with-mode f90-mode
    (electric-operator-test-type "a=b")
    (electric-operator-test-should-see "a = b")))

(ert-deftest f90-mode-dont-space-assignment-inside-function-call ()
  (test-with-mode f90-mode
    (electric-operator-test-type "f(a=b)")
    (electric-operator-test-should-see "f(a=b)")))

;; Implicit array declaration
(ert-deftest f90-mode-dividing-numbers-as-usual ()
  (test-with-mode f90-mode
    (electric-operator-test-type "1/2")
    (electric-operator-test-should-see "1 / 2")))

(ert-deftest f90-mode-single-line-array-declaration ()
  (test-with-mode f90-mode
    (electric-operator-test-type "(/1,2,3/)")
    (electric-operator-test-should-see "(/ 1, 2, 3 /)")))

;; Known failure test commented out
;; (ert-deftest f90-mode-single-line-array-declaration-with-fraction ()
;;   (test-with-mode f90-mode
;;     (electric-operator-test-type "(/1,2/5,3/)")
;;     (electric-operator-test-should-see "(/ 1, 2 / 5, 3 /)")))

(ert-deftest f90-mode-multi-line-array-declaration ()
  (test-with-mode f90-mode
    (insert "(/1, 2, 3, &\n\n")
    (electric-operator-test-type "4,5,6/)")
    (electric-operator-test-should-see "(/1, 2, 3, &\n\n4, 5, 6 /)")))
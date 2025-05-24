;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)

;; Make sure we actually managed to enable electric-operator-mode
(ert-deftest prog-enable-electric-spacing ()
  (test-with-mode prog-mode
    (should electric-operator-mode)))

;; Make sure the mode is really always being turned on and not just
;; toggling!
(ert-deftest prog-enable-electric-spacing-again ()
  (test-with-mode prog-mode
    (should electric-operator-mode)))

;; Basic stuff
(ert-deftest prog-space-a-simple-operator ()
  (test-with-mode prog-mode
    (electric-operator-test-type "a=b")
    (electric-operator-test-should-see "a = b")))

(ert-deftest prog-space-an-operator-with-spacing-after ()
  (test-with-mode prog-mode
    (electric-operator-test-type "f(a,b)")
    (electric-operator-test-should-see "f(a, b)")))

(ert-deftest prog-space-an-operator-with-spacing-before ()
  ;; There are no pre-spaced operators in default mode, so use c-mode
  (test-with-mode c-mode
    (electric-operator-test-type "char*a")
    (electric-operator-test-should-see "char *a")))

(ert-deftest prog-space-a-double-character-operator ()
  (test-with-mode prog-mode
    (electric-operator-test-type "a==b")
    (electric-operator-test-should-see "a == b")))

;; Digraphs work correctly
(ert-deftest prog-dont-space-!-operator ()
  (test-with-mode prog-mode
    (electric-operator-test-type "if(!b)")
    (electric-operator-test-should-see "if(!b)")))

(ert-deftest prog-space-!=-as-single-operator ()
  (test-with-mode prog-mode
    (electric-operator-test-type "a!=b")
    (electric-operator-test-should-see "a != b")))

(ert-deftest prog-but-dont-space-=!-as-a-single-operator ()
  (test-with-mode prog-mode
    (electric-operator-test-type "a=!b")
    (electric-operator-test-should-see "a = !b")))

(ert-deftest prog-dont-space-=*-as-single-operator ()
  ;; Interesting because * and = are both operators
  (test-with-mode prog-mode
    (electric-operator-test-type "a=*b")
    (electric-operator-test-should-see "a = * b")))

;; Decimal numbers
(ert-deftest prog-dont-space-the-dot-in-decimals ()
  (test-with-mode prog-mode
    (electric-operator-test-type "0.235")
    (electric-operator-test-should-see "0.235")))

;; Negative exponents
(ert-deftest prog-space---operator ()
  (test-with-mode prog-mode
    (electric-operator-test-type "e-b")
    (electric-operator-test-should-see "e - b")))

(ert-deftest prog-dont-space-negative-exponent-lower-case ()
  (test-with-mode prog-mode
    (electric-operator-test-type "1.2e-10")
    (electric-operator-test-should-see "1.2e-10")))

(ert-deftest prog-dont-space-negative-exponent-upper-case ()
  (test-with-mode prog-mode
    (electric-operator-test-type "1.2E-10")
    (electric-operator-test-should-see "1.2E-10")))

(ert-deftest prog-dont-space-negative-exponent-integer ()
  (test-with-mode prog-mode
    (electric-operator-test-type "5e-10")
    (electric-operator-test-should-see "5e-10")))

;; Unix #! paths
(ert-deftest prog-correctly-space-shebang ()
  (test-with-mode prog-mode
    (electric-operator-test-type "#! /bin/bash")
    (electric-operator-test-should-see "#! /bin/bash")))

(ert-deftest prog-but-also-space-division ()
  (test-with-mode prog-mode
    (electric-operator-test-type "a/b")
    (electric-operator-test-should-see "a / b")))

;; Negative numbers
(ert-deftest prog-space---operator-negative-numbers ()
  (test-with-mode prog-mode
    (electric-operator-test-type "e-b")
    (electric-operator-test-should-see "e - b")))

(ert-deftest prog-dont-space--1 ()
  (test-with-mode prog-mode
    (electric-operator-test-type "-1")
    (electric-operator-test-should-see-pattern "^-1$")))

(ert-deftest prog-dont-modify-indentation-of--1 ()
  (test-with-mode prog-mode
    (electric-operator-test-type "  -1")
    (electric-operator-test-should-see-pattern "^  -1$")))

(ert-deftest prog-dont-space-+1 ()
  (test-with-mode prog-mode
    (electric-operator-test-type "+1")
    (electric-operator-test-should-see-pattern "^\\+1$")))

(ert-deftest prog-dont-modify-indentation-of-+1 ()
  (test-with-mode prog-mode
    (electric-operator-test-type "  +1")
    (electric-operator-test-should-see-pattern "^  \\+1$")))

(ert-deftest prog-a-=-1 ()
  (test-with-mode prog-mode
    (electric-operator-test-type "a=-1")
    (electric-operator-test-should-see "a = -1")))

(ert-deftest prog-a-*--1 ()
  (test-with-mode prog-mode
    (electric-operator-test-type "a*-1")
    (electric-operator-test-should-see "a * -1")))

(ert-deftest prog-a-+--1 ()
  (test-with-mode prog-mode
    (electric-operator-test-type "a+-1")
    (electric-operator-test-should-see "a + -1")))

(ert-deftest prog-a----1 ()
  (test-with-mode prog-mode
    (electric-operator-test-type "a--1")
    (electric-operator-test-should-see "a - -1")))

(ert-deftest prog-a-/--1 ()
  (test-with-mode prog-mode
    (electric-operator-test-type "a/-1")
    (electric-operator-test-should-see "a / -1")))

(ert-deftest prog-a-^--1 ()
  (test-with-mode prog-mode
    (electric-operator-test-type "a^-1")
    (electric-operator-test-should-see "a ^ -1")))

(ert-deftest prog-a-<--1 ()
  (test-with-mode prog-mode
    (electric-operator-test-type "a<-1")
    (electric-operator-test-should-see "a < -1")))

(ert-deftest prog-a->--1 ()
  (test-with-mode prog-mode
    (electric-operator-test-type "a>-1")
    (electric-operator-test-should-see "a > -1")))

(ert-deftest prog-a-=--1- ()
  (test-with-mode prog-mode
    (electric-operator-test-type "a=[-1]")
    (electric-operator-test-should-see "a = [-1]")))

(ert-deftest prog-f--1- ()
  (test-with-mode prog-mode
    (electric-operator-test-type "f(-1)")
    (electric-operator-test-should-see "f(-1)")))

(ert-deftest prog-f-x---1- ()
  (test-with-mode prog-mode
    (electric-operator-test-type "f(x,-1)")
    (electric-operator-test-should-see "f(x, -1)")))

(ert-deftest prog-return ()
  (test-with-mode prog-mode
    (electric-operator-test-type "return -1")
    (electric-operator-test-should-see "return -1")))

(ert-deftest prog-a-=-+1 ()
  (test-with-mode prog-mode
    (electric-operator-test-type "a=+1")
    (electric-operator-test-should-see "a = +1")))

(ert-deftest prog-a-*-+1 ()
  (test-with-mode prog-mode
    (electric-operator-test-type "a*+1")
    (electric-operator-test-should-see "a * +1")))

(ert-deftest prog-a-+-+1 ()
  (test-with-mode prog-mode
    (electric-operator-test-type "a++1")
    (electric-operator-test-should-see "a + +1")))

(ert-deftest prog-a-/-+1 ()
  (test-with-mode prog-mode
    (electric-operator-test-type "a/+1")
    (electric-operator-test-should-see "a / +1")))

(ert-deftest prog-a-^-+1 ()
  (test-with-mode prog-mode
    (electric-operator-test-type "a^+1")
    (electric-operator-test-should-see "a ^ +1")))

(ert-deftest prog-a-<-+1 ()
  (test-with-mode prog-mode
    (electric-operator-test-type "a<+1")
    (electric-operator-test-should-see "a < +1")))

(ert-deftest prog-a->-+1 ()
  (test-with-mode prog-mode
    (electric-operator-test-type "a>+1")
    (electric-operator-test-should-see "a > +1")))

(ert-deftest prog-a-=-+1- ()
  (test-with-mode prog-mode
    (electric-operator-test-type "a=[+1]")
    (electric-operator-test-should-see "a = [+1]")))

(ert-deftest prog-f-+1- ()
  (test-with-mode prog-mode
    (electric-operator-test-type "f(+1)")
    (electric-operator-test-should-see "f(+1)")))

(ert-deftest prog-f-x--+1- ()
  (test-with-mode prog-mode
    (electric-operator-test-type "f(x,+1)")
    (electric-operator-test-should-see "f(x, +1)")))

(ert-deftest prog-return-positive ()
  (test-with-mode prog-mode
    (electric-operator-test-type "return +1")
    (electric-operator-test-should-see "return +1")))


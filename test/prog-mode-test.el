;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'electric-operator)
(require 'test-helper)

;; Make sure we actually managed to enable electric-operator-mode
(ert-deftest prog-enable-electric-spacing ()
  (th-fixtures #'prog-mode
    (should electric-operator-mode)))

;; Make sure the mode is really always being turned on and not just
;; toggling!
(ert-deftest prog-enable-electric-spacing-again ()
  (th-fixtures #'prog-mode
    (should electric-operator-mode)))

;; Basic stuff
(ert-deftest prog-space-a-simple-operator ()
  (th-fixtures #'prog-mode
    (th-type "a=b")
    (th-should-see "a = b")))

(ert-deftest prog-space-an-operator-with-spacing-after ()
  (th-fixtures #'prog-mode
    (th-type "f(a,b)")
    (th-should-see "f(a, b)")))

(ert-deftest prog-space-an-operator-with-spacing-before ()
  ;; There are no pre-spaced operators in default mode, so use c-mode
  (th-fixtures #'c-mode
    (th-type "char*a")
    (th-should-see "char *a")))

(ert-deftest prog-space-a-double-character-operator ()
  (th-fixtures #'prog-mode
    (th-type "a==b")
    (th-should-see "a == b")))

;; Digraphs work correctly
(ert-deftest prog-dont-space-!-operator ()
  (th-fixtures #'prog-mode
    (th-type "if(!b)")
    (th-should-see "if(!b)")))

(ert-deftest prog-space-!=-as-single-operator ()
  (th-fixtures #'prog-mode
    (th-type "a!=b")
    (th-should-see "a != b")))

(ert-deftest prog-but-dont-space-=!-as-a-single-operator ()
  (th-fixtures #'prog-mode
    (th-type "a=!b")
    (th-should-see "a = !b")))

(ert-deftest prog-dont-space-=*-as-single-operator ()
  ;; Interesting because * and = are both operators
  (th-fixtures #'prog-mode
    (th-type "a=*b")
    (th-should-see "a = * b")))

;; Decimal numbers
(ert-deftest prog-dont-space-the-dot-in-decimals ()
  (th-fixtures #'prog-mode
    (th-type "0.235")
    (th-should-see "0.235")))

;; Negative exponents
(ert-deftest prog-space---operator ()
  (th-fixtures #'prog-mode
    (th-type "e-b")
    (th-should-see "e - b")))

(ert-deftest prog-dont-space-negative-exponent-lower-case ()
  (th-fixtures #'prog-mode
    (th-type "1.2e-10")
    (th-should-see "1.2e-10")))

(ert-deftest prog-dont-space-negative-exponent-upper-case ()
  (th-fixtures #'prog-mode
    (th-type "1.2E-10")
    (th-should-see "1.2E-10")))

(ert-deftest prog-dont-space-negative-exponent-integer ()
  (th-fixtures #'prog-mode
    (th-type "5e-10")
    (th-should-see "5e-10")))

;; Unix #! paths
(ert-deftest prog-correctly-space-shebang ()
  (th-fixtures #'prog-mode
    (th-type "#! /bin/bash")
    (th-should-see "#! /bin/bash")))

(ert-deftest prog-but-also-space-division ()
  (th-fixtures #'prog-mode
    (th-type "a/b")
    (th-should-see "a / b")))

;; Negative numbers
(ert-deftest prog-space---operator-negative-numbers ()
  (th-fixtures #'prog-mode
    (th-type "e-b")
    (th-should-see "e - b")))

(ert-deftest prog-dont-space--1 ()
  (th-fixtures #'prog-mode
    (th-type "-1")
    (th-should-see-pattern "^-1$")))

(ert-deftest prog-dont-modify-indentation-of--1 ()
  (th-fixtures #'prog-mode
    (th-type "  -1")
    (th-should-see-pattern "^  -1$")))

(ert-deftest prog-dont-space-+1 ()
  (th-fixtures #'prog-mode
    (th-type "+1")
    (th-should-see-pattern "^\\+1$")))

(ert-deftest prog-dont-modify-indentation-of-+1 ()
  (th-fixtures #'prog-mode
    (th-type "  +1")
    (th-should-see-pattern "^  \\+1$")))

(ert-deftest prog-a-=-1 ()
  (th-fixtures #'prog-mode
    (th-type "a=-1")
    (th-should-see "a = -1")))

(ert-deftest prog-a-*--1 ()
  (th-fixtures #'prog-mode
    (th-type "a*-1")
    (th-should-see "a * -1")))

(ert-deftest prog-a-+--1 ()
  (th-fixtures #'prog-mode
    (th-type "a+-1")
    (th-should-see "a + -1")))

(ert-deftest prog-a----1 ()
  (th-fixtures #'prog-mode
    (th-type "a--1")
    (th-should-see "a - -1")))

(ert-deftest prog-a-/--1 ()
  (th-fixtures #'prog-mode
    (th-type "a/-1")
    (th-should-see "a / -1")))

(ert-deftest prog-a-^--1 ()
  (th-fixtures #'prog-mode
    (th-type "a^-1")
    (th-should-see "a ^ -1")))

(ert-deftest prog-a-<--1 ()
  (th-fixtures #'prog-mode
    (th-type "a<-1")
    (th-should-see "a < -1")))

(ert-deftest prog-a->--1 ()
  (th-fixtures #'prog-mode
    (th-type "a>-1")
    (th-should-see "a > -1")))

(ert-deftest prog-a-=--1- ()
  (th-fixtures #'prog-mode
    (th-type "a=[-1]")
    (th-should-see "a = [-1]")))

(ert-deftest prog-f--1- ()
  (th-fixtures #'prog-mode
    (th-type "f(-1)")
    (th-should-see "f(-1)")))

(ert-deftest prog-f-x---1- ()
  (th-fixtures #'prog-mode
    (th-type "f(x,-1)")
    (th-should-see "f(x, -1)")))

(ert-deftest prog-return ()
  (th-fixtures #'prog-mode
    (th-type "return -1")
    (th-should-see "return -1")))

(ert-deftest prog-a-=-+1 ()
  (th-fixtures #'prog-mode
    (th-type "a=+1")
    (th-should-see "a = +1")))

(ert-deftest prog-a-*-+1 ()
  (th-fixtures #'prog-mode
    (th-type "a*+1")
    (th-should-see "a * +1")))

(ert-deftest prog-a-+-+1 ()
  (th-fixtures #'prog-mode
    (th-type "a++1")
    (th-should-see "a + +1")))

(ert-deftest prog-a-/-+1 ()
  (th-fixtures #'prog-mode
    (th-type "a/+1")
    (th-should-see "a / +1")))

(ert-deftest prog-a-^-+1 ()
  (th-fixtures #'prog-mode
    (th-type "a^+1")
    (th-should-see "a ^ +1")))

(ert-deftest prog-a-<-+1 ()
  (th-fixtures #'prog-mode
    (th-type "a<+1")
    (th-should-see "a < +1")))

(ert-deftest prog-a->-+1 ()
  (th-fixtures #'prog-mode
    (th-type "a>+1")
    (th-should-see "a > +1")))

(ert-deftest prog-a-=-+1- ()
  (th-fixtures #'prog-mode
    (th-type "a=[+1]")
    (th-should-see "a = [+1]")))

(ert-deftest prog-f-+1- ()
  (th-fixtures #'prog-mode
    (th-type "f(+1)")
    (th-should-see "f(+1)")))

(ert-deftest prog-f-x--+1- ()
  (th-fixtures #'prog-mode
    (th-type "f(x,+1)")
    (th-should-see "f(x, +1)")))

(ert-deftest prog-return-positive ()
  (th-fixtures #'prog-mode
    (th-type "return +1")
    (th-should-see "return +1")))

(ert-deftest prog-operators-not-expanded-inside-comments ()
  (th-fixtures #'c-mode
    (th-type "//a=b*c")
    (th-should-see "// a=b*c")))


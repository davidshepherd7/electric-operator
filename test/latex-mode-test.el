;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)

(ert-deftest latex-mode-single-space-dot ()
  (th-fixtures latex-mode
    (let ((electric-operator-enable-in-docs t)
          (electric-operator-double-space-docs nil))
      (th-type "hello.World")
      (th-should-see "hello. World"))))

(ert-deftest latex-mode-space-+-in-math-mode ()
  (th-fixtures latex-mode
    (th-type "$a+b$ or $1+2$")
    (th-should-see "$a + b$ or $1 + 2$")))

(ert-deftest latex-mode-dont-space-+-in-text ()
  (th-fixtures latex-mode
    (th-type "a+b")
    (th-should-see "a+b")))

(ert-deftest latex-mode-exponents-and-subscripts ()
  (th-fixtures latex-mode
    (th-type "$a^+_*$")
    (th-should-see "$a^+_*$")))

(ert-deftest latex-mode-exponents-with-curly-braces ()
  (th-fixtures latex-mode
    (th-type "$a^{+}+2$")
    (th-should-see "$a^{+} + 2$")))

(ert-deftest latex-mode-exponents-followed-by-another-operator ()
  (th-fixtures latex-mode
    (th-type "$a^*+2$")
    (th-should-see "$a^* + 2$")))

(ert-deftest latex-mode-normal-unary-operator-situation ()
  (th-fixtures latex-mode
    (th-type "$a*+2$")
    (th-should-see "$a * +2$")))

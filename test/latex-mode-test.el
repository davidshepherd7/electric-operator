;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)

(ert-deftest latex-mode-single-space-dot ()
  (test-with-mode latex-mode
    (let ((electric-operator-enable-in-docs t)
          (electric-operator-double-space-docs nil))
      (electric-operator-test-type "hello.World")
      (electric-operator-test-should-see "hello. World"))))

(ert-deftest latex-mode-space-+-in-math-mode ()
  (test-with-mode latex-mode
    (electric-operator-test-type "$a+b$ or $1+2$")
    (electric-operator-test-should-see "$a + b$ or $1 + 2$")))

(ert-deftest latex-mode-dont-space-+-in-text ()
  (test-with-mode latex-mode
    (electric-operator-test-type "a+b")
    (electric-operator-test-should-see "a+b")))

(ert-deftest latex-mode-exponents-and-subscripts ()
  (test-with-mode latex-mode
    (electric-operator-test-type "$a^+_*$")
    (electric-operator-test-should-see "$a^+_*$")))

(ert-deftest latex-mode-exponents-with-curly-braces ()
  (test-with-mode latex-mode
    (electric-operator-test-type "$a^{+}+2$")
    (electric-operator-test-should-see "$a^{+} + 2$")))

(ert-deftest latex-mode-exponents-followed-by-another-operator ()
  (test-with-mode latex-mode
    (electric-operator-test-type "$a^*+2$")
    (electric-operator-test-should-see "$a^* + 2$")))

(ert-deftest latex-mode-normal-unary-operator-situation ()
  (test-with-mode latex-mode
    (electric-operator-test-type "$a*+2$")
    (electric-operator-test-should-see "$a * +2$")))

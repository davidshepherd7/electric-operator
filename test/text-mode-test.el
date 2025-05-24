;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)

(ert-deftest text-single-space-. ()
  (test-with-mode text-mode
    (let ((electric-operator-enable-in-docs t)
          (electric-operator-double-space-docs nil))
      (electric-operator-test-type "hello.World")
      (electric-operator-test-should-see "hello. World"))))

(ert-deftest text-double-space-. ()
  (test-with-mode text-mode
    (let ((electric-operator-enable-in-docs t)
          (electric-operator-double-space-docs t))
      (electric-operator-test-type "hello.World")
      (electric-operator-test-should-see "hello.  World"))))

(ert-deftest text-typing-comma-before-an-empty-line-doesnt-ruin-everything ()
  (test-with-mode text-mode
    (let ((electric-operator-enable-in-docs t))
      (insert "header1:\n1. abcde\n2. arosietn\n3. asdf\n\ncompile and install:")
      (goto-char (point-min))
      (search-forward "asdf")
      (electric-operator-test-type ",")
      (electric-operator-test-should-see-pattern "^3\\. asdf, $"))))

;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)

(ert-deftest text-single-space-. ()
  (th-fixtures text-mode
    (let ((electric-operator-enable-in-docs t)
          (electric-operator-double-space-docs nil))
      (th-type "hello.World")
      (th-should-see "hello. World"))))

(ert-deftest text-double-space-. ()
  (th-fixtures text-mode
    (let ((electric-operator-enable-in-docs t)
          (electric-operator-double-space-docs t))
      (th-type "hello.World")
      (th-should-see "hello.  World"))))

(ert-deftest text-typing-comma-before-an-empty-line-doesnt-ruin-everything ()
  (th-fixtures text-mode
    (let ((electric-operator-enable-in-docs t))
      (insert "header1:\n1. abcde\n2. arosietn\n3. asdf\n\ncompile and install:")
      (goto-char (point-min))
      (search-forward "asdf")
      (th-type ",")
      (th-should-see-pattern "^3\\. asdf, $"))))

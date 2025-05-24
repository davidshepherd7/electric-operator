;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)

;; I'm testing with ',' here because , is spaced in both prose and prog
;; modes. '.' is not spaced in prog modes so using that missed a bug.

(ert-deftest optional-spacing-dont-run-in-strings-when-disabled ()
  (th-fixtures python-mode
    (let ((electric-operator-enable-in-docs nil))
      (th-type "'Hello,world'")
      (th-should-see "'Hello,world'"))))

(ert-deftest optional-spacing-dont-run-in-comments-when-disabled ()
  (th-fixtures python-mode
    (let ((electric-operator-enable-in-docs nil))
      (th-type "#Hello,world")
      (th-should-see "#Hello,world"))))

(ert-deftest optional-spacing-do-run-in-strings-when-enabled ()
  (th-fixtures python-mode
    (let ((electric-operator-enable-in-docs t))
      (th-type "'Hello,world'")
      (th-should-see "'Hello, world'"))))

(ert-deftest optional-spacing-do-run-in-comments-when-enabled ()
  (th-fixtures python-mode
    (let ((electric-operator-enable-in-docs t))
      (th-type "#Hello,world")
      (th-should-see "#Hello, world"))))

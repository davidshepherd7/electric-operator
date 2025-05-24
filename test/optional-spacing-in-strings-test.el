;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)

;; I'm testing with ',' here because , is spaced in both prose and prog
;; modes. '.' is not spaced in prog modes so using that missed a bug.

(ert-deftest optional-spacing-dont-run-in-strings-when-disabled ()
  (test-with-mode python-mode
    (let ((electric-operator-enable-in-docs nil))
      (electric-operator-test-type "'Hello,world'")
      (electric-operator-test-should-see "'Hello,world'"))))

(ert-deftest optional-spacing-dont-run-in-comments-when-disabled ()
  (test-with-mode python-mode
    (let ((electric-operator-enable-in-docs nil))
      (electric-operator-test-type "#Hello,world")
      (electric-operator-test-should-see "#Hello,world"))))

(ert-deftest optional-spacing-do-run-in-strings-when-enabled ()
  (test-with-mode python-mode
    (let ((electric-operator-enable-in-docs t))
      (electric-operator-test-type "'Hello,world'")
      (electric-operator-test-should-see "'Hello, world'"))))

(ert-deftest optional-spacing-do-run-in-comments-when-enabled ()
  (test-with-mode python-mode
    (let ((electric-operator-enable-in-docs t))
      (electric-operator-test-type "#Hello,world")
      (electric-operator-test-should-see "#Hello, world"))))
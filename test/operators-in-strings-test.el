;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)

(ert-deftest strings-dont-space-unix-separators ()
  ;; The second string checks that we did manage to turn on
  ;; electric-operator in strings.
  (test-with-mode python-mode
    (let ((electric-operator-enable-in-docs t))
      (electric-operator-test-type "a='/usr/bin/python3'")
      (electric-operator-test-should-see "a = '/usr/bin/python3'"))))

(ert-deftest strings-dont-space-windows-separators ()
  (test-with-mode python-mode
    (let ((electric-operator-enable-in-docs t))
      (electric-operator-test-type "a='C:\\WINDOWS'")
      (electric-operator-test-should-see "a = 'C:\\WINDOWS'"))))
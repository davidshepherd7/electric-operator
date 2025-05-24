;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)

(ert-deftest strings-dont-space-unix-separators ()
  ;; The second string checks that we did manage to turn on
  ;; electric-operator in strings.
  (th-fixtures #'python-mode
    (let ((electric-operator-enable-in-docs t))
      (th-type "a='/usr/bin/python3'")
      (th-should-see "a = '/usr/bin/python3'"))))

(ert-deftest strings-dont-space-windows-separators ()
  (th-fixtures #'python-mode
    (let ((electric-operator-enable-in-docs t))
      (th-type "a='C:\\WINDOWS'")
      (th-should-see "a = 'C:\\WINDOWS'"))))

;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)

(ert-deftest ruby-mode-dont-modify-string-literal ()
  (test-with-mode ruby-mode
    ;; For some reason this is extremely slow in tests, like >1 second per |
    ;; character that it matches
    (let ((smie-blink-matching-open nil)
          (electric-operator-enable-in-docs nil))
      (electric-operator-test-type "'var+foo-1'")
      (electric-operator-test-should-see "'var+foo-1'"))))

(ert-deftest ruby-mode-block-params-with-{} ()
  (test-with-mode ruby-mode
    (let ((smie-blink-matching-open nil))
      (electric-operator-test-type "arr.map { |a|a }")
      (electric-operator-test-should-see "arr.map { |a| a }"))))

(ert-deftest ruby-mode-block-params-with-do ()
  (test-with-mode ruby-mode
    (let ((smie-blink-matching-open nil))
      (electric-operator-test-type "arr.map do|a|a end")
      (electric-operator-test-should-see "arr.map do |a| a end"))))

(ert-deftest ruby-mode-normal-|-usage ()
  (test-with-mode ruby-mode
    (let ((smie-blink-matching-open nil))
      (electric-operator-test-type "3|4")
      (electric-operator-test-should-see "3 | 4"))))

;; Check *args works ok
(ert-deftest ruby-mode-space-*args-on-its-own ()
  (test-with-mode ruby-mode
    (let ((smie-blink-matching-open nil))
      (electric-operator-test-type "f(*args)")
      (electric-operator-test-should-see "f(*args)"))))

(ert-deftest ruby-mode-space-*args-with-other-args ()
  (test-with-mode ruby-mode
    (let ((smie-blink-matching-open nil))
      (electric-operator-test-type "f(a,*args)")
      (electric-operator-test-should-see "f(a, *args)"))))

(ert-deftest ruby-mode-space-*args-with-a-newline-before-it ()
  (test-with-mode ruby-mode
    (let ((smie-blink-matching-open nil))
      (insert "f(a,\n\n")
      (electric-operator-test-type "*args)")
      (electric-operator-test-should-see "f(a,\n\n*args)"))))

;; And **kwargs
(ert-deftest ruby-mode-space-**kwargs-on-its-own ()
  (test-with-mode ruby-mode
    (let ((smie-blink-matching-open nil))
      (electric-operator-test-type "f(**kwargs)")
      (electric-operator-test-should-see "f(**kwargs)"))))

(ert-deftest ruby-mode-space-**kwargs-with-other-args ()
  (test-with-mode ruby-mode
    (let ((smie-blink-matching-open nil))
      (electric-operator-test-type "f(a,**kwargs)")
      (electric-operator-test-should-see "f(a, **kwargs)"))))

(ert-deftest ruby-mode-space-**kwargs-with-a-newline-before-it ()
  (test-with-mode ruby-mode
    (let ((smie-blink-matching-open nil))
      (insert "f(a,\n\n")
      (electric-operator-test-type "**kwargs)")
      (electric-operator-test-should-see "f(a,\n\n**kwargs)"))))

(ert-deftest ruby-mode-percentage-literals ()
  (test-with-mode ruby-mode
    (let ((smie-blink-matching-open nil))
      (insert "%[ship good code]")
      (electric-operator-test-should-see "%[ship good code]"))))

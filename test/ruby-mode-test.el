;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)

(ert-deftest ruby-mode-dont-modify-string-literal ()
  (th-fixtures ruby-mode
    ;; For some reason this is extremely slow in tests, like >1 second per |
    ;; character that it matches
    (let ((smie-blink-matching-open nil)
          (electric-operator-enable-in-docs nil))
      (th-type "'var+foo-1'")
      (th-should-see "'var+foo-1'"))))

(ert-deftest ruby-mode-block-params-with-{} ()
  (th-fixtures ruby-mode
    (let ((smie-blink-matching-open nil))
      (th-type "arr.map { |a|a }")
      (th-should-see "arr.map { |a| a }"))))

(ert-deftest ruby-mode-block-params-with-do ()
  (th-fixtures ruby-mode
    (let ((smie-blink-matching-open nil))
      (th-type "arr.map do|a|a end")
      (th-should-see "arr.map do |a| a end"))))

(ert-deftest ruby-mode-normal-|-usage ()
  (th-fixtures ruby-mode
    (let ((smie-blink-matching-open nil))
      (th-type "3|4")
      (th-should-see "3 | 4"))))

;; Check *args works ok
(ert-deftest ruby-mode-space-*args-on-its-own ()
  (th-fixtures ruby-mode
    (let ((smie-blink-matching-open nil))
      (th-type "f(*args)")
      (th-should-see "f(*args)"))))

(ert-deftest ruby-mode-space-*args-with-other-args ()
  (th-fixtures ruby-mode
               (let ((smie-blink-matching-open nil))
                 (th-type "f(a,*args)")
                 (th-should-see "f(a, *args)"))))

(ert-deftest ruby-mode-space-*args-with-a-newline-before-it ()
  (th-fixtures ruby-mode
               (let ((smie-blink-matching-open nil))
                 (insert "f(a,\n\n")
                 (th-type "*args)")
                 (th-should-see "f(a,\n\n*args)"))))

;; And **kwargs
(ert-deftest ruby-mode-space-**kwargs-on-its-own ()
  (th-fixtures ruby-mode
               (let ((smie-blink-matching-open nil))
                 (th-type "f(**kwargs)")
                 (th-should-see "f(**kwargs)"))))

(ert-deftest ruby-mode-space-**kwargs-with-other-args ()
  (th-fixtures ruby-mode
               (let ((smie-blink-matching-open nil))
                 (th-type "f(a,**kwargs)")
                 (th-should-see "f(a, **kwargs)"))))

(ert-deftest ruby-mode-space-**kwargs-with-a-newline-before-it ()
  (th-fixtures ruby-mode
               (let ((smie-blink-matching-open nil))
                 (insert "f(a,\n\n")
                 (th-type "**kwargs)")
                 (th-should-see "f(a,\n\n**kwargs)"))))

(ert-deftest ruby-mode-percentage-literals ()
  (th-fixtures ruby-mode
               (let ((smie-blink-matching-open nil))
                 (insert "%[ship good code]")
                 (th-should-see "%[ship good code]"))))

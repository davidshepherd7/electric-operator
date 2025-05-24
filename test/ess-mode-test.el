;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)
(require 'ess-r-mode)

(ert-deftest ess-r-mode-dot-tilde-operator ()
  (test-with-mode ess-r-mode
    (electric-operator-test-type "Species~.")
    (electric-operator-test-should-see "Species ~ .")))

(ert-deftest ess-r-mode-interaction-with-ess-smart-comma-when-enabled ()
  (test-with-mode ess-r-mode
    (let ((ess-r-smart-operators t))
      (electric-operator-test-type "f(x,y)")
      (electric-operator-test-should-see "f(x, y)"))))

(ert-deftest ess-r-mode-interaction-with-ess-smart-comma-when-disabled ()
  (test-with-mode ess-r-mode
    (let ((ess-r-smart-operators nil))
      (electric-operator-test-type "f(x,y)")
      (electric-operator-test-should-see "f(x, y)"))))

(ert-deftest ess-r-mode-interaction-with-ess-smart-command-when-disabled-and-mode-disabled ()
  (let ((ess-r-smart-operators nil))
    (with-temp-buffer
      (ess-r-mode)
      (electric-operator-mode -1)
      (electric-operator-test-type "f(x,y)")
      (electric-operator-test-should-see "f(x,y)"))))

(ert-deftest ess-r-mode-spaced-equals-for-keyword-args ()
  (test-with-mode ess-r-mode
    (let ((electric-operator-R-named-argument-style 'spaced))
      (electric-operator-test-type "somefunc(a=1, b=2)")
      (electric-operator-test-should-see "somefunc(a = 1, b = 2)"))))

(ert-deftest ess-r-mode-equals-for-keyword-args ()
  (test-with-mode ess-r-mode
    (let ((electric-operator-R-named-argument-style 'unspaced))
      (electric-operator-test-type "somefunc(a=1, b=2)")
      (electric-operator-test-should-see "somefunc(a=1, b=2)"))))

(ert-deftest ess-r-mode-lone-dot-does-not-force-unary-operators ()
  (test-with-mode ess-r-mode
    (electric-operator-test-type "x~.+1")
    (electric-operator-test-should-see "x ~ . + 1")))

(ert-deftest ess-r-mode-colons-for-ranges-are-not-spaced ()
  (test-with-mode ess-r-mode
    (electric-operator-test-type "for (i in 1:100) {")
    (electric-operator-test-should-see "for (i in 1:100) {")))

(ert-deftest ess-r-mode-bang-bang-operator ()
  (test-with-mode ess-r-mode
    (electric-operator-test-type "group_by(!!grouping_var)")
    (electric-operator-test-should-see "group_by(!!grouping_var)")))

(ert-deftest ess-r-mode-data-table-assign-operator ()
  (test-with-mode ess-r-mode
    (electric-operator-test-type "DT[, V1 := exp(V1)]")
    (electric-operator-test-should-see "DT[, V1 := exp(V1)]")))

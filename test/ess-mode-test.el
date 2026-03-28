;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'electric-operator)
(require 'test-helper)
(require 'ess-r-mode)

;; Prevent flymake from warning about R not being installed
(setq ess-use-flymake nil)

(ert-deftest ess-r-mode-dot-tilde-operator ()
  (th-fixtures #'ess-r-mode
    (th-type "Species~.")
    (th-should-see "Species ~ .")))

(ert-deftest ess-r-mode-interaction-with-ess-smart-comma-when-enabled ()
  (th-fixtures #'ess-r-mode
    (let ((ess-r-smart-operators t))
      (th-type "f(x,y)")
      (th-should-see "f(x, y)"))))

(ert-deftest ess-r-mode-interaction-with-ess-smart-comma-when-disabled ()
  (th-fixtures #'ess-r-mode
    (let ((ess-r-smart-operators nil))
      (th-type "f(x,y)")
      (th-should-see "f(x, y)"))))

(ert-deftest ess-r-mode-interaction-with-ess-smart-command-when-disabled-and-mode-disabled ()
  (let ((ess-r-smart-operators nil))
    (with-temp-buffer
      (ess-r-mode)
      (electric-operator-mode -1)
      (th-type "f(x,y)")
      (th-should-see "f(x,y)"))))

(ert-deftest ess-r-mode-spaced-equals-for-keyword-args ()
  (th-fixtures #'ess-r-mode
    (let ((electric-operator-R-named-argument-style 'spaced))
      (th-type "somefunc(a=1, b=2)")
      (th-should-see "somefunc(a = 1, b = 2)"))))

(ert-deftest ess-r-mode-equals-for-keyword-args ()
  (th-fixtures #'ess-r-mode
    (let ((electric-operator-R-named-argument-style 'unspaced))
      (th-type "somefunc(a=1, b=2)")
      (th-should-see "somefunc(a=1, b=2)"))))

(ert-deftest ess-r-mode-lone-dot-does-not-force-unary-operators ()
  (th-fixtures #'ess-r-mode
    (th-type "x~.+1")
    (th-should-see "x ~ . + 1")))

(ert-deftest ess-r-mode-colons-for-ranges-are-not-spaced ()
  (th-fixtures #'ess-r-mode
    (th-type "for (i in 1:100) {")
    (th-should-see "for (i in 1:100) {")))

(ert-deftest ess-r-mode-bang-bang-operator ()
  (th-fixtures #'ess-r-mode
    (th-type "group_by(!!grouping_var)")
    (th-should-see "group_by(!!grouping_var)")))

(ert-deftest ess-r-mode-data-table-assign-operator ()
  (th-fixtures #'ess-r-mode
    (th-type "DT[, V1 := exp(V1)]")
    (th-should-see "DT[, V1 := exp(V1)]")))

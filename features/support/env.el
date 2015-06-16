(require 'f)

(defvar electric-operator-support-path
  (f-dirname load-file-name))

(defvar electric-operator-features-path
  (f-parent electric-operator-support-path))

(defvar electric-operator-root-path
  (f-parent electric-operator-features-path))

(add-to-list 'load-path electric-operator-root-path)

(require 'electric-operator)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )

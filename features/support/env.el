(require 'f)

(defvar electric-spacing-support-path
  (f-dirname load-file-name))

(defvar electric-spacing-features-path
  (f-parent electric-spacing-support-path))

(defvar electric-spacing-root-path
  (f-parent electric-spacing-features-path))

(add-to-list 'load-path electric-spacing-root-path)

(require 'electric-spacing)
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

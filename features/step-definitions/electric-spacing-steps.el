;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Then "^electric-spacing-mode is on$"
      (lambda ()
        (cl-assert electric-spacing-mode)
        ))

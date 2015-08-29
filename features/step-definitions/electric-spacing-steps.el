;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Then "^electric-operator-mode is on$"
      (lambda ()
        (cl-assert electric-operator-mode)
        ))

(When "^I'm inside C main"
      (lambda ()
        (When "The buffer is empty")
        (insert "int main() {\n")
        (insert "\n")
        (insert "}\n")
        (When "I go to line \"2\"")))

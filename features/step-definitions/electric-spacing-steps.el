;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Then "^electric-operator-mode is on$"
      (lambda ()
        (cl-assert electric-operator-mode)
        ))

;; C/C++ stuff
(When "^I'm inside C main"
      (lambda ()
        (When "The buffer is empty")
        (insert "int main() {\n")
        (insert "\n")
        (insert "}\n")
        (When "I go to line \"2\"")))


;; Rust stuff
(When "^inside a rust function$"
      (lambda ()
        (insert "fn foo() -> i32 {\n")
        (insert "\n")
        (insert "}\n")
        (When "I go to line \"2\"")
        ))

(When "^inside a rust pattern matching statement$"
      (lambda ()
        (insert "match x {\n")
        (insert "\n")
        (insert "}\n")
        (When "I go to line \"2\"")
        ))

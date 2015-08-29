Feature: * operator
  Background:
      When the buffer is empty
      When I turn on c-mode
      When I turn on electric-operator-mode

    Scenario: Multiplication
      # Some of cc-mode's syntatic analysis stuff requires us to be inside
      # a function to properly detect that we aren't declaring a function.
      When I insert:
      """
      int main() {

      }
      """
      When I go to line "2"
      When I type "a*b"
      Then I should see "a * b"

    Scenario: Pointer type
      When I insert:
      """
      int main() {

      }
      """
      When I go to line "2"
      When I type "char*a"
      Then I should see "char *a"

    Scenario: Non-builtin pointer type
      When I insert:
      """
      int main() {

      }
      """
      When I go to line "2"
      When I type "size_t*a"
      Then I should see "size_t *a"

    Scenario: Assign pointer dereference
      When I insert:
      """
      int main() {

      }
      """
      When I go to line "2"
      When I type "a=*b"
      Then I should see "a = *b"

    Scenario: Pointer dereference and increment
      When I insert:
      """
      int main() {

      }
      """
      When I go to line "2"
      When I type "*p++"
      Then I should see "*p++"

    Scenario: Function call with dereference
      When I insert:
      """
      int main() {

      }
      """
      When I go to line "2"
      Then I type "f(*p,*q)"
      Then I should see "f(*p, *q)"



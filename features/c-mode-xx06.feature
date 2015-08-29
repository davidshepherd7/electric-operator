Feature: & operator
  Background:
      When the buffer is empty
      When I turn on c-mode
      When I turn on electric-operator-mode

    Scenario: Bitwise and
      When I insert:
      """
      int main() {

      }
      """
      When I go to line "2"
      When I type "a&b"
      Then I should see "a & b"

    Scenario: Reference type
      When I insert:
      """
      int main() {

      }
      """
      When I go to line "2"
      When I type "char&a"
      Then I should see "char &a"

    Scenario: Assign address of
      When I insert:
      """
      int main() {

      }
      """
      When I go to line "2"
      When I type "a=&b"
      Then I should see "a = &b"

    Scenario: Function call with address of
      When I insert:
      """
      int main() {

      }
      """
      When I go to line "2"
      Then I type "f(&p,&q)"
      Then I should see "f(&p, &q)"



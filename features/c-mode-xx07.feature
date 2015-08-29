Feature: Pointer to pointer type
  Background:
      When the buffer is empty
      When I turn on c-mode
      When I turn on electric-operator-mode

  Scenario: pointer to pointer type
      When I insert:
      """
      int main() {

      }
      """
      When I go to line "2"
      When I type "char**a"
      Then I should see "char **a"



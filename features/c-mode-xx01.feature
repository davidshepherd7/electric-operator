Feature: -> operator
  Background:
    When the buffer is empty
    When I turn on c-mode
    When I turn on electric-operator-mode

  Scenario: Space >
    When I insert:
    """
    int main() {

    }
    """
    When I go to line "2"
    When I type "a>b"
    Then I should see "a > b"

  Scenario: Space -
    When I type "a-b"
    Then I should see "a - b"

  Scenario: Don't space ->
    When I type "a->b"
    Then I should see "a->b"



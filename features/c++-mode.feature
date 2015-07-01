# Most C++ features are covered by c-mode

Feature: Move constructor type
  Background:
    When the buffer is empty
    When I turn on c++-mode
    When I turn on electric-operator-mode

  Scenario: and operator works
    When I insert:
    """
    int main() {

    }
    """
    When I go to line "2"
    When I type "x=a&&b"
    Then I should see "a && b"

  Scenario: Move constructor works
    When I type "A(A&&a)"
    Then I should see "A(A &&a)"

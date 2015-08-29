Feature: Type detection works inside classes
  Background:
    When the buffer is empty
    When I turn on c++-mode
    When I turn on electric-operator-mode

  Scenario: Move constructor
    When I insert:
    """
    struct A {

    }
    """
    When I go to line "2"
    When I type "A(A&&a)"
    Then I should see "A(A &&a)"



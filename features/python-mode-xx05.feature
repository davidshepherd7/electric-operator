Feature: Keyword argument =
  Background:
    When the buffer is empty
    When I turn on python-mode
    When I turn on electric-operator-mode

  Scenario: Space standard assignment as normal
    When I type "a=b"
    Then I should see "a = b"

  Scenario: Don't space assignment inside function call
    When I type "f(a=b)"
    Then I should see "f(a=b)"



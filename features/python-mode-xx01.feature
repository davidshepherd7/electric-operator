Feature: Python mode basics
  Background:
    When the buffer is empty
    When I turn on python-mode
    When I turn on electric-operator-mode

  Scenario: Enable electric spacing
    Then electric-operator-mode is on

  Scenario: Space + operator
    When I type "a+b"
    Then I should see "a + b"



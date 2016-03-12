Feature: R specific operators
  Background:
    When the buffer is empty
    When I turn on R-mode
    When I turn on electric-operator-mode

  Scenario: dot-tilde operator
    When I type "Species~."
    Then I should see "Species ~ ."

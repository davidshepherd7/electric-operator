Feature: R specific operators
  Background:
    When the buffer is empty
    When I turn on R-mode
    When I turn on electric-operator-mode

  Scenario: dot-tilde operator
    When I type "Species~."
    Then I should see "Species ~ ."

  Scenario: Interaction with ess-smart-comma when enabled
    When I set ess-smart-operators to t
    When I type "f(x,y)"
    Then I should see "f(x, y)"

  Scenario: Interaction with ess-smart-comma when disabled
    When I set ess-smart-operators to nil
    When I type "f(x,y)"
    Then I should see "f(x, y)"

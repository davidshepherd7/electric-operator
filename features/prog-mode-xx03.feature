Feature: Decimal numbers
  Background:
    When I turn on prog-mode
    When I turn on electric-operator-mode
    When the buffer is empty

  Scenario: Don't space decimals
    When I type "0.235"
    Then I should see "0.235"



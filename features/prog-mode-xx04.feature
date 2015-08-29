Feature: Negative exponents
  Background:
    When I turn on prog-mode
    When I turn on electric-operator-mode
    When the buffer is empty

  Scenario: Space - operator
    When I type "e-b"
    Then I should see "e - b"

  Scenario: Don't space negative exponent (lower case)
    When I type "1.2e-10"
    Then I should see "1.2e-10"

  Scenario: Don't space negative exponent (upper case)
    When I type "1.2E-10"
    Then I should see "1.2E-10"

  Scenario: Don't space negative exponent (integer)
    When I type "5e-10"
    Then I should see "5e-10"



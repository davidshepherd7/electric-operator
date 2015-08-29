Feature: -> operator
  Background:
    When I turn on c-mode
    When I turn on electric-operator-mode
    When I'm inside C main

  Scenario: Space >
    When I type "a>b"
    Then I should see "a > b"

  Scenario: Space -
    When I type "a-b"
    Then I should see "a - b"

  Scenario: Don't space ->
    When I type "a->b"
    Then I should see "a->b"

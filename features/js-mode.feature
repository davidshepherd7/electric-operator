Feature: Javascript specific operators
  Background:
    When the buffer is empty
    When I turn on js-mode
    When I turn on electric-operator-mode

  Scenario: Colon inside objects
    When I type "{a:1}"
    Then I should see "{a: 1}"

  Scenario: Ternary operator
    When I type "bool?1:2"
    Then I should see "bool ? 1 : 2"

  Scenario: Division
    When I type "x=b/a.foo"
    Then I should see "x = b / a.foo"

  Scenario: Regex literals simple
    When I type "/a.foo/"
    Then I should see "/a.foo/"

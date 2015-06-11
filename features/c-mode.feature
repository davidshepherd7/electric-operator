Feature: Pointer dereference is unspaced
  Background:
    When the buffer is empty
    When I turn on c-mode
    When I turn on electric-spacing-mode

  Scenario: Space >
    When I type "a>b"
    Then I should see "a > b"

  Scenario: Space -
    When I type "a-b"
    Then I should see "a - b"

  Scenario: Don't space ->
    When I type "a->b"
    Then I should see "a->b"

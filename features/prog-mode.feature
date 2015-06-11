Feature: Generic programming mode spacing
  Background:
    When I turn on prog-mode
    When I turn on electric-spacing-mode
    When the buffer is empty

  Scenario: Space = operator
    When I type "a=b"
    Then I should see "a = b"

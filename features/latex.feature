Feature: LaTeX support
  Background:
    When the buffer is empty
    When I turn on latex-mode
    When I turn on electric-operator-mode

  Scenario: Single space '.'
    When I set electric-operator-enable-in-docs to t
    When I set electric-operator-double-space-docs to nil
    When I type "hello.World"
    Then I should see "hello. World"

  Scenario: Space + in math mode
    When I type "$a+b$"
    Then I should see "a + b"

  Scenario: Don't space + in text
    When I type "a+b"
    Then I should see "a+b"

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
    When I type "$a+b$ or $1+2$"
    Then I should see "a + b"
    Then I should see "1 + 2"

  Scenario: Don't space + in text
    When I type "a+b"
    Then I should see "a+b"

  Scenario: Exponents and subscripts
    When I type "$a^+_*$"
    Then I should see "a^+_*"

  Scenario: Exponents with curly braces
    When I type "$a^{+}+2$"
    Then I should see "a^{+} + 2"

  Scenario: Exponents followed by another operator
    When I type "$a^*+2$"
    Then I should see "a^* + 2"

  Scenario: Normal unary operator situation
    When I type "$a*+2$"
    Then I should see "a * +2"

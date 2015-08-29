Feature: Ternary operator
    Background:
      When the buffer is empty
      When I turn on c-mode
      When I turn on electric-operator-mode

    Scenario: Ternary operator
      When I type "a?b:c"
      Then I should see "a ? b : c"

    Scenario: Label
      When I type "error:"
      Then I should see "error:"



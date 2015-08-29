Feature: Optionally run in comments and strings
  # I'm testing with ',' here because , is spaced in both prose and prog
  # modes. '.' is not spaced in prog modes so using that missed a bug.

  Background:
    When I turn on python-mode
    When I turn on electric-operator-mode
    When the buffer is empty

  Scenario: Don't run in strings when disabled
    When I set electric-operator-enable-in-docs to nil
    When I type "'Hello,world'"
    Then I should see "'Hello,world'"

  Scenario: Don't run in comments when disabled
    When I set electric-operator-enable-in-docs to nil
    When I type "#Hello,world"
    Then I should see "#Hello,world"

  Scenario: Do run in strings when enabled
    When I set electric-operator-double-space-docs to nil
    When I set electric-operator-enable-in-docs to t
    When I type "'Hello,world'"
    Then I should see "'Hello, world'"

  Scenario: Do run in comments when enabled
    When I set electric-operator-double-space-docs to nil
    When I set electric-operator-enable-in-docs to t
    When I type "#Hello,world"
    Then I should see "#Hello, world"



Feature: Slice operator
  Background:
    When the buffer is empty
    When I turn on python-mode
    When I turn on electric-operator-mode

  Scenario: Don't space : inside slices
    When I type "a[1:2]"
    Then I should see "a[1:2]"

  Scenario: Don't space negative slices
    When I type "a[-1:-2]"
    Then I should see "a[-1:-2]"

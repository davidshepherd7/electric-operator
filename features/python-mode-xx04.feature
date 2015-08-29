Feature: Member access
  Background:
    When the buffer is empty
    When I turn on python-mode
    When I turn on electric-operator-mode

  Scenario: Don't space accessing class members
    When I type "my_class.a"
    Then I should see "my_class.a"



Feature: Don't space path separators
  Background:
    When I turn on python-mode
    When I turn on electric-operator-mode
    When the buffer is empty
    When I set electric-operator-enable-in-docs to t

  Scenario: Don't space UNIX separators
    # The second string checks that we did manage to turn on
    # electric-operator in strings.
    When I type "a='/usr/bin/python3'"
    Then I should see "a = '/usr/bin/python3'"

  Scenario: Don't space windows separators
    When I type "a='C:\WINDOWS'"
    Then I should see "a = 'C:\WINDOWS'"

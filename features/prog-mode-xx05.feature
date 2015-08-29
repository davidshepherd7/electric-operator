Feature: Unix #! paths
  Background:
    When I turn on prog-mode
    When I turn on electric-operator-mode
    When the buffer is empty

  Scenario: Correctly space basic #!
    When I type "#! /bin/bash"
    Then I should see "#! /bin/bash"

  Scenario: But also space division
    When I type "a/b"
    Then I should see "a / b"



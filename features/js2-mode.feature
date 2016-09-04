Feature: js2-mode (an alternative javascript major mode)
  Background:
    When the buffer is empty
    When I turn on js2-mode
    When I turn on electric-operator-mode

  Scenario: It gets the javascript rules
    When I type "{a:1}"
    Then I should see "{a: 1}"

  Scenario: Regex literals simple
    When I type "/a.foo/"
    Then I should see "/a.foo/"

  Scenario: Regex literals with spacing before
    When I type "x=/a.foo/"
    Then I should see "x = /a.foo/"

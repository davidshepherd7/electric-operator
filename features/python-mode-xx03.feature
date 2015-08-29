Feature: python dictionaries
  Background:
    When the buffer is empty
    When I turn on python-mode
    When I turn on electric-operator-mode

  Scenario: Don't space : in most cases
    When I type "if:"
    Then I should see "if:"

  Scenario: Space after : in dict
    When I type "{a:1}"
    Then I should see "{a: 1}"

  # TODO: maybe check more convoluted nesting? dicts inside fucntions?
  # commas? newlines? lists? These are probably automatically ok with the
  # current implementation though.



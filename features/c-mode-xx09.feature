Feature: Comments
  Background:
    When the buffer is empty
    When I turn on c-mode
    When I turn on electric-operator-mode

  Scenario: // is not spaced internally
    When I type "//"
    Then I should see "// "

  Scenario: /* is not spaced internally
    When I type "/*"
    Then I should see "/* "

  # # This doesn't actually test what it should, it always passes. Possible
  # # ecukes bug?
  # Scenario: // does not lose indentation
  #   When I insert:
  #     """
  #     {
  #       /
  #     }
  #     """
  #   Then I should see:
  #     """
  #     {
  #       /
  #     }
  #     """
  #   When I go to line "2"
  #   When I go to end of line
  #   When I type "/"
  #   Then I should see:
  #     """
  #     {
  #       //
  #     }
  #     """


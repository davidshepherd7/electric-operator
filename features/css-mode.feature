Feature: CSS mode
  Background:
    When the buffer is empty
    When I turn on css-mode
    When I turn on electric-operator-mode

  Scenario: colon for psuedo classes
    When I type "button:hover {"
    Then I should see "button:hover {"


  Scenario: colon for properties
    When I insert:
    """
    button.foo {

    }
    """
    When I go to line "2"
    When I type "text-decoration:underline;"
    Then I should see "text-decoration: underline;"

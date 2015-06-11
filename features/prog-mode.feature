Feature: Generic programming mode spacing
  Background:
    When I turn on prog-mode
    When I turn on electric-spacing-mode
    When the buffer is empty

  Scenario: Space = operator
    When I type "a=b"
    Then I should see "a = b"

  Scenario: Space an operator with spacing 'after
    When I type "f(a,b)"
    Then I should see "f(a, b)"

  Scenario: Space an operator with spacing 'before
    # There are no pre-spaced operators in default mode, so use c-mode
    When I turn on c-mode
    When I turn on electric-spacing-mode
    When I type "char*a"
    Then I should see "char *a"

  Scenario: Space a double character operator
    When I type "a==b"
    Then I should see "a == b"

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


Feature: Digraphs
  Background:
    When I turn on prog-mode
    When I turn on electric-spacing-mode
    When the buffer is empty

  Scenario: Don't space ! operator
    When I type "if(!b)"
    Then I should see "if(!b)"

  Scenario: Space != as single operator
    When I type "a!=b"
    Then I should see "a != b"

  Scenario: But don't space =! as a single operator
    When I type "a=!b"
    Then I should see "a = !b"

  Scenario: Don't space =* as single operator
    # Interesting because * and = are both operators
    When I type "a=*b"
    Then I should see "a = * b"

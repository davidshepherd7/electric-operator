Feature: Digraphs
  Background:
    When I turn on prog-mode
    When I turn on electric-operator-mode
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



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


Feature: Decimal numbers
  Background:
    When I turn on prog-mode
    When I turn on electric-spacing-mode
    When the buffer is empty

  Scenario: Don't space decimals
    When I type "0.235"
    Then I should see "0.235"


Feature: Negative exponents
  Background:
    When I turn on prog-mode
    When I turn on electric-spacing-mode
    When the buffer is empty

  Scenario: Space - operator
    When I type "e-b"
    Then I should see "e - b"

  Scenario: Don't space negative exponent (lower case)
    When I type "1.2e-10"
    Then I should see "1.2e-10"

  Scenario: Don't space negative exponent (upper case)
    When I type "1.2E-10"
    Then I should see "1.2E-10"

  Scenario: Don't space negative exponent (integer)
    When I type "5e-10"
    Then I should see "5e-10"


# TODO:
# Feature: Negative numbers
#   Background:
#     When I turn on prog-mode
#     When I turn on electric-spacing-mode
#     When the buffer is empty

#   Scenario: Space - operator
#     When I type "e-b"
#     Then I should see "e - b"

#   Scenario: Don't space -1
#     When I type "-1"
#     Then I should see "-1"

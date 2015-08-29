Feature: Increment and decrement operators
  Background:
    When the buffer is empty

  Scenario: Post-increment
    When I type "b=c+a++"
    Then I should see "b = c + a++"

  Scenario: Pre-increment
    When I type "b=++a+c"
    Then I should see "b = ++a + c"

  Scenario: Post-decrement
    When I type "b=c-a--"
    Then I should see "b = c - a--"

  Scenario: Pre-decrement
    When I type "b=--a-c"
    Then I should see "b = --a - c"

  Scenario: Post-increment with semi-colon
    When I type "a++;"
    Then I should see "a++;"

  Scenario: Post-decrement with semi-colon
    When I type "a--;"
    Then I should see "a--;"

  # It might be possible to handle some of these without brackets. There's
  # also cases like `a++ + b` vs `a + ++b`, is this possible to parse?



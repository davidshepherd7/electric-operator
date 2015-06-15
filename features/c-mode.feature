Feature: -> operator
  Background:
    When the buffer is empty
    When I turn on c-mode
    When I turn on electric-spacing-mode

  Scenario: Space >
    When I type "a>b"
    Then I should see "a > b"

  Scenario: Space -
    When I type "a-b"
    Then I should see "a - b"

  Scenario: Don't space ->
    When I type "a->b"
    Then I should see "a->b"


Feature: #include directives
  Background:
    When the buffer is empty
    When I turn on c-mode
    When I turn on electric-spacing-mode

  Scenario: Include statement
    When I type "#include<stdio.h>"
    Then I should see "#include <stdio.h>"

  Scenario: Include statement with spaces
    When I type "# include<stdio.h>"
    Then I should see "# include <stdio.h>"


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

  # It might be possible to handle some of these without brackets. There's
  # also cases like `a++ + b` vs `a + ++b`, is this possible to parse?


Feature: Ternary operator
    Background:
      When the buffer is empty
      When I turn on c-mode
      When I turn on electric-spacing-mode

    Scenario: Ternary operator
      When I type "a?b:c"
      Then I should see "a ? b : c"

    Scenario: Label
      When I type "error:"
      Then I should see "error:"


Feature: * operator
      Background:
      When the buffer is empty
      When I turn on c-mode
      When I turn on electric-spacing-mode

    Scenario: Multiplication
      When I type "a*b"
      Then I should see "a * b"

    Scenario: Pointer type
      When I type "char*a"
      Then I should see "char *a"

    Scenario: Assign pointer dereference
      When I type "a=*b"
      Then I should see "a = *b"

    Scenario: Pointer dereference and increment
      When I type "*p++"
      Then I should see "*p++"

    Scenario: Function call with dereference
      Then I type "f(*p,*q)"
      Then I should see "f(*p, *q)"


Feature: & operator
      Background:
      When the buffer is empty
      When I turn on c-mode
      When I turn on electric-spacing-mode

    Scenario: Bitwise and
      When I type "a&b"
      Then I should see "a & b"

    Scenario: Reference type
      When I type "char&a"
      Then I should see "char &a"

    Scenario: Assign address of
      When I type "a=&b"
      Then I should see "a = &b"

    Scenario: Function call with address of
      Then I type "f(&p,&q)"
      Then I should see "f(&p, &q)"


Feature: Pointer to pointer type
  Background:
      When the buffer is empty
      When I turn on c-mode
      When I turn on electric-spacing-mode

  Scenario: pointer to pointer type
    When I type "char**a"
    Then I should see "char **a"

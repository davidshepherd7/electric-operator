Feature: C basic operators
  Background:
    When I turn on c-mode
    When I turn on electric-operator-mode
    When I'm inside C main

  # Some simple cases
  Scenario: Ternary operator
    When I type "a?b:c"
    Then I should see "a ? b : c"

  Scenario: Label
    When I type "error:"
    Then I should see "error:"


  # Pointer dereference
  Scenario: Space >
    When I type "a>b"
    Then I should see "a > b"

  Scenario: Space -
    When I type "a-b"
    Then I should see "a - b"

  Scenario: Don't space ->
    When I type "a->b"
    Then I should see "a->b"


  # Increment/decrement
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


  # * operator
  Scenario: Multiplication
    When I type "a*b"
    Then I should see "a * b"

  Scenario: Pointer type
    When I type "char*a"
    Then I should see "char *a"

  Scenario: Non-builtin pointer type
    When I type "size_t*a"
    Then I should see "size_t *a"

  Scenario: Assign pointer dereference
    When I type "a=*b"
    Then I should see "a = *b"

  Scenario: Pointer dereference and increment
    When I type "*p++"
    Then I should see "*p++"

  Scenario: Function call with dereference
    Then I type "f(*p,*q)"
    Then I should see "f(*p, *q)"

  Scenario: pointer to pointer type
      When I type "char**a"
      Then I should see "char **a"


  # & operator
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


  # Respect option to have pointer operators touching the type or the
  # variable name
  Scenario: Operator * on type
    When I set electric-operator-c-pointer-type-style to type
    When I type "int*x"
    Then I should see "int* x"

  Scenario: Operator * on variable
    When I set electric-operator-c-pointer-type-style to variable
    When I type "int*x"
    Then I should see "int *x"

  Scenario: Operator & on type
    When I set electric-operator-c-pointer-type-style to type
    When I type "int&x"
    Then I should see "int& x"

  Scenario: Operator & on variable
    When I set electric-operator-c-pointer-type-style to variable
    When I type "int&x"
    Then I should see "int &x"

  Scenario: Operator ** on type
    When I set electric-operator-c-pointer-type-style to type
    When I type "int**x"
    Then I should see "int** x"

  Scenario: Operator ** on variable
    When I set electric-operator-c-pointer-type-style to variable
    When I type "int**x"
    Then I should see "int **x"


  # Comments
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

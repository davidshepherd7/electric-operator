Feature: -> operator
  Background:
    When the buffer is empty
    When I turn on c-mode
    When I turn on electric-operator-mode

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
    When I turn on electric-operator-mode

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

  Scenario: Post-increment with semi-colon
    When I type "a++;"
    Then I should see "a++;"

  Scenario: Post-decrement with semi-colon
    When I type "a--;"
    Then I should see "a--;"

  # It might be possible to handle some of these without brackets. There's
  # also cases like `a++ + b` vs `a + ++b`, is this possible to parse?


Feature: Ternary operator
    Background:
      When the buffer is empty
      When I turn on c-mode
      When I turn on electric-operator-mode

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
      When I turn on electric-operator-mode

    Scenario: Multiplication
      # Some of cc-mode's syntatic analysis stuff requires us to be inside
      # a function to properly detect that we aren't declaring a function.
      When I insert:
      """
      int main() {

      }
      """
      When I go to line "2"
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


Feature: & operator
  Background:
      When the buffer is empty
      When I turn on c-mode
      When I turn on electric-operator-mode

    Scenario: Bitwise and
      When I insert:
      """
      int main() {

      }
      """
      When I go to line "2"
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
      When I turn on electric-operator-mode

  Scenario: pointer to pointer type
    When I type "char**a"
    Then I should see "char **a"


Feature: Option to have * and & touching next to the type
  Background:
      When the buffer is empty
      When I turn on c-mode
      When I turn on electric-operator-mode

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

Feature: Function declaration pointer/reference types
  Background:
    When the buffer is empty
    When I turn on c-mode
    When I turn on electric-operator-mode

  Scenario: Handle pointer type in function decl.
    When I type "void foo(my_struct*s"
    Then I should see "void foo(my_struct *s"

  Scenario: Handle reference type in function decl.
    When I type "void foo(my_struct&s"
    Then I should see "void foo(my_struct &s"

  Scenario: Handle pointer types with bracket on new lines
    When I insert:
    """
    void foo
    (
    """
    When I type "my_struct*s"
    Then I should see "my_struct *s"

  Scenario: Handle pointer types with argument on new lines
    When I insert:
    """
    void foo(

    """
    When I type "my_struct*s"
    Then I should see "my_struct *s"

  Scenario: Handle pointer types with arguments continued on new line
    When I insert:
    """
    void foo(int a,

    """
    When I type "my_struct*s"
    Then I should see "my_struct *s"

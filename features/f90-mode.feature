Feature: F90 mode basics
  Background:
    When the buffer is empty
    When I turn on f90-mode
    When I turn on electric-operator-mode

  Scenario: don't modify string literal
    When I set electric-operator-enable-in-docs to nil
    When I type "'var+foo-1'"
    Then I should see "'var+foo-1'"

  Scenario: don't modify string literal after operator
    When I type "a+''"
    Then I should see "a + ''"

  ## * and ** operators
  Scenario: Space *
    When I type "a*b"
    Then I should see "a * b"

  Scenario: Multiplication after a function
    When I type "f(x)*a"
    Then I should see "f(x) * a"

  Scenario: Exponentiation after a function
    When I type "f(x)**a"
    Then I should see "f(x) ** a"

  Scenario: Write statements with default formatting and default output
    When I type "write(*,*)"
    Then I should see "write(*, *)"

  Scenario: Write statements with custom formatting and default output
    When I type "write(*, '(format)')"
    Then I should see "write(*, '(format)')"

  Scenario: Write statements with custom formatting and default output
    When I type "write(101,*)"
    Then I should see "write(101, *)"

  Scenario: Print statements with default formatting
    When I type "print*,"
    Then I should see "print *,"


  ## Variable declaration
  Scenario: Declaring a variable
    When I type "type::A"
    Then I should see "type :: A"


  ## Custom operators with .operator. syntax
  @known-failure
  Scenario: Using a generic operator
    When I type "a.operator_123.b"
    Then I should see "a .operator_123. b"

  Scenario: Number preceeding an operator
    When I type "1.eq.a"
    Then I should see "1 .eq. a"

  ## Keyword argument =
  Scenario: Space standard assignment as normal
    When I type "a=b"
    Then I should see "a = b"

  Scenario: Don't space assignment inside function call
    When I type "f(a=b)"
    Then I should see "f(a=b)"


  ## Implicit array declaration
  Scenario: Dividing numbers as usual
    When I type "1/2"
    Then I should see "1 / 2"

  Scenario: Single line array declaration
    When I type "(/1,2,3/)"
    Then I should see "(/ 1, 2, 3 /)"

  @known-failure
  Scenario: Single line array declaration with fraction
    When I type "(/1,2/5,3/)"
    Then I should see "(/ 1, 2 / 5, 3 /)"

  Scenario: Multi-line array declaration
    When I insert:
      """
      (/1, 2, 3, &

      """
    When I type "4,5,6/)"
    Then I should see:
      """
      (/1, 2, 3, &
      4, 5, 6 /)
      """

Feature: C++ specific operators
  Background:
    When the buffer is empty
    When I turn on c++-mode
    When I turn on electric-operator-mode


  # Move constructor type
  Scenario: and operator still works
    When I'm inside C main
    When I type "x=a&&b"
    Then I should see "a && b"

  Scenario: Move constructor works
    When I type "A(A&&a)"
    Then I should see "A(A &&a)"


  # Type detection works inside classes
  Scenario: Move constructor
    When I insert:
    """
    struct A {

    }
    """
    When I go to line "2"
    When I type "A(A&&a)"
    Then I should see "A(A &&a)"


  # Templates are not spaced like gt/lt
  Scenario: Greater than still works
    When I'm inside C main
    When I type "bool x=0>1"
    Then I should see "bool x = 0 > 1"

  Scenario: Less than still works
    When I'm inside C main
    When I type "bool x=0<1"
    Then I should see "bool x = 0 < 1"

  Scenario: Template in function rvalue
    When I type "MyType<double> f()"
    Then I should see "MyType<double> f()"

  Scenario: Template in function argument
    When I type "void f(MyType<double> x)"
    Then I should see "void f(MyType<double> x)"

  Scenario: Nested template in function argument
    When I type "void f(MyType<d<int>> x)"
    Then I should see "void f(MyType<d<int>> x)"


  Scenario: Automatic type references
    When I type "auto&thing"
    Then I should see "auto &thing"

  # TODO

  # Scenario: Template type definition
  #   When I'm inside C main
  #   When I type "MyType<double> x"
  #   Then I should see "MyType<double> x"

  # Similarly for nested templates

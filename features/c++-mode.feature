# Most C++ features are covered by c-mode

Feature: Move constructor type
  Background:
    When the buffer is empty
    When I turn on c++-mode
    When I turn on electric-operator-mode

  Scenario: and operator works
    When I insert:
    """
    int main() {

    }
    """
    When I go to line "2"
    When I type "x=a&&b"
    Then I should see "a && b"

  Scenario: Move constructor works
    When I type "A(A&&a)"
    Then I should see "A(A &&a)"


Feature: Type detection works inside classes
  Background:
    When the buffer is empty
    When I turn on c++-mode
    When I turn on electric-operator-mode

  Scenario: Move constructor
    When I insert:
    """
    struct A {

    }
    """
    When I go to line "2"
    When I type "A(A&&a)"
    Then I should see "A(A &&a)"


Feature: Templates are not spaced like gt/lt
  Background:
    When the buffer is empty
    When I turn on c++-mode
    When I turn on electric-operator-mode

  Scenario: Greater than still works
    When I insert:
    """
    int main() {

    }
    """
    When I go to line "2"
    When I type "bool x=0>1"
    Then I should see "bool x = 0 > 1"

  Scenario: Less than still works
    When I insert:
    """
    int main() {

    }
    """
    When I go to line "2"
    When I type "bool x=0<1"
    Then I should see "bool x = 0 < 1"

  Scenario: Template in function rvalue
    When I type "MyType<double> f()"
    Then I should see "MyType<double> f()"

  Scenario: Template in function argument
    When I type "void f(MyType<double> x)"
    Then I should see "void f(MyType<double> x)"

  # TODO

  # Scenario: Template type definition
  #   When I insert:
  #   """
  #   int main() {

  #   }
  #   """
  #   When I go to line "2"
  #   When I type "MyType<double> x"
  #   Then I should see "MyType<double> x"

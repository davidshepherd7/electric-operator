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

  Scenario: Nested template in function argument
    When I type "void f(MyType<d<int>> x)"
    Then I should see "void f(MyType<d<int>> x)"

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

  # Similarly for nested templates

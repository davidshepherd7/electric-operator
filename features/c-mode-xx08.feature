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



Feature: R specific operators
  Background:
    When the buffer is empty
    # Can't enable ess-r-mode here because it ignores ess-r-smart-operators once
    # it is on :(

  Scenario: dot-tilde operator
    When I turn on ess-r-mode
    When I turn on electric-operator-mode
    When I type "Species~."
    Then I should see "Species ~ ."

  Scenario: Interaction with ess-smart-comma when enabled
    When I set ess-r-smart-operators to t
    When I turn on ess-r-mode
    When I turn on electric-operator-mode
    When I type "f(x,y)"
    Then I should see "f(x, y)"

  Scenario: Interaction with ess-smart-comma when disabled
    When I set ess-r-smart-operators to nil
    When I turn on ess-r-mode
    When I turn on electric-operator-mode
    When I type "f(x,y)"
    Then I should see "f(x, y)"

  Scenario: Interaction with ess-smart-command when disabled and mode disabled
    When I turn off minor mode electric-operator-mode
    When I set ess-r-smart-operators to nil
    When I turn on ess-r-mode
    When I type "f(x,y)"
    Then I should see "f(x,y)"

  Scenario: Spaced equals for keyword args
    When I turn on ess-r-mode
    When I turn on electric-operator-mode
    When I set electric-operator-R-named-argument-style to spaced
    When I type "somefunc(a=1, b=2)"
    Then I should see "somefunc(a = 1, b = 2)"

  Scenario: Equals for keyword args
    When I turn on ess-r-mode
    When I turn on electric-operator-mode
    When I set electric-operator-R-named-argument-style to unspaced
    When I type "somefunc(a=1, b=2)"
    Then I should see "somefunc(a=1, b=2)"

  Scenario: Lone dot does not force unary operators
    When I turn on ess-r-mode
    When I turn on electric-operator-mode
    When I type "x~.+1"
    Then I should see "x ~ . + 1"

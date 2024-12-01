Feature: Ruby mode
  Background:
    When the buffer is empty
    When I turn on ruby-mode
    When I turn on electric-operator-mode
    # For some reason this is extremely slow in tests, like >1 second per |
    # character that it matches
    When I turn off smie-blink-matching-open

  Scenario: don't modify string literal
    When I set electric-operator-enable-in-docs to nil
    When I type "'var+foo-1'"
    Then I should see "'var+foo-1'"

  Scenario: block params with {}
    When I type "|foo arr.map { |a|a }"
    Then I should see "arr.map { |a| a }"

  Scenario: block params with do
    When I type "arr.map do|a|a end"
    Then I should see "arr.map do |a| a end"

  Scenario: Normal | usage
    When I type "3|4"
    Then I should see "3 | 4"
    
  # Check *args works ok
  Scenario: Space *args on its own
    When I type "f(*args)"
    Then I should see "f(*args)"

  Scenario: Space *args with other args
    When I type "f(a,*args)"
    Then I should see "f(a, *args)"

  Scenario: Space *args with a newline before it
    When I insert:
    """
    f(a,

    """
    When I type "*args)"
    Then I should see:
    """
    f(a,
    *args)
    """

  # And **kwargs
  Scenario: Space **kwargs on its own
    When I type "f(**kwargs)"
    Then I should see "f(**kwargs)"

  Scenario: Space **kwargs with other args
    When I type "f(a,**kwargs)"
    Then I should see "f(a, **kwargs)"

  Scenario: Space **kwargs with a newline before it
    When I insert:
    """
    f(a,

    """
    When I type "**kwargs)"
    Then I should see:
    """
    f(a,
    **kwargs)
    """

  Scenario: Percentage literals
    When I insert "%[ship good code]"
    Then I should see "%[ship good code]"
  
  # Unfortunately eckues doesn't handle % properly so we can't test the modulo
  # operator


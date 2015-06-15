Feature: Python mode basics
  Background:
    When the buffer is empty
    When I turn on python-mode
    When I turn on electric-spacing-mode

  Scenario: Enable electric spacing
    Then electric-spacing-mode is on

  Scenario: Space + operator
    When I type "a+b"
    Then I should see "a + b"


Feature: python *args and **kwargs
  Background:
    When the buffer is empty
    When I turn on python-mode
    When I turn on electric-spacing-mode

  # Make sure we haven't messed up normal multiplication or exponentiation
  Scenario: Space *
    When I type "a*b"
    Then I should see "a * b"

  Scenario: Space * inside function
    When I type "f(a*b)"
    Then I should see "f(a * b)"

  Scenario: Space **
    When I type "a**b"
    Then I should see "a ** b"

  Scenario: Space ** inside function
    When I type "f(a**b)"
    Then I should see "f(a ** b)"

  # Check *args works ok
  Scenario: Space *args on its own
    When I type "f(*args)"
    Then I should see "f(*args)"

  Scenario: Space *args with other args
    When I type "f(a,*args)"
    Then I should see "f(a, *args)"

  # And **kwargs
  Scenario: Space **kwargs on its own
    When I type "f(**kwargs)"
    Then I should see "f(**kwargs)"

  Scenario: Space **kwargs with other args
    When I type "f(a,**kwargs)"
    Then I should see "f(a, **kwargs)"

  # TODO: figure out how to check cases where there is a newline, e.g. f(a,b,\n*args)


Feature: python dictionaries
  Background:
    When the buffer is empty
    When I turn on python-mode
    When I turn on electric-spacing-mode

  Scenario: Don't space : in most cases
    When I type "if:"
    Then I should see "if:"

  Scenario: Space after : in dict
    When I type "{a:1}"
    Then I should see "{a: 1}"

  # TODO: maybe check more convoluted nesting? dicts inside fucntions?
  # commas? newlines? lists? These are probably automatically ok with the
  # current implementation though.


Feature: Member access
  Background:
    When the buffer is empty
    When I turn on python-mode
    When I turn on electric-spacing-mode

  Scenario: Don't space accessing class members
    When I type "my_class.a"
    Then I should see "my_class.a"


Feature: Keyword argument =
  Background:
    When the buffer is empty
    When I turn on python-mode
    When I turn on electric-spacing-mode

  Scenario: Space standard assignment as normal
    When I type "a=b"
    Then I should see "a = b"

  Scenario: Don't space assignment inside function call
    When I type "f(a=b)"
    Then I should see "f(a=b)"

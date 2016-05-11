Feature: Python mode basics
  Background:
    When the buffer is empty
    When I turn on python-mode
    When I turn on electric-operator-mode


  # *args and **kwargs
  Scenario: Space *
    When I type "a*b"
    Then I should see "a * b"

  Scenario: Multiplication after a function
    When I type "f(x)*a"
    Then I should see "f(x) * a"

  Scenario: Exponentiation after a function
    When I type "f(x)**a"
    Then I should see "f(x) ** a"

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


  # python dictionaries
  Scenario: Don't space : in most cases
    When I type "if:"
    Then I should see "if:"

  Scenario: Space after : in dict
    When I type "{a:1}"
    Then I should see "{a: 1}"

  # Lambda functions
  Scenario: Space after lambda arguments
    When I type "lambda x:x"
    Then I should see "lambda x: x"

  Scenario: Space after lambda arguments inside dict
    When I type "{a:lambda x:x, b:2}"
    Then I should see "{a: lambda x: x, b: 2}"

  Scenario: Lambda containing dict
    When I type "lambda x:{a:x, b:2}"
    Then I should see "lambda x: {a: x, b: 2}"

  Scenario: Lambda containing slice
    When I type "lambda x:x[1:2]"
    Then I should see "lambda x: x[1:2]"

  @known-failure
  Scenario: Lambda with default argument containing dict
    When I type "lambda x={a:1}:print x"
    Then I should see "lambda x={a: 1}: print x"

  @known-failure
  Scenario: Lambda with default argument containing slice
    When I type "lambda x=y[1:5]:print x"
    Then I should see "lambda x=y[1:5]: print x"


  # Member access
  Scenario: Don't space accessing class members
    When I type "my_class.a"
    Then I should see "my_class.a"


  # Keyword argument =
  Scenario: Space standard assignment as normal
    When I type "a=b"
    Then I should see "a = b"

  Scenario: Don't space assignment inside function call
    When I type "f(a=b)"
    Then I should see "f(a=b)"

  Scenario: Don't space default args in lambda
    When I type "lambda x=1: print x"
    Then I should see "lambda x=1: print x"


  # Slice operator
  Scenario: Don't space : inside slices
    When I type "a[1:2]"
    Then I should see "a[1:2]"

  Scenario: Don't space negative slices
    When I type "a[-1:-2]"
    Then I should see "a[-1:-2]"

Feature: Negative numbers
  Background:
    When I turn on prog-mode
    When I turn on electric-operator-mode
    When the buffer is empty

  Scenario: Space - operator
    When I type "e-b"
    Then I should see "e - b"

  Scenario: Don't space -1
    When I type "-1"
    Then I should see "-1"

  Scenario: a = -1
    When I type "a=-1"
    Then I should see "a = -1"

  Scenario: a * -1
    When I type "a*-1"
    Then I should see "a * -1"

  Scenario: a + -1
    When I type "a+-1"
    Then I should see "a + -1"

  Scenario: a - -1
    When I type "a--1"
    Then I should see "a - -1"

  Scenario: a / -1
    When I type "a/-1"
    Then I should see "a / -1"

  Scenario: a ^ -1
    When I type "a^-1"
    Then I should see "a ^ -1"

  Scenario: a < -1
    When I type "a<-1"
    Then I should see "a < -1"

  Scenario: a > -1
    When I type "a>-1"
    Then I should see "a > -1"

  Scenario: a = [-1]
    When I type "a=[-1]"
    Then I should see "a = [-1]"

  Scenario: f(-1)
    When I type "f(-1)"
    Then I should see "f(-1)"

  Scenario: f(x, -1)
    When I type "f(x,-1)"
    Then I should see "f(x, -1)"

  Scenario: return
    When I type "return -1"
    Then I should see "return -1"

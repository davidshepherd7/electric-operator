Feature: Double spacing when requested
  Background:
    When the buffer is empty
    When I turn on text-mode
    When I turn on electric-operator-mode
    When I set electric-operator-enable-in-docs to t

  Scenario: Single space '.'
    When I set electric-operator-double-space-docs to nil
    When I type "hello.World"
    Then I should see "hello. World"

  Scenario: Double space '.'
    When I set electric-operator-double-space-docs to t
    When I type "hello.World"
    Then I should see "hello.  World"

  Scenario: Custom rules are applied
    When I add a custom rule "-" "-  " to electric-operator-prose-rules
    When I type "hello-world"
    Then I should see "hello-  world"

  Scenario: Typing ',' before an empty line doesn't ruin everything
    When I insert:
    """
    header1:
    1. abcde
    2. arosietn
    3. asdf

    compile and install:
    """
    When I place the cursor after "asdf"
    When I type ","
    Then I should see pattern "^3\. asdf, $"

Feature: Never space % in strings
  Background:
    When the buffer is empty
    When I set electric-spacing-docs to t

  # Unfortunately this is currently untestable with ecukes: % signs can't
  # appear in steps (see bug #58)

  # Scenario: Normally space %
  #   When I type "a%b"
  #   Then I should see "a % b"

  # Scenario: C mode format string
  #   When I turn on c-mode
  #   When I turn on electric-spacing-mode
  #   When I type "printf('an integer \%i'"
  #   Then I should see "printf('an integer \%i'"

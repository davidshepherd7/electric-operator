Feature: #include directives
  Background:
    When the buffer is empty
    When I turn on c-mode
    When I turn on electric-operator-mode

  Scenario: Include statement
    When I type "#include<stdio.h>"
    Then I should see "#include <stdio.h>"

  Scenario: Include statement with spaces
    When I type "# include<stdio.h>"
    Then I should see "# include <stdio.h>"



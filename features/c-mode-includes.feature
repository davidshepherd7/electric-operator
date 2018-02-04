Feature: #include directives
  Background:
    When the buffer is empty
    When I turn on c-mode
    When I turn on electric-operator-mode

  Scenario: Include statement with angle brackets
    When I type "#include<stdio.h>"
    Then I should see "#include <stdio.h>"

  Scenario: Include statement with double quotes
    When I type "#include"stdio.h""
    Then I should see "#include "stdio.h""

  Scenario: Include statement with spaces
    When I type "# include<stdio.h>"
    Then I should see "# include <stdio.h>"

  Scenario: Include statement with path inside angle brackets
    When I type "#include<old/stdio.h>"
    Then I should see "#include <old/stdio.h>"

  Scenario: Include statement with dash inside angle brackets
    When I type "#include<some-file.h>"
    Then I should see "#include <some-file.h>"

  Scenario: Include statement with dash inside quotes
    When I type "#include"some-file.h""
    Then I should see "#include "some-file.h""

  Scenario: Includes with angle brackets and symbols
    When I type "#include <bits/stdc++.h>"
    Then I should see "#include <bits/stdc++.h>"

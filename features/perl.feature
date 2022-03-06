Feature: Perl mode
  Background:
    When the buffer is empty
    When I turn on perl-mode
    When I turn on electric-operator-mode

  Scenario: null filehandle is left alone
    When I type "while (<>) {"
    Then I should see "while (<>) {"

  Scenario: foo x= is  left alone
    When I type "$foo x="
    Then I should see "$foo x="

  Scenario: foox = is also left alone
    When I type "$foox ="
    Then I should see "$foox ="

  Scenario: foox== is handled
    When I type "$foox=="
    Then I should see "$foox =="

  Scenario: pre-increment assignment
    When I type "$a=++$foo"
    Then I should see "$a = ++$foo"

  Scenario: pre-increment within parens
    When I type "while (++$foo < 2)"
    Then I should see "while (++$foo < 2)"

  Scenario: post-increment
    When I type "$a = $foo++"
    Then I should see "$a = $foo++"

  Scenario: Regexes are left alone
    When I type "/foo/bar"
    Then I should see "/foo/bar"

  # This is probably impossible without writing a full on perl parser?
  @known-failure
  Scenario: Division
    When I type "$a/3"
    Then I should see "$a / 3"

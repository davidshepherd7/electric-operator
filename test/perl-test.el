;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'electric-operator)
(require 'test-helper)

(ert-deftest null-filehandle-is-left-alone ()
  (th-fixtures #'perl-mode
    (th-type "while (<>) {")
    (th-should-see "while (<>) {")
    ))

(ert-deftest foo-x-equals-is-left-alone ()
  (th-fixtures #'perl-mode
    (th-type "$foo x=")
    (th-should-see "$foo x=")
    ))

(ert-deftest foox-equals-is-also-left-alone ()
  (th-fixtures #'perl-mode
    (th-type "$foox =")
    (th-should-see "$foox =")
    ))

(ert-deftest foox-double-equals-is-handled ()
  (th-fixtures #'perl-mode
    (th-type "$foox==")
    (th-should-see "$foox ==")
    ))

(ert-deftest pre-increment-assignment ()
  (th-fixtures #'perl-mode
    (th-type "$a=++$foo")
    (th-should-see "$a = ++$foo")
    ))

(ert-deftest pre-increment-within-parens ()
  (th-fixtures #'perl-mode
    (th-type "while (++$foo < 2)")
    (th-should-see "while (++$foo < 2)")
    ))

(ert-deftest post-increment ()
  (th-fixtures #'perl-mode
    (th-type "$a = $foo++")
    (th-should-see "$a = $foo++")
    ))

(ert-deftest regexes-are-left-alone ()
  (th-fixtures #'perl-mode
    (th-type "/foo/bar")
    (th-should-see "/foo/bar")
    ))

(ert-deftest division ()
  :expected-result :failed
  (th-fixtures #'perl-mode
    (th-type "$a/3")
    (th-should-see "$a / 3")
    ))

;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)

(ert-deftest null-filehandle-is-left-alone ()
  (test-with-mode perl-mode
    (electric-operator-test-type "while (<>) {")
    (electric-operator-test-should-see "while (<>) {")
    ))

(ert-deftest foo-x-equals-is-left-alone ()
  (test-with-mode perl-mode
    (electric-operator-test-type "$foo x=")
    (electric-operator-test-should-see "$foo x=")
    ))

(ert-deftest foox-equals-is-also-left-alone ()
  (test-with-mode perl-mode
    (electric-operator-test-type "$foox =")
    (electric-operator-test-should-see "$foox =")
    ))

(ert-deftest foox-double-equals-is-handled ()
  (test-with-mode perl-mode
    (electric-operator-test-type "$foox==")
    (electric-operator-test-should-see "$foox ==")
    ))

(ert-deftest pre-increment-assignment ()
  (test-with-mode perl-mode
    (electric-operator-test-type "$a=++$foo")
    (electric-operator-test-should-see "$a = ++$foo")
    ))

(ert-deftest pre-increment-within-parens ()
  (test-with-mode perl-mode
    (electric-operator-test-type "while (++$foo < 2)")
    (electric-operator-test-should-see "while (++$foo < 2)")
    ))

(ert-deftest post-increment ()
  (test-with-mode perl-mode
    (electric-operator-test-type "$a = $foo++")
    (electric-operator-test-should-see "$a = $foo++")
    ))

(ert-deftest regexes-are-left-alone ()
  (test-with-mode perl-mode
    (electric-operator-test-type "/foo/bar")
    (electric-operator-test-should-see "/foo/bar")
    ))

;; ;; This is probably impossible without writing a full on perl parser?
;; ;; @known-failure
;; (ert-deftest division ()
;;   (test-with-mode perl-mode
;;     (electric-operator-test-type "$a/3")
;;     (electric-operator-test-should-see "$a / 3")
;;     ))

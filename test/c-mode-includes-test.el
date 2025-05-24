;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)


(ert-deftest include-statement-with-angle-brackets ()
  (test-with-mode c-mode
    (electric-operator-test-type "#include<stdio.h>")
    (electric-operator-test-should-see "#include <stdio.h>")))

(ert-deftest include-statement-with-double-quotes ()
  (test-with-mode c-mode
    (electric-operator-test-type "#include\"stdio.h\"")
    (electric-operator-test-should-see "#include \"stdio.h\"")))

(ert-deftest include-statement-with-spaces ()
  (test-with-mode c-mode
    (electric-operator-test-type "# include<stdio.h>")
    (electric-operator-test-should-see "# include <stdio.h>")))

(ert-deftest include-statement-with-path-inside-angle-brackets ()
  (test-with-mode c-mode
    (electric-operator-test-type "#include<old/stdio.h>")
    (electric-operator-test-should-see "#include <old/stdio.h>")))

(ert-deftest include-statement-with-dash-inside-angle-brackets ()
  (test-with-mode c-mode
    (electric-operator-test-type "#include<some-file.h>")
    (electric-operator-test-should-see "#include <some-file.h>")))

(ert-deftest include-statement-with-dash-inside-quotes ()
  (test-with-mode c-mode
    (electric-operator-test-type "#include\"some-file.h\"")
    (electric-operator-test-should-see "#include \"some-file.h\"")))

(ert-deftest includes-with-angle-brackets-and-symbols ()
  (test-with-mode c-mode
    (electric-operator-test-type "#include <bits/stdc++.h>")
    (electric-operator-test-should-see "#include <bits/stdc++.h>")))

;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'electric-operator)
(require 'cc-mode)
(require 'test-helper)


(ert-deftest include-statement-with-angle-brackets ()
  (th-fixtures #'c-mode
    (th-type "#include<stdio.h>")
    (th-should-see "#include <stdio.h>")))

(ert-deftest include-statement-with-double-quotes ()
  (th-fixtures #'c-mode
    (th-type "#include\"stdio.h\"")
    (th-should-see "#include \"stdio.h\"")))

(ert-deftest include-statement-with-spaces ()
  (th-fixtures #'c-mode
    (th-type "# include<stdio.h>")
    (th-should-see "# include <stdio.h>")))

(ert-deftest include-statement-with-path-inside-angle-brackets ()
  (th-fixtures #'c-mode
    (th-type "#include<old/stdio.h>")
    (th-should-see "#include <old/stdio.h>")))

(ert-deftest include-statement-with-dash-inside-angle-brackets ()
  (th-fixtures #'c-mode
    (th-type "#include<some-file.h>")
    (th-should-see "#include <some-file.h>")))

(ert-deftest include-statement-with-dash-inside-quotes ()
  (th-fixtures #'c-mode
    (th-type "#include\"some-file.h\"")
    (th-should-see "#include \"some-file.h\"")))

(ert-deftest includes-with-angle-brackets-and-symbols ()
  (th-fixtures #'c-mode
    (th-type "#include <bits/stdc++.h>")
    (th-should-see "#include <bits/stdc++.h>")))

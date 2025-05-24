;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)

(ert-deftest css-mode-colon-for-pseudo-classes ()
  (th-fixtures css-mode
    (th-type "button:hover {")
    (th-should-see "button:hover {")))

(ert-deftest css-mode-colon-for-properties ()
  (th-fixtures css-mode
    (insert "button.foo {\n\n}")
    (goto-line 2)
    (th-type "text-decoration:underline;")
    (th-should-see "button.foo {\ntext-decoration: underline;\n}")))

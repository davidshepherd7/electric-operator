;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)
(require 'edmacro)

(ert-deftest undo-undoing-the-electric-expansion-doesnt-undo-previous-text ()
  (th-fixtures prog-mode
    (buffer-enable-undo)
    (th-type "const auto a=")
    (th-should-see "const auto a =")

    ;; Calling undo doesn't seem to work, so fake pressing the actual keys instead
    (execute-kbd-macro (edmacro-parse-keys "C-_"))

    (th-should-see "const auto a=")))

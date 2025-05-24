;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)
(require 'edmacro)

(ert-deftest undo-undoing-the-electric-expansion-doesnt-undo-previous-text ()
  (test-with-mode prog-mode
    (buffer-enable-undo)
    (electric-operator-test-type "const auto a=")
    (electric-operator-test-should-see "const auto a =")

    ;; Calling undo doesn't seem to work, so fake pressing the actual keys instead
    (execute-kbd-macro (edmacro-parse-keys "C-_"))

    (electric-operator-test-should-see "const auto a=")))

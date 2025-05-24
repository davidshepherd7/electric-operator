;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'electric-operator)
(require 'test-helper)

(ert-deftest undo-undoing-the-electric-expansion-doesnt-undo-previous-text ()
  (th-fixtures #'prog-mode
    (buffer-enable-undo)
    (th-type "const auto a=")
    (th-should-see "const auto a =")

    ;; Calling undo doesn't seem to work, so fake pressing the actual keys instead
    (th-press-keys "C-_")

    (th-should-see "const auto a=")))

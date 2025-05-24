(require 'ert)
(require 'edmacro)

;; Don't load old byte-compiled versions!
(setq load-prefer-newer t)

;;; Test helper functions for integration tests

(defmacro th-fixtures (mode &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (save-window-excursion
       (set-window-buffer nil (current-buffer))
       (funcall ,mode)
       (electric-operator-mode 1)
       ,@body)))

(defun th-type (string)
  "Type STRING character by character, simulating user input."
  (dolist (char (string-to-list string))
    (insert char)
    ;; Trigger post-self-insert-hook to simulate real typing
    (run-hooks 'post-self-insert-hook)))

(defun th-should-see (expected)
  "Assert that buffer contents contain EXPECTED."
  (let ((actual (buffer-string))
        (expected-re (regexp-quote expected)))
    (should (string-match-p expected-re actual))))

(defun th-should-see-pattern (pattern)
  "Assert that buffer contents match regex PATTERN."
  (should (string-match-p pattern (buffer-string))))

(defun th-should-not-see (unexpected)
  "Assert that buffer contents do not contain UNEXPECTED."
  (let ((actual (buffer-string))
        (unexpected-re (regexp-quote unexpected)))
    (should-not (string-match-p unexpected-re actual))))

(defun th-press-keys (keystring)
  "Simulate pressing this key combo.

e.g. (th-press-keys \"C-_\")
"
  (execute-kbd-macro (edmacro-parse-keys keystring)))

(defun th-goto-line (line)
  "Normal goto-line is for interactive only, so define our own"
  (goto-char (point-min))
  (forward-line (- line 1)))


(provide 'test-helper)

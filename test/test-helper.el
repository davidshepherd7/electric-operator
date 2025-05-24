(require 'cc-mode)
(require 'ert)
(require 'electric-operator)
(require 'edmacro)

;; Don't load old byte-compiled versions!
(setq load-prefer-newer t)

(require 'f)

(defvar electric-operator-test-path
  (f-dirname load-file-name))

(defvar electric-operator-root-path
  (f-parent electric-operator-test-path))

(add-to-list 'load-path electric-operator-root-path)


(defun log-trace-buffer (&rest _)
  (when (get-buffer trace-buffer)
    (with-current-buffer trace-buffer
      (message (buffer-substring-no-properties (point-min) (point-max))))))
(add-to-list 'ert-runner-reporter-run-ended-functions #'log-trace-buffer)

;;; Test helper functions for integration tests

(defmacro th-fixtures (mode &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (save-window-excursion
       (set-window-buffer nil (current-buffer))
       (funcall ',mode)
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
  (let ((actual (buffer-string)))
    (should (string-match-p (regexp-quote expected) actual))))

(defun th-should-see-pattern (pattern)
  "Assert that buffer contents match regex PATTERN."
  (should (string-match-p pattern (buffer-string))))

(defun th-should-not-see (unexpected)
  "Assert that buffer contents do not contain UNEXPECTED."
  (let ((actual (buffer-string)))
    (should-not (string-match-p (regexp-quote unexpected) actual))))

(defun th-press-keys (keystring)
  "Simulate pressing this key combo.

e.g. (th-press-keys \"C-_\")
"
  (execute-kbd-macro (edmacro-parse-keys keystring)))

(provide 'test-helper)

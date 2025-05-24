(require 'cc-mode)
(require 'ert)
(require 'electric-operator)

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

(defmacro test-with-mode (mode &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (save-window-excursion
       (set-window-buffer nil (current-buffer))
       (funcall ',mode)
       (electric-operator-mode 1)
       ,@body)))

(defun electric-operator-test-type (string)
  "Type STRING character by character, simulating user input."
  (dolist (char (string-to-list string))
    (insert char)
    ;; Trigger post-self-insert-hook to simulate real typing
    (run-hooks 'post-self-insert-hook)))

(defun electric-operator-test-should-see (expected)
  "Assert that buffer contents contain EXPECTED."
  (let ((actual (buffer-string)))
    (should (string-match-p (regexp-quote expected) actual))))

(defun electric-operator-test-should-see-pattern (pattern)
  "Assert that buffer contents match regex PATTERN."
  (should (string-match-p pattern (buffer-string))))

(defun electric-operator-test-should-not-see (unexpected)
  "Assert that buffer contents do not contain UNEXPECTED."
  (let ((actual (buffer-string)))
    (should-not (string-match-p (regexp-quote unexpected) actual))))

(defun electric-operator-test-setup-c-main ()
  "Set up a C main function context."
  (insert "int main() {\n\n}\n")
  (goto-char (point-min))
  (forward-line 1))

(defun electric-operator-test-setup-rust-function ()
  "Set up a Rust function context."
  (insert "fn foo() -> i32 {\n\n}\n")
  (goto-char (point-min))
  (forward-line 1))

(provide 'test-helper)

;;; ert-runner configuration file

;; Actual helper functions for tests are in the root directory.

;; This file doesn't compile: the variable is from ert-runner.el, but can't
;; require it here because it errors. So don't compile this file.



(defun log-trace-buffer (&rest _)
  (when (get-buffer trace-buffer)
    (with-current-buffer trace-buffer
      (message (buffer-substring-no-properties (point-min) (point-max))))))

(add-to-list 'ert-runner-reporter-run-ended-functions #'log-trace-buffer)

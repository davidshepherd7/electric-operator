
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

;; To use this file run: make benchmark
;;
;; Expected result is that electric-operator-post-self-insert-function takes
;; under a second to run 1,000 iterations.

(require 'benchmark)
(require 'elp)
(require 'electric-operator)
(require 'python)


(defun elp-benchmark (repeats &optional setup-fn)
  (interactive)

  ;; Instrument all of the interesting functions
  (elp-instrument-package "electric-operator")
  (elp-instrument-package "electric-operator-")
  (elp-instrument-function #'looking-back)

  ;; Capture perf data on everything inside the self-insert hook
  (elp-set-master #'electric-operator-post-self-insert-function)

  ;; Prevent garbage collection
  (let ((gc-cons-threshold most-positive-fixnum))
    (garbage-collect)

    ;; Run our function a lot
    (dolist (_ (-repeat repeats 0))
      (with-temp-buffer
        (when setup-fn (funcall setup-fn))
        (electric-operator-post-self-insert-function))))

  (elp-results)
  (terpri))

(setq python-indent-guess-indent-offset nil)

(elp-benchmark 1000 (lambda ()
                      (python-mode)
                      (insert "a->")))

(elp-benchmark 1000 (lambda ()
                      (python-mode)
                      (insert "a=")))

(elp-benchmark 1000 (lambda ()
                      (c++-mode)
                      (insert "a=")))

(elp-benchmark 1000 (lambda ()
                      (c++-mode)
                      (insert "aaorsitenar")))











;; (defun do-pair ()

;;   (electric-operator-mode 1)
;;   (let ((with (do-benchmark)))

;;     (electric-operator-mode 0)
;;     (let ((without (do-benchmark)))


;;       (message "with: %S" with)
;;       (message "without: %S" without)
;;       (message "diffs: %s" (-map (lambda (p) (- (car p) (cdr p))) (-zip with without)))
;;       )))


;; (defun do-benchmarks ()
;;   (byte-compile-file "electric-operator.el")
;;   (load-file "electric-operator.elc")

;;   (python-mode)

;;   (evil-insert-state)

;;   ;; (message "gc off")
;;   ;; (electric-operator-mode 1)
;;   ;; (let ((gc-cons-threshold most-positive-fixnum))
;;   ;;   (do-pair))

;;   (message "gc on")
;;   (do-pair)
;;   )
;; (defun do-benchmark ()
;;   (garbage-collect)
;;   (prog1
;;       (benchmark-run-compiled 100 (execute-kbd-macro (kbd "a = b + c * d / e RET")))
;;     (delete-region (point-min) (point-max))))


;; (defun other-benchmark ()
;;   (interactive)
;;   (with-temp-buffer
;;     (setq python-indent-guess-indent-offset nil)
;;     (python-mode)
;;     (let ((gc-cons-threshold most-positive-fixnum))
;;       (garbage-collect)
;;       (--> (-repeat 20 0)
;;            (--map (benchmark-run-compiled 1000 (progn
;;                                                  (insert "a=")
;;                                                  (electric-operator-post-self-insert-function)
;;                                                  (delete-region 1 (point-max)))) it)
;;            (--min-by (< (car it) (car other)) it)))))

;; (princ (other-benchmark))
;; (terpri)


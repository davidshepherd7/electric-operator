
;; Don't load old byte-compiled versions!
(setq load-prefer-newer t)

(require 'f)

(defvar electric-operator-test-path
  (f-dirname load-file-name))

(defvar electric-operator-root-path
  (f-parent electric-operator-test-path))

(add-to-list 'load-path electric-operator-root-path)

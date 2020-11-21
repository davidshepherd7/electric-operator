(package "electric-operator" "1.0.0" "Automatically add spaces around operators")

(files "electric-operator.el")

;; Tell Cask to add package information to this file (so that we can have a
;; single file package).
(package-file "electric-operator.el")

;; TODO: switch back to melpa stable once ess fixes the "julia mode failed to
;; install" error
(source melpa)
(source gnu)

(development
 (depends-on "ecukes")
 (depends-on "espuds")
 (depends-on "ert-runner")

 ;; For testing
 (depends-on "rust-mode")
 (depends-on "ess")
 (depends-on "js2-mode")
 (depends-on "haskell-mode")
 (depends-on "julia-mode")
 (depends-on "php-mode")
 (depends-on "auctex")
 )

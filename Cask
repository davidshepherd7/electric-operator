(package "electric-operator" "1.0.0" "Automatically add spaces around operators")

(files "electric-operator.el")

;; Tell Cask to add package information to this file (so that we can have a
;; single file package).
(package-file "electric-operator.el")

(source melpa)

(development
 (depends-on "ecukes")
 (depends-on "espuds")

 ;; For testing, not available on melpa stable so pull from github directly
 (depends-on "rust-mode" :git "https://github.com/rust-lang/rust-mode.git")

 ;; For testing
 (depends-on "ess")
 (depends-on "js2-mode" :git "https://github.com/mooz/js2-mode" :ref "c0801b25d6ada38cc93e7a6d33d5de0d1ad5bc1a")
 (depends-on "haskell-mode")
 )

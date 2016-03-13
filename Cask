(package "electric-operator" "0.1" "Automatically add spaces around operators")

(files "electric-operator.el")

;; Tell Cask to add package information to this file (so that we can have a
;; single file package).
(package-file "electric-operator.el")

(source melpa-stable)

(development
 (depends-on "ecukes")
 (depends-on "espuds")

 ;; For testing, not available on melpa stable so pull from github directly
 (depends-on "rust-mode" :git "https://github.com/rust-lang/rust-mode.git")

 ;; For testing, we currently (13/3/2016) need the latest version for the
 ;; autoload fix cdfd70cb7267d2b1777f5e84aecdf44cf75b2a4b, go back to melpa
 ;; stable when possible.
 (depends-on "ess" :git "https://github.com/emacs-ess/ess.git"
             :files
             ("*.el" ("lisp" "lisp/*.el") (:exclude "lisp/ess-pkg.el") ("etc" "etc/*")
              "doc/*.texi" "doc/info/dir" "lisp/ess-pkg.el"))

 )

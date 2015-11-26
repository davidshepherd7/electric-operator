;;; -*- lexical-binding: t; -*-
;;; electric-operator.el --- Automatically add spaces around operators

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: David Shepherd <davidshepherd7@gmail.com>
;; Version: 0.1
;; Package-Requires: ((dash "2.10.0") (names "20150618.0") (emacs "24.4"))
;; Keywords: electric
;; URL: https://github.com/davidshepherd7/electric-operator

;;; Commentary:

;; An emacs minor-mode to automatically add spacing around operators. For
;; example typing `a=10*5+2' results in `a = 10 * 5 + 2'.

;;; Code:

(require 'cc-mode)
(require 'thingatpt)
(require 'subr-x)

(require 'dash)
(require 'names)

;; namespacing using names.el:
;;;###autoload
(define-namespace electric-operator-

;; Tell names that it's ok to expand things inside these threading macros.
:functionlike-macros (-->)



;;; Customisable variables

(defcustom double-space-docs nil
  "Enable double spacing of . in document lines - e,g, type '.' => get '.  '."
  :type 'boolean
  :group 'electricity)

(defcustom enable-in-docs nil
  "Enable electric-operator in strings and comments."
  :type 'boolean
  :group 'electricity)

(defcustom c-pointer-type-style 'variable
  "Defines how C/C++ mode pointer and reference types are spaced.

If set to 'variable then the operator is touching the variable
name, as in `int *x'.

If set to 'type then the operator is touching the type name , as
in `int* x'."
  :group 'electricity
  :options '(variable type))



;;; Other variables

(defvar mode-rules-table
  (make-hash-table)
  "A hash table of replacement rule lists for specific major modes")



;;; Rule list helper functions

(defun -add-rule (initial new-rule)
  "Replace or append a new rule

Returns a modified copy of the rule list."
  (let* ((op (car new-rule))
         (existing-rule (assoc op initial)))
    (if existing-rule
        (-replace existing-rule new-rule initial)
      (-snoc initial new-rule))))

(defun -add-rule-list (initial new-rules)
  "Replace or append a list of rules

Returns a modified copy of the rule list."
  (-reduce #'-add-rule (-concat (list initial) new-rules)))

(defun add-rules (initial &rest new-rules)
  "Replace or append multiple rules

Returns a modified copy of the rule list."
  (-add-rule-list initial new-rules))

(defun get-rules-for-mode (major-mode-symbol)
  "Get the spacing rules for major mode"
  (gethash major-mode-symbol mode-rules-table))

(defun add-rules-for-mode (major-mode-symbol &rest new-rules)
  "Replace or add spacing rules for major mode

Destructively modifies mode-rules-table to use the new rules for
the given major mode."
  (puthash major-mode-symbol
           (-add-rule-list (get-rules-for-mode major-mode-symbol)
                           new-rules)
           mode-rules-table))



;;; Default rule lists

(defvar prog-mode-rules
  (list (cons "=" " = ")
        (cons "<" " < ")
        (cons ">" " > ")
        (cons "%" " % ")
        (cons "+" " + ")
        (cons "-" #'prog-mode--)
        (cons "*" " * ")
        (cons "/" #'prog-mode-/)
        (cons "&" " & ")
        (cons "|" " | ")
        (cons "?" "? ")
        (cons "," ", ")
        (cons "^" " ^ ")

        (cons "==" " == ")
        (cons "!=" " != ")
        (cons "<=" " <= ")
        (cons ">=" " >= ")

        (cons "*=" " *= ")
        (cons "+=" " += ")
        (cons "/=" " /= ")
        (cons "-=" " -= ")

        (cons "&&" " && ")
        (cons "||" " || ")
        )
  "Default spacing rules for programming modes")

(defvar prose-rules
  (add-rules '()
             (cons "." #'docs-.)
             (cons "," ", ")
             )
  "Rules to use in comments, strings and text modes.")



;;; Core functions

(defun get-rules-list ()
  "Pick which rule list is appropriate for spacing at point"
  (cond
   ;; In comment or string?
   ((in-docs?) (if enable-in-docs prose-rules (list)))

   ;; Try to find an entry for this mode in the table
   ((get-rules-for-mode major-mode))

   ;; Default modes
   ((derived-mode-p 'prog-mode) prog-mode-rules)
   (t prose-rules)))

(defun rule-regex-with-whitespace (op)
  "Construct regex matching operator and any whitespace before/inside/after.

For example for the operator '+=' we allow '+=', ' +=', '+ ='. etc.

Whitespace before the operator is captured for possible use later.
"
  (concat "\\(\s*\\)"
          (mapconcat #'regexp-quote (split-string op "" t) "\s*")
          "\s*"))

(defun longest-matching-rule (rule-list)
  "Return the rule with the most characters that applies to text before point"
  (--> rule-list
       (-filter (lambda (rule) (looking-back-locally (rule-regex-with-whitespace (car rule)))) it)
       (-sort (lambda (p1 p2) (> (length (car p1)) (length (car p2)))) it)
       (car it)))

(defun post-self-insert-function ()
  "Check for a matching rule and apply it"
  (-let* ((rule (longest-matching-rule (get-rules-list)))
          ((operator . action) rule))
    (when (and rule action)

      ;; Delete the characters matching this rule before point
      (looking-back-locally (rule-regex-with-whitespace operator) t)
      (let ((pre-whitespace (match-string 1)))
        (delete-region (match-beginning 0) (match-end 0))

        ;; If this is the first thing in a line then restore the
        ;; indentation.
        (if (looking-back-locally "^\s*")
            (insert pre-whitespace))

        ;; Insert correctly spaced operator
        (if (stringp action)
            (insert action)
          (insert (funcall action)))))))

:autoload
(define-minor-mode mode
  "Toggle automatic insertion of spaces around operators (Electric Spacing mode).

With a prefix argument ARG, enable Electric Spacing mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

This is a local minor mode.  When enabled, typing an operator automatically
inserts surrounding spaces, e.g., `=' becomes ` = ',`+=' becomes ` += '."
  :global nil
  :group 'electricity
  :lighter " _+_"

  ;; body
  (if mode
      (add-hook 'post-self-insert-hook
                #'post-self-insert-function nil t)
    (remove-hook 'post-self-insert-hook
                 #'post-self-insert-function t)))



;;; Helper functions

(defun in-docs? ()
  "Check if we are inside a string or comment"
  (nth 8 (syntax-ppss)))

(defun hashbang-line? ()
  "Does the current line contain a UNIX hashbang?"
  (and (eq 1 (line-number-at-pos))
       (save-excursion
         (move-beginning-of-line nil)
         (looking-at "#!"))))

(defun enclosing-paren ()
  "Return the opening parenthesis of the enclosing parens, or nil
if not inside any parens."
  (interactive)
  (let ((ppss (syntax-ppss)))
    (when (nth 1 ppss)
      (char-after (nth 1 ppss)))))

(defun probably-unary-operator? ()
  "Try to guess if the operator we are about to insert will be unary

(i.e. takes one argument). This is a bit of a fudge based on C-like syntax."
  (or
   (looking-back-locally "^")
   (looking-back-locally "[=,:\*\+-/><&^]")
   (looking-back-locally "\\(return\\)")))

(defun just-inside-bracket ()
  (looking-back-locally "[([{]"))

t(defun looking-back-locally (string &optional greedy)
   "A wrapper for looking-back limited to the two previous lines

Apparently looking-back can be slow without a limit, and calling
it without a limit is deprecated.

Any better ideas would be welcomed."
   (let ((two-lines-up (save-excursion
                         (forward-line -2)
                         (beginning-of-line)
                         (point))))
     (looking-back string two-lines-up greedy)))



;;; General tweaks

(defun docs-. ()
  "Double space if setting tells us to"
  (if double-space-docs
      ".  "
    ". "))

(defun prog-mode-- ()
  "Handle exponent and negative number notation"
  (cond
   ;; Exponent notation, e.g. 1e-10: don't space
   ((looking-back-locally "[0-9.]+[eE]") "-")

   ;; Space negative numbers as e.g. a = -1 (but don't space f(-1) or -1
   ;; alone at all). This will proabaly need to be major mode specific
   ;; eventually.
   ((probably-unary-operator?) " -")
   ((just-inside-bracket) "-")

   (t " - ")))

(defun prog-mode-/ ()
  "Handle path separator in UNIX hashbangs"
  ;; First / needs a space before it, rest don't need any spaces
  (cond ((and (hashbang-line?) (looking-back-locally "#!")) " /")
        ((hashbang-line?) "/")
        (t " / ")))



;;; C/C++ mode tweaks

(apply #'add-rules-for-mode 'c-mode prog-mode-rules)
(add-rules-for-mode 'c-mode
                    (cons "->" "->")

                    (cons "/" #'c-mode-/)

                    ;; ternary operator
                    (cons "?" " ? ")
                    (cons ":" #'c-mode-:) ; (or case label)

                    ;; pointers
                    (cons "*" #'c-mode-*)
                    (cons "&" #'c-mode-&)
                    (cons "**" #'c-mode-**) ; pointer-to-pointer type

                    ;; increment/decrement
                    (cons "++" #'c-mode-++)
                    (cons "--" #'c-mode---)

                    ;; #include statements
                    (cons "<" #'c-mode-<)
                    (cons ">" #'c-mode->)

                    ;; bitshift operators
                    (cons "<<" " << ")
                    (cons ">>" " >> ")

                    ;; Comments
                    (cons "/*" "/* ")
                    (cons "//" "// ")

                    ;; End of statement inc/decrement, handled separately
                    ;; because there is no space after the ++/--.
                    (cons "++;" "++;")
                    (cons "--;" "--;")

                    ;; Weirder assignment operators
                    (cons "%=" " %= ")
                    (cons "^=" " ^= ")
                    (cons "&=" " &= ")
                    (cons "|=" " |= ")
                    (cons "<<=" " <<= ")
                    (cons ">>=" " >>= ")

                    )


;; Use the same rules for c++
(apply #'add-rules-for-mode 'c++-mode (get-rules-for-mode 'c-mode))

;; And some extra rules
(add-rules-for-mode 'c++-mode

                    ;; Move constructor or `and' operator
                    (cons "&&" #'c++-mode-&&)

                    ;; Nested templates
                    (cons ">>" #'c++-mode->>)

                    ;; Handle for-each loops as well
                    (cons ":" #'c++-mode-:)

                    ;; Namespaces
                    (cons "::" "::")

                    ;; Lambdas
                    (cons "->" #'c++-mode-->))

;; Construct and add null rules for operator=, operator<< etc.
(--> (get-rules-for-mode 'c++-mode)
     (-map (lambda (p) (cons (concat "operator" (car p)) nil)) it)
     (apply #'add-rules-for-mode 'c++-mode it))


(defvar c-user-types-regex
  "_t"
  "Regex used in looking-back-locally to check for C types

For now we just assume that anything ending in '_t' is a type.
I'm not sure if we can do any better by default.

You could add your own type names to this if needed. Send pull
requests/bug reports if you find any widely used type names that
could be added here.")

(defun c-after-type? ()
  (or
   ;; Check for built-in types
   (looking-back-locally (concat c-primitive-type-key "?"))

   ;; Check if previous word is struct/union/enum keyword
   (looking-back-locally "\\b\\(struct\\|union\\|enum\\|const\\)[[:space:]]+[[:alnum:]\\|_\\|:]+")

   (looking-back-locally "auto")

   ;; Check for any user-defined types
   (looking-back-locally c-user-types-regex)))

(defvar c-function-definition-syntax-list
  '(topmost-intro
    topmost-intro-cont
    arglist-intro
    arglist-cont-nonempty)
  "syntax symbols for lines which contain a function definition

See `c-guess-basic-syntax'.")

(defun c-is-function-or-class-definition? ()
  "Try to guess if we are in function definition/declaration

Using `cc-mode''s syntactic analysis."
  ;; There are similar but different symbols for objective-C, but I'm not
  ;; going to try to support that now.

  (--> (c-guess-basic-syntax)
       (-map #'car it)
       (-intersection c-function-definition-syntax-list it)))

(defun c-mode-include-line? ()
  (looking-back-locally "#\s*include.*"))

(defun c-mode-probably-ternary ()
  (looking-back-locally "\\?.+"))

(defun c-mode-: ()
  "Handle the : part of ternary operator"
  (if (c-mode-probably-ternary)
      " : "
    ":"))

(defun c++-mode-: ()
  "Handle ternary, case, or for each"
  (cond
   ;; The colon in `class Foo : public Bar`
   ((c-is-function-or-class-definition?) " : ")

   ((c-mode-probably-ternary) " : ")

   ;; probably a for-each loop
   ((equal (enclosing-paren) ?\() " : ")

   ;; probably a case statement
   (t ":" )))


(defun c-mode-++ ()
  "Handle ++ operator pre/postfix"
  (if (looking-back-locally "[a-zA-Z0-9_]\s*")
      "++ "
    " ++"))

(defun c-mode--- ()
  "Handle -- operator pre/postfix"
  (if (looking-back-locally "[a-zA-Z0-9_]\s*")
      "-- "
    " --"))

(defun c-mode-< ()
  "Handle #include brackets and templates"
  (cond ((c-mode-include-line?) " <")
        ((c-is-function-or-class-definition?) "<")
        (t " < ")))

(defun c-mode-> ()
  "Handle #include brackets and templates"
  (cond ((c-mode-include-line?) ">")
        ((c-is-function-or-class-definition?) "> ")
        (t " > ")))

(defun c++-mode->> ()
  "Handle nested templates"
  (cond ((c-is-function-or-class-definition?) ">> ")
        (t " >> ")))

(defun c-space-pointer-type (op)
  "Space a C pointer types operator as specified by
  `c-pointer-type-style'.

 For example `int* x'  or `int *x'."
  (cond ((eq c-pointer-type-style  'variable) (concat " " op))
        ((eq c-pointer-type-style 'type) (concat op " "))
        (t (error "Unrecognised value for c-pointer-type-style."))))

(defun c-mode-& ()
  "Handle C address-of operator and reference types"
  (cond
   ;; Reference types
   ((or (c-after-type?) (c-is-function-or-class-definition?))
    (c-space-pointer-type "&"))

   ;; Address-of operator or lambda pass-by-reference specifier
   ((just-inside-bracket) "&")
   ((probably-unary-operator?) " &")

   (t " & ")))

(defun c-mode-* ()
  "Handle C dereference operator and pointer types"
  (cond
   ;; Pointer types
   ((or (c-after-type?) (c-is-function-or-class-definition?))
    (c-space-pointer-type "*"))

   ;; Pointer dereference
   ((just-inside-bracket) "*")
   ((probably-unary-operator?) " *")

   (t " * ")))

(defun c-mode-** ()
  "C pointer to pointer or multiplication by pointer dereference.
  e.g. `res = a * *b;`'"
  (if (c-after-type?)
      (c-space-pointer-type "**")
    " * *"))

(defun c++-mode-&& ()
  "Handle move constructor"
  (if (c-is-function-or-class-definition?)
      (c-space-pointer-type "&&")
    " && "))

(defun c-mode-/ ()
  "Handle / in #include <a/b>"
  (if (c-mode-include-line?) "/" (prog-mode-/)))

(defun c++-probably-lambda-arrow ()
  "Try to guess if we are writing a lambda statement"
  (looking-back-locally "\\[[^]]*\\]\\s-*([^)]*)\\s-*\\(mutable\\)?"))

(defun c++-mode--> ()
  "Handle lambda arrows"
  (if (c++-probably-lambda-arrow)
      " -> "
    "->"))



;;; Python mode tweaks

(apply #'add-rules-for-mode 'python-mode prog-mode-rules)
(add-rules-for-mode 'python-mode
                    (cons "**" #'python-mode-**)
                    (cons "*" #'python-mode-*)
                    (cons ":" #'python-mode-:)
                    (cons "//" " // ") ; integer division
                    (cons "=" #'python-mode-kwargs-=)
                    (cons "-" #'python-mode-negative-slices)
                    )

(defun python-mode-: ()
  "Handle python dict assignment"
  (if (eq (enclosing-paren) ?\{)
      ": "
    ":"))

(defun python-mode-* ()
  "Handle python *args"
  ;; Can only occur after '(' ',' or on a new line, so just check for those.
  ;; If it's just after a comma then also insert a space before the *.
  (cond ((looking-back-locally ",")  " *")
        ((looking-back-locally "[(,][ \t]*")  "*")
        ;; Othewise act as normal
        (t  " * ")))

(defun python-mode-** ()
  "Handle python **kwargs"
  (cond ((looking-back-locally ",") " **")
        ((looking-back-locally "[(,][ \t]*") "**")
        (t " ** ")))

(defun python-mode-kwargs-= ()
  (if (eq (enclosing-paren) ?\()
      "="
    " = "))

(defun python-mode-negative-slices ()
  "Handle cases like a[1:-1], see issue #2."
  (if (and (eq (enclosing-paren) ?\[)
           (looking-back-locally ":"))
      "-"
    (prog-mode--)))



;;; Javascript mode tweaks

(defun js-mode-: ()
  "Handle object assignment and ternary"
  (if (eq (enclosing-paren) ?\{)
      ": "
    " : "))

(puthash 'js-mode
         (add-rules prog-mode-rules
                    (cons "%=" " %= ")
                    (cons "++" "++ ")
                    (cons "--" "-- ")
                    (cons "===" " === ")
                    (cons "!==" " !== ")
                    (cons "<<" " << ")
                    (cons ">>" " >> ")
                    (cons ":" #'js-mode-:)
                    (cons "?" " ? ")
                    )
         mode-rules-table)




;;; Other major mode tweaks

(apply #'add-rules-for-mode 'ruby-mode
       prog-mode-rules)
(add-rules-for-mode 'ruby-mode
                    (cons "=~" " =~ ") ; regex equality
                    )

(apply #'add-rules-for-mode 'perl-mode prog-mode-rules)
(add-rules-for-mode 'perl-mode
                    (cons "=~" " =~ ") ; regex equality
                    )

;; cperl mode is another perl mode, copy the rules
(apply #'add-rules-for-mode 'cperl-mode (get-rules-for-mode 'cperl-mode))

;; This is based on a syntax guide and hasn't been tested.
(apply #'add-rules-for-mode 'java-mode prog-mode-rules)
(add-rules-for-mode 'java-mode

                    ;; ternary operator
                    (cons "?" " ? ")
                    (cons ":" #'c-mode-:) ; (or case label)

                    ;; increment/decrement
                    (cons "++" #'c-mode-++)
                    (cons "--" #'c-mode---)

                    ;; bitshift operators
                    (cons "<<" " << ")
                    (cons ">>" " >> ")
                    (cons ">>>" " >>> ")

                    ;; Weirder assignment operators
                    (cons "%=" " %= ")
                    (cons "^=" " ^= ")
                    (cons "&=" " &= ")
                    (cons "|=" " |= ")
                    (cons "<<=" " <<= ")
                    (cons ">>=" " >>= ")

                    ;; Comments
                    (cons "/*" "/* ")
                    (cons "//" "// ")
                    )

;; Again: based on a syntax guide and not really tested
(apply #'add-rules-for-mode 'haskell-mode prog-mode-rules)
(add-rules-for-mode 'haskell-mode
                    (cons "." " . ") ; function composition
                    (cons "++" " ++ ") ; list concat
                    (cons "!!" " !! ") ; indexing
                    (cons "--" "-- ") ; comment
                    (cons "$" " $ ") ; delay evaluation
                    (cons "<-" " <- ") ; assignment
                    (cons "->" " -> ") ; lambdas and function types
                    (cons ":" nil) ; list constructor
                    (cons "::" " :: ") ; type specification
                    (cons "!=" nil) ; unused
                    (cons ".|." " .|. ") ; bitwise OR
                    (cons ".&." " .&. ") ; bitwise AND

                    ;; Exponents, for some reason there are three of
                    ;; them!
                    (cons "^" " ^ ")
                    (cons "**" " ** ")
                    (cons "^^" " ^^ ")
                    )

;; Integration testing these is hard because ess-mode is not built in to
;; emacs and it's weird (doesn't define autoloads, doesn't inherit from
;; prog-mode, ...).
(apply #'add-rules-for-mode 'ess-mode prog-mode-rules)
(add-rules-for-mode 'ess-mode
                    (cons "." nil) ; word separator
                    (cons "<-" " <- ") ; assignment
                    (cons "->" " -> ") ; Right assignment
                    (cons "%%" " %% ") ; Modulus
                    (cons "%/%" " %/% ") ; Integer divide
                    (cons "%*%" " %*% ") ; Matrix product
                    (cons "%o%" " %o% ") ; Outer product
                    (cons "%x%" " %x% ") ; Kronecker product
                    (cons "%in%" " %in% ") ; Matching operator
                    )

;; ess-mode binds comma to a function, so we need to advise that function
;; to also run our code:
(with-eval-after-load 'ess-mode
  (advice-add 'ess-smart-comma :after #'post-self-insert-function))


(apply #'add-rules-for-mode 'php-mode prog-mode-rules)
(add-rules-for-mode 'php-mode
                    (cons "**" " ** ")
                    (cons "%=" " %= ")
                    (cons "===" " === ")
                    (cons "<>" " <> ") ; not-equal
                    (cons "!==" " !== ")
                    (cons "++" #'c-mode-++)
                    (cons "--" #'c-mode---)
                    (cons "." " . ")   ; string concat
                    (cons ".=" " .= ")
                    )






) ; end of namespace

(provide 'electric-operator)

;;; electric-operator.el ends here

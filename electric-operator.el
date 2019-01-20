;;; electric-operator.el --- Automatically add spaces around operators -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: David Shepherd <davidshepherd7@gmail.com>
;; Version: 1.0.0
;; Package-Requires: ((dash "2.10.0") (emacs "24.4"))
;; Keywords: electric
;; URL: https://github.com/davidshepherd7/electric-operator

;;; Commentary:

;; An emacs minor-mode to automatically add spacing around operators. For
;; example typing `a=10*5+2' results in `a = 10 * 5 + 2'.

;;; Code:

(require 'cc-mode)
(require 'thingatpt)
(require 'cl-macs)
(eval-when-compile (require 'subr-x))

(require 'dash)



;;; Customisable variables

(defcustom electric-operator-double-space-docs nil
  "Enable double spacing of . in document lines - e,g, type '.' => get '.  '."
  :type 'boolean
  :group 'electricity)

(defcustom electric-operator-enable-in-docs nil
  "Enable electric-operator in strings and comments."
  :type 'boolean
  :group 'electricity)

(defcustom electric-operator-c-pointer-type-style 'variable
  "Defines how C/C++ mode pointer and reference types are spaced.

If set to 'variable' then the operator is touching the variable
name, as in `int *x'.

If set to 'type' then the operator is touching the type name , as
in `int* x'."
  :group 'electricity
  :type 'symbol
  :options '(variable type))

(defcustom electric-operator-R-named-argument-style 'unspaced
  "Defines whether = in R named function arguments should be
spaced.

Setting the value to 'spaced' results in f(foo = 1), 'unspaced'
results in f(foo=1)."
  :group 'electricity
  :type 'symbol
  :options '(spaced unspaced))



;;; Other variables

(defvar electric-operator--mode-rules-table
  (make-hash-table)
  "A hash table of replacement rule lists for specific major modes")




;;; Trie implementation, heavily based on code from PAIP

;; Outside the namespace because defstruct doesn't seem to work correctly

(cl-defstruct electric-operator--trie
  (value nil)
  (arcs (electric-operator--trie-arcs-make))
  ;; Used internally to implement longest-matching, don't set this by hand
  (terminal nil))

;; arcs as alists
(defun electric-operator--trie-arcs-make () nil)
(defun electric-operator--trie-arcs-put (key trie)
  (let ((new-trie (make-electric-operator--trie)))
    (push (cons key new-trie)
          (electric-operator--trie-arcs trie))
    new-trie))
(defun electric-operator--trie-arcs-get (key trie)
  (assoc key (electric-operator--trie-arcs trie)))


(defun electric-operator--trie-find (key on-fail trie)
  (cond
   ((null trie) nil)
   ((electric-operator--trie-terminal trie) (electric-operator--trie-follow-arc nil on-fail trie))
   ((atom key) (electric-operator--trie-follow-arc key on-fail trie))
   (t (electric-operator--trie-find (cdr key) on-fail
                                    (electric-operator--trie-find (car key) on-fail trie)))))

(defun electric-operator--trie-follow-arc (key-component on-fail trie)
  (let ((arc (electric-operator--trie-arcs-get key-component trie)))
    (cond
     ((not (null arc)) (cdr arc))
     ((equal on-fail 'return-nil) nil)
     ((equal on-fail 'extend) (electric-operator--trie-arcs-put key-component trie))
     ((equal on-fail 'longest-match)
      (make-electric-operator--trie
       ;; HACK: Construct a trie such that the next call to find will stop and
       ;; return this value
       :arcs (list (electric-operator--trie-arcs-get nil trie))
       :terminal t))
     (t (error "Unknown on-fail value: %s" on-fail)))))

(defun electric-operator--trie-put (key trie value)
  "Insert value for KEY into the trie.

KEY is a list of symbols,"
  (setf (electric-operator--trie-value (electric-operator--trie-find key 'extend trie)) value))

(defun electric-operator--trie-get (key trie)
  "Get the value for KEY in the trie.

KEY is a list of symbols."
  (let ((key-trie (electric-operator--trie-find key 'return-nil trie)))
    (when key-trie
      (electric-operator--trie-value key-trie))))

(defun electric-operator--trie-get-all (trie)
  "Extract all values into a list"
  (cond
   ((electric-operator--trie-value trie) (list (electric-operator--trie-value trie)))
   (t (-mapcat #'electric-operator--trie-get-all
               (-map #'cdr (electric-operator--trie-arcs trie))))))



(defun electric-operator--string-to-trie-key (string)
  ;; TODO: this is probably too slow long-term
  (--> string
       (split-string it "" t)
       (-map #'string-to-char it)
       (-filter (lambda (s) (not (or (= ?\s s) (= ?\t s)))) it)
       (reverse it)))

(defun electric-operator--trie-put-operator (operator value trie)
  "Like trie-put but works with operator strings"
  (electric-operator--trie-put (electric-operator--string-to-trie-key operator) trie value))

(defun electric-operator--trie-get-operator (operator trie)
  "Like trie-get, but with buffer substrings (looking backwards
from point) as the key."
  (let ((key-trie (electric-operator--trie-find
                   (electric-operator--string-to-trie-key operator)
                   'longest-match
                   trie)))
    (when key-trie
      (electric-operator--trie-value key-trie))))



;;; Rule data structure helpers


;; Uncomment safety for debugging, otherwise use speed. This seems to be faster
;; than speed 3.
(cl-declaim (optimize (speed 2) (safety 1)))
;; (cl-declaim (optimize (safety 3)))


(cl-defstruct electric-operator-compiled-rule operator regex action)

(defun electric-operator-make-compiled-rule-wrapper (rule)
  (when rule
    (if (not (electric-operator-compiled-rule-p rule))
        (make-electric-operator-compiled-rule
         :operator (car rule)
         :regex (electric-operator-rule-regex-with-whitespace (car rule))
         :action (cdr rule))
      rule)))



;;; Rule list helper functions

(defun electric-operator-rule-regex-with-whitespace (op)
  "Construct regex matching operator and any whitespace before/inside/after.

For example for the operator '+=' we allow '+=', ' +=', '+ ='. etc.

Whitespace before the operator is captured for possible use later.
"
  (concat "\\([ \t]*\\)"
          (mapconcat #'regexp-quote (split-string op "" t) "\\s-*")
          "\\([ \t]*\\)"))

(defun electric-operator--add-rule (initial new-rule)
  "Replace or append a new rule

Returns a modified copy of the rule list."
  (let* ((compiled (electric-operator-make-compiled-rule-wrapper new-rule))
         (op (electric-operator-compiled-rule-operator compiled)))
    (electric-operator--trie-put-operator op compiled initial)
    initial))

(defun electric-operator--add-rule-list (initial new-rules)
  "Replace or append a list of rules

Returns a modified copy of the rule list."
  (-each new-rules (lambda (r) (electric-operator--add-rule initial r)))
  initial)

(defun electric-operator-add-rules (initial &rest new-rules)
  "Replace or append multiple rules

Returns a modified copy of the rule list."
  (electric-operator--add-rule-list initial new-rules))


;; All rule manipulation should be done through these functions and not by
;; using puthash/gethash directly because it's plausible that the
;; underlying data structure could be changed (e.g. to an alist).

(defun electric-operator-get-rules-for-mode (major-mode-symbol)
  "Get the spacing rules for major mode"
  (electric-operator--trie-get-all (electric-operator-get-rules-trie-for-mode major-mode-symbol)))

(defun electric-operator-get-rules-trie-for-mode (major-mode-symbol)
  "Get the spacing rules for major mode"
  (gethash major-mode-symbol electric-operator--mode-rules-table))

(defun electric-operator-add-rules-for-mode (major-mode-symbol &rest new-rules)
  "Replace or add spacing rules for major mode

Destructively modifies `electric-operator--mode-rules-table' to use the new rules for
the given major mode."
  (puthash major-mode-symbol
           (electric-operator--add-rule-list (or (electric-operator-get-rules-trie-for-mode major-mode-symbol)
                                                 (make-electric-operator--trie))
                                             new-rules)
           electric-operator--mode-rules-table))




;;; Debugging helpers

(defun electric-operator--buffer-context (p n)
  "Print the contents of the buffer around p with n characters of context"
  (let ((before (buffer-substring (max (point-min) (- p n)) p))
        (after (buffer-substring p (min (point-max) (+ p n)))))
    (concat before "|" after)))

(defmacro electric-operator-debug-log (string &rest args)
  "Log a debugging message.

To enable debugging change the constant in the `when' t and
recompile electric-operator. It's like this because doing the
`when' at runtime introduces a 1.5x performance hit."
  `(when nil
     (message (funcall #'format (concat "ELO DEBUG: " ,string) ,@args))))




;;; Default rule lists

(electric-operator-add-rules-for-mode 'prog-mode
                                      (cons "=" " = ")
                                      (cons "<" " < ")
                                      (cons ">" " > ")
                                      (cons "%" " % ")
                                      (cons "+" #'electric-operator-prog-mode-+)
                                      (cons "-" #'electric-operator-prog-mode--)
                                      (cons "*" " * ")
                                      (cons "/" #'electric-operator-prog-mode-/)
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

(electric-operator-add-rules-for-mode 'text-mode
                                      (cons "." #'electric-operator-docs-.)
                                      (cons "," ", ")
                                      )



;;; Core functions

;; Borrowed from s.el
(defun electric-operator--trim-left (s)
  "Remove whitespace at the beginning of S."
  (save-match-data
    (if (string-match "\\`[ \t\n\r]+" s)
        (replace-match "" t t s)
      s)))

(defun electric-operator-get-rules-list ()
  "Pick which rule list is appropriate for spacing just before point"
  (save-excursion
    ;; We want to look one character before point because this is called
    ;; via post-self-insert-hook (there is no pre-self-insert-hook). This
    ;; allows us to correctly handle cases where the just-inserted
    ;; character ended a comment/string/...
    (forward-char -1)

    (cond
     ;; In comment or string?
     ((electric-operator-in-docs?) (if electric-operator-enable-in-docs
                                       (electric-operator-get-rules-trie-for-mode 'text-mode)
                                     (make-electric-operator--trie)))

     ;; Try to find an entry for this mode in the table
     ((electric-operator-get-rules-trie-for-mode major-mode))

     ;; Default modes
     ((derived-mode-p 'prog-mode) (electric-operator-get-rules-trie-for-mode 'prog-mode))
     (t (electric-operator-get-rules-trie-for-mode 'text-mode)))))

(defun electric-operator-longest-matching-rule (rule-list)
  "Return the rule with the most characters that applies to text before point"
  (electric-operator--trie-get-operator (buffer-substring-no-properties (max (point-min) (- (point) 20))
                                                                        (point))
                                        rule-list))

(defun electric-operator-eval-action (action point)
  (cond
   ((functionp action)
    (save-excursion (goto-char point) (funcall action)))
   ((stringp action) action)
   (t (error "Unrecognised action: %s" action))))

(defun electric-operator-post-self-insert-function ()
  "Check for a matching rule and apply it"
  (electric-operator-debug-log "Electric operator ran with context: %s" (electric-operator--buffer-context (point) 10))
  (-let* ((rule (electric-operator-longest-matching-rule (electric-operator-get-rules-list)))
          (operator-regex (and rule (electric-operator-compiled-rule-regex rule)))
          (action (and rule (electric-operator-compiled-rule-action rule)))
          (operator-just-inserted nil))
    (when (and rule action)
      (electric-operator-debug-log "Matched rule for operator: %S" (electric-operator-compiled-rule-operator rule))

      ;; Find point where operator starts
      (electric-operator-looking-back-locally operator-regex t)

      ;; Capture operator include all leading and *trailing* whitespace
      (save-excursion
        (goto-char (match-beginning 0))
        (looking-at operator-regex))

      (let* ((pre-whitespace (match-string 1))
             (op-match-beginning (match-beginning 0))
             (op-match-end (match-end 0))
             (spaced-string (electric-operator-eval-action action op-match-beginning)))

        ;; If action was a function which eval-d to nil then we do nothing.
        (when spaced-string

          ;; Record the fact we are inserting something for passing to fixup
          ;; functions
          (setq operator-just-inserted t)

          ;; Set an undo boundary for easy undo-ing of the automatic insertion
          (undo-boundary)

          ;; Delete the characters matching this rule before point
          (delete-region op-match-beginning op-match-end)

          (electric-operator-debug-log "Inserting spaced operator: %S" spaced-string)

          (if (electric-operator-looking-back-locally "^\\s-*")

              ;; This is the first thing in a line: leave the indentation alone.
              (progn
                (insert pre-whitespace)
                (insert (electric-operator--trim-left spaced-string)))

            ;; Insert correctly spaced operator
            (insert spaced-string)))))

    (when (derived-mode-p 'haskell-mode)
      (electric-operator-haskell-mode-fixup-partial-operator-parens operator-just-inserted))))

;;;###autoload
(define-minor-mode electric-operator-mode
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
  (if electric-operator-mode
      (add-hook 'post-self-insert-hook
                #'electric-operator-post-self-insert-function nil t)
    (remove-hook 'post-self-insert-hook
                 #'electric-operator-post-self-insert-function t)))



;;; Helper functions

(defun electric-operator-in-docs? ()
  "Check if we are inside a string or comment"
  (nth 8 (syntax-ppss)))

(defun electric-operator-hashbang-line? ()
  "Does the current line contain a UNIX hashbang?"
  (and (eq 1 (line-number-at-pos))
       (save-excursion
         (move-beginning-of-line nil)
         (looking-at "#!"))))

(defun electric-operator-enclosing-paren ()
  "Return the opening parenthesis of the enclosing parens, or nil
if not inside any parens."
  (interactive)
  (let ((ppss (syntax-ppss)))
    (when (nth 1 ppss)
      (char-after (nth 1 ppss)))))

(defun electric-operator-probably-unary-operator? ()
  "Try to guess if the operator we are about to insert will be unary

(i.e. takes one argument). This is a bit of a fudge based on C-like syntax."
  (or
   (electric-operator-looking-back-locally "[=,:\*\+-/><&^{;]\\s-*")
   (electric-operator-looking-back-locally "\\(return\\)\\s-*")
   (electric-operator-looking-back-locally "^\\s-*")))

(defun electric-operator-just-inside-bracket ()
  (electric-operator-looking-back-locally "[([{]"))

(defun electric-operator-looking-back-locally (string &optional greedy)
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

(defun electric-operator-docs-. ()
  "Double space if setting tells us to"
  (if electric-operator-double-space-docs
      ".  "
    ". "))

(defun electric-operator-prog-mode-- ()
  "Handle exponent and negative number notation"
  (cond
   ;; Exponent notation, e.g. 1e-10: don't space
   ((electric-operator-looking-back-locally "[0-9.]+[eE]") "-")

   ;; Space negative numbers as e.g. a = -1 (but don't space f(-1) or -1
   ;; alone at all). This will probably need to be major mode specific
   ;; eventually.
   ((electric-operator-probably-unary-operator?) " -")
   ((electric-operator-just-inside-bracket) "-")

   (t " - ")))

(defun electric-operator-prog-mode-+ ()
  "Handle +-prefix number notation"
  (cond
   ;; Space positive numbers as e.g. a = +1 (but don't space f(+1) or +1
   ;; alone at all). This will probably need to be major mode specific
   ;; eventually.
   ((electric-operator-probably-unary-operator?) " +")
   ((electric-operator-just-inside-bracket) "+")

   (t " + ")))

(defun electric-operator-prog-mode-/ ()
  "Handle path separator in UNIX hashbangs"
  ;; First / needs a space before it, rest don't need any spaces
  (cond ((and (electric-operator-hashbang-line?) (electric-operator-looking-back-locally "#!")) " /")
        ((electric-operator-hashbang-line?) "/")
        (t " / ")))




;;; C/C++ mode tweaks

(apply #'electric-operator-add-rules-for-mode 'c-mode (electric-operator-get-rules-for-mode 'prog-mode))
(electric-operator-add-rules-for-mode 'c-mode
                                      (cons "->" "->")

                                      (cons "/" #'electric-operator-c-mode-/)
                                      (cons "-" #'electric-operator-c-mode--)
                                      (cons "\"" #'electric-operator-c-mode-\")

                                      ;; ternary operator
                                      (cons "?" " ? ")
                                      (cons ":" #'electric-operator-c-mode-:) ; (or case label)

                                      ;; pointers
                                      (cons "*" #'electric-operator-c-mode-*)
                                      (cons "&" #'electric-operator-c-mode-&)
                                      (cons "**" #'electric-operator-c-mode-**) ; pointer-to-pointer type

                                      ;; increment/decrement
                                      (cons "++" #'electric-operator-c-mode-++)
                                      (cons "--" #'electric-operator-c-mode---)

                                      ;; #include statements
                                      (cons "<" #'electric-operator-c-mode-<)
                                      (cons ">" #'electric-operator-c-mode->)

                                      ;; bitshift operators
                                      (cons "<<" " << ")
                                      (cons ">>" " >> ")

                                      ;; Comments
                                      (cons "/*" " /* ")
                                      (cons "//" " // ")

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
(apply #'electric-operator-add-rules-for-mode 'c++-mode (electric-operator-get-rules-for-mode 'c-mode))

;; And some extra rules
(electric-operator-add-rules-for-mode 'c++-mode

                                      ;; Move constructor or `and' operator
                                      (cons "&&" #'electric-operator-c++-mode-&&)

                                      ;; Nested templates
                                      (cons ">>" #'electric-operator-c++-mode->>)

                                      ;; Handle for-each loops, public/private as well
                                      (cons ":" #'electric-operator-c++-mode-:)

                                      ;; Namespaces
                                      (cons "::" #'electric-operator-c++-mode-::)

                                      ;; Lambdas
                                      (cons "->" #'electric-operator-c++-mode-->)
                                      (cons "=" #'electric-operator-c++-mode-=)

                                      ;; Templates are hard to deal with sensibly
                                      (cons "<" nil)
                                      (cons ">" nil)
                                      )

;; Construct and add null rules for operator=, operator<< etc.
(--> (electric-operator-get-rules-for-mode 'c++-mode)
     (-map (lambda (p) (cons (concat "operator" (electric-operator-compiled-rule-operator p)) nil)) it)
     (apply #'electric-operator-add-rules-for-mode 'c++-mode it))

;; Use the c rules for arduino mode
(apply #'electric-operator-add-rules-for-mode 'arduino-mode (electric-operator-get-rules-for-mode 'c-mode))


(defvar electric-operator-c-user-types-regex
  "_t"
  "Regex used in looking-back-locally to check for C types

For now we just assume that anything ending in '_t' is a type.
I'm not sure if we can do any better by default.

You could add your own type names to this if needed. Send pull
requests/bug reports if you find any widely used type names that
could be added here.")

(defun electric-operator-c-after-type? ()
  (or
   ;; Check for built-in types
   (electric-operator-looking-back-locally (concat c-primitive-type-key "?"))

   ;; Check if previous word is struct/union/enum keyword
   (electric-operator-looking-back-locally "\\b\\(struct\\|union\\|enum\\|const\\)[[:space:]]+[[:alnum:]\\|_\\|:]+")

   (electric-operator-looking-back-locally "auto")

   ;; Check for any user-defined types
   (electric-operator-looking-back-locally electric-operator-c-user-types-regex)))

(defvar electric-operator-c-function-definition-syntax-list
  '(topmost-intro
    topmost-intro-cont
    arglist-intro
    arglist-cont-nonempty)
  "syntax symbols for lines which contain a function definition

See `c-guess-basic-syntax'.")

(defun electric-operator-c-is-function-or-class-definition? ()
  "Try to guess if we are in function definition/declaration

Using `cc-mode''s syntactic analysis."
  ;; There are similar but different symbols for objective-C, but I'm not
  ;; going to try to support that now.

  (--> (c-guess-basic-syntax)
       (-map #'car it)
       (-intersection electric-operator-c-function-definition-syntax-list it)))

(defun electric-operator-c-mode-include-line-opening-quote? ()
  (electric-operator-looking-back-locally "#\\s-*include\\s-*"))

(defun electric-operator-c-mode-include-line? ()
  (electric-operator-looking-back-locally "#\\s-*include.*"))

(defun electric-operator-c-mode-probably-ternary ()
  (electric-operator-looking-back-locally "\\?.+"))

(defun electric-operator-c-mode-\" ()
  "Handle the opening quote of an include directive"
  (when (electric-operator-c-mode-include-line-opening-quote?)
    " \""))

(defun electric-operator-c-mode-: ()
  "Handle the : part of ternary operator"
  (if (electric-operator-c-mode-probably-ternary)
      " : "
    ":"))

(defun electric-operator-c++-mode-: ()
  "Handle ternary, case, or for each"
  (cond
   ;; Public/private class methods
   ((electric-operator-looking-back-locally "private\\|public\\|protected") ":")

   ;; The colon in `class Foo : public Bar`
   ((electric-operator-c-is-function-or-class-definition?) " : ")

   ((electric-operator-c-mode-probably-ternary) " : ")

   ;; probably a for-each loop
   ((equal (electric-operator-enclosing-paren) ?\() " : ")

   ;; probably a case statement
   (t ":" )))

(defun electric-operator-c++-mode-:: ()
  "Handle qualified inheritance"
  (cond
   ;; Public/protected/private inheritance
   ((electric-operator-looking-back-locally "private\\|public\\|protected\\|:") " ::")
   ;; First colon of fully qualified inheritance without access-specifier
   ((electric-operator-looking-back-locally "\\(struct\\|class\\)[^:{]+") " : :")
   (t "::" )))


(defun electric-operator-c-mode-++ ()
  "Handle ++ operator pre/postfix and c++ in include strings"
  (cond
   ((electric-operator-c-mode-include-line?) "++")
   ((electric-operator-looking-back-locally "[a-zA-Z0-9_]\\s-*") "++ ")
   (t " ++")))

(defun electric-operator-c-mode--- ()
  "Handle -- operator pre/postfix"
  (cond ((electric-operator-looking-back-locally "[a-zA-Z0-9_]\\s-*") "-- ")
        (t " --")))

(defun electric-operator-c-mode-< ()
  "Handle #include brackets and templates"
  (cond ((electric-operator-c-mode-include-line?) " <")
        ((electric-operator-c-is-function-or-class-definition?) "<")
        (t " < ")))

(defun electric-operator-c-mode-> ()
  "Handle #include brackets and templates"
  (cond ((electric-operator-c-mode-include-line?) ">")
        ((electric-operator-c-is-function-or-class-definition?) "> ")
        (t " > ")))

(defun electric-operator-c++-mode->> ()
  "Handle nested templates"
  (cond ((electric-operator-c-is-function-or-class-definition?) ">> ")
        (t " >> ")))

(defun electric-operator-c-space-pointer-type (op)
  "Space a C pointer types operator as specified by
  `electric-operator-c-pointer-type-style'.

 For example `int* x'  or `int *x'."
  (cond ((eq electric-operator-c-pointer-type-style  'variable) (concat " " op))
        ((eq electric-operator-c-pointer-type-style 'type) (concat op " "))
        (t (error "Unrecognised value for electric-operator-c-pointer-type-style."))))

(defun electric-operator-c-mode-& ()
  "Handle C address-of operator and reference types"
  (cond
   ;; Reference types
   ((or (electric-operator-c-after-type?) (electric-operator-c-is-function-or-class-definition?))
    (electric-operator-c-space-pointer-type "&"))

   ;; Address-of operator or lambda pass-by-reference specifier
   ((electric-operator-just-inside-bracket) "&")
   ((electric-operator-probably-unary-operator?) " &")

   (t " & ")))

(defun electric-operator-c-mode-* ()
  "Handle C dereference operator and pointer types

Also handles C++ lambda capture by reference."
  (cond
   ;; Pointer types
   ((or (electric-operator-c-after-type?) (electric-operator-c-is-function-or-class-definition?))
    (electric-operator-c-space-pointer-type "*"))

   ;; Pointer dereference
   ((electric-operator-just-inside-bracket) "*")
   ((electric-operator-probably-unary-operator?) " *")

   (t " * ")))

(defun electric-operator-c-mode-** ()
  "C pointer to pointer or multiplication by pointer dereference.
  e.g. `res = a * *b;`'"
  (if (electric-operator-c-after-type?)
      (electric-operator-c-space-pointer-type "**")
    " * *"))

(defun electric-operator-c++-mode-&& ()
  "Handle move constructor"
  (if (electric-operator-c-is-function-or-class-definition?)
      (electric-operator-c-space-pointer-type "&&")
    " && "))

(defun electric-operator-c-mode-/ ()
  "Handle / in #include <a/b>"
  (cond
   ((electric-operator-c-mode-include-line?) "/")
   (t (electric-operator-prog-mode-/))))

(defun electric-operator-c-mode-- ()
  "Handle - in #include <a-b.h>"
  (cond
   ((electric-operator-c-mode-include-line?) "-")
   (t (electric-operator-prog-mode--))))

(defun electric-operator-c++-probably-lambda-arrow ()
  "Try to guess if we are writing a lambda statement"
  (electric-operator-looking-back-locally "\\[[^]]*\\]\\s-*([^)]*)\\s-*\\(mutable\\)?"))

(defun electric-operator-c++-mode--> ()
  "Handle lambda arrows"
  (if (electric-operator-c++-probably-lambda-arrow)
      " -> "
    "->"))

(defun electric-operator-c++-mode-= ()
  "Handle capture-by-value in lamdas"
  (cond ((electric-operator-probably-unary-operator?) " =")
        ((electric-operator-just-inside-bracket) "=")
        (t " = ")))



;;; Python mode tweaks

(apply #'electric-operator-add-rules-for-mode 'python-mode (electric-operator-get-rules-for-mode 'prog-mode))
(electric-operator-add-rules-for-mode 'python-mode
                     (cons "**" #'electric-operator-python-mode-**)
                     (cons "*" #'electric-operator-python-mode-*)
                     (cons ":" #'electric-operator-python-mode-:)
                     (cons "//" " // ") ; integer division
                     (cons "=" #'electric-operator-python-mode-kwargs-=)
                     (cons "-" #'electric-operator-python-mode-negative-slices)
                     (cons "->" " -> ") ; function return types
                     (cons "|=" " |= ")
                     (cons "&=" " &= ")
                     (cons "^=" " ^= ")
                     (cons "%=" " %= ")
                     (cons "<<" " << ")
                     (cons ">>" " >> ")
                     (cons "//=" " //= ")
                     (cons "**=" " **= ")
                     (cons ">>=" " >>= ")
                     (cons "<<=" " <<= ")
                     )

(defun electric-operator-python-mode-in-lambda-args? ()
    "Are we inside the arguments statement of a lambda?"
    (electric-operator-looking-back-locally "lambda[^:]*"))

  (defun electric-operator-python-mode-: ()
    "Handle python dict assignment"
    (cond
     ((electric-operator-python-mode-in-lambda-args?) ": ")
     ((eq (electric-operator-enclosing-paren) ?\{) ": ")
     ((and (eq (electric-operator-enclosing-paren) ?\() (electric-operator-looking-back-locally "def .*")) ": ") ; type definitions

     ;; Probably a variable type definition or an end of a keyword line, leave it
     ;; alone for now (possible TODO: variable type definitions properly by
     ;; checking if this line matches any keywords, and if not treating it as a
     ;; type definition).
     (t nil)))

  (defun electric-operator-python-mode-* ()
    "Handle python *args"
    (cond
     ;; After a ',' we need a space before
     ((electric-operator-looking-back-locally ",")  " *")
     ;; After a '(' or a newline we don't
     ((electric-operator-looking-back-locally "\\((\\|^\\)")  "*")
     ;; Othewise act as normal
     (t  " * ")))

  (defun electric-operator-python-mode-** ()
    "Handle python **kwargs"
    (cond
     ;; After a ',' we need a space before
     ((electric-operator-looking-back-locally ",")  " **")
     ;; After a '(' or a newline we don't
     ((electric-operator-looking-back-locally "\\((\\|^\\)")  "**")
     (t " ** ")))

  (defun electric-operator-python-mode-kwargs-= ()
    (cond
     ((electric-operator-python-mode-in-lambda-args?) "=")
     ((eq (electric-operator-enclosing-paren) ?\() "=")
     (t " = ")))

  (defun electric-operator-python-mode-negative-slices ()
    "Handle cases like a[1:-1], see issue #2."
    (if (and (eq (electric-operator-enclosing-paren) ?\[)
             (electric-operator-looking-back-locally ":"))
        "-"
      (electric-operator-prog-mode--)))



;;; Javascript mode tweaks

  (defun electric-operator-js-mode-: ()
    "Handle object assignment and ternary"
    (if (eq (electric-operator-enclosing-paren) ?\{)
        ": "
      " : "))

  (defun electric-operator-js-mode-/ ()
    "Handle regex literals and division"
    ;; Closing / counts as being inside a string so we don't need to do anything.
    (cond
     ;; Probably starting a regex
  ((electric-operator-probably-unary-operator?) nil)
  (t (electric-operator-prog-mode-/))))

(apply #'electric-operator-add-rules-for-mode 'js-mode (electric-operator-get-rules-for-mode 'prog-mode))
(electric-operator-add-rules-for-mode 'js-mode
                     (cons "%=" " %= ")
                     (cons "++" "++ ")
                     (cons "--" "-- ")
                     (cons "===" " === ")
                     (cons "!==" " !== ")
                     (cons "<<" " << ")
                     (cons ">>" " >> ")
                     (cons ":" #'electric-operator-js-mode-:)
                     (cons "?" " ? ")
                     (cons "/" #'electric-operator-js-mode-/)
                     (cons "//" " // ")
                     (cons "/*" " /* ")
                     (cons "=>" " => ") ; ES6 arrow functions
                     (cons "|=" " |= ")
                     (cons "&=" " &= ")
                     )

(apply #'electric-operator-add-rules-for-mode 'js2-mode (electric-operator-get-rules-for-mode 'js-mode))

(apply #'electric-operator-add-rules-for-mode 'typescript-mode (electric-operator-get-rules-for-mode 'js-mode))
(electric-operator-add-rules-for-mode 'typescript-mode
                     (cons ":" nil)
                     ;; Generics ruin everything
                     (cons ">>" nil)
                     (cons "<" nil)
                     (cons ">" nil)
                     (cons ">=" nil))



;;; Rust mode tweaks

(apply #'electric-operator-add-rules-for-mode 'rust-mode (electric-operator-get-rules-for-mode 'prog-mode))
(electric-operator-add-rules-for-mode 'rust-mode
                     ;; templates are hard
                     (cons "<" nil)
                     (cons ">" nil)

                     ;; mut vs. bitwise and
                     (cons "&" nil)

                     ;; pointer deref vs multiplication
                     (cons "*" nil)

                     (cons "/" #'electric-operator-prog-mode-/)
                     (cons "/*" " /* ")
                     (cons "//" " // ")

                     ;; Extra operators
                     (cons "<<" " << ")
                     (cons ">>" " >> ")
                     (cons "->" " -> ")
                     (cons "=>" " => ")

                     ;; Bar is used for lambdas as well as or
                     (cons "|" nil))



;; R tweaks (ess mode)

(defun electric-operator-ess-mode-keyword-args-= ()
  (if (and (eq electric-operator-R-named-argument-style 'unspaced)
           (eq (electric-operator-enclosing-paren) ?\())
      "="
    " = "))

;; ess-mode was renamed to ess-r-mode sometime in 2018, hopefully we can remove
;; the old mode in a few years.
(dolist (mode '(ess-mode ess-r-mode))
  (apply #'electric-operator-add-rules-for-mode mode (electric-operator-get-rules-for-mode 'prog-mode))
  (electric-operator-add-rules-for-mode mode
					(cons "." nil) ; word separator
					(cons "<-" " <- ") ; assignment
					(cons "->" " -> ") ; Right assignment
					(cons "%%" " %% ") ; Modulus
					(cons "%/%" " %/% ") ; Integer divide
					(cons "%*%" " %*% ") ; Matrix product
					(cons "%o%" " %o% ") ; Outer product
					(cons "%x%" " %x% ") ; Kronecker product
					(cons "%in%" " %in% ") ; Matching operator
					(cons "~" " ~ ") ; "is modeled by"
					(cons "%>%" " %>% ") ; Pipe (magrittr)
					(cons "%<>%" " %<>% ") ; Assignment pipe (magrittr)
					(cons "%$%" " %$% ") ; Exposition pipe (magrittr)
					(cons "%T>%" " %T>% ") ; Tee operator (magrittr)
					(cons "=" #'electric-operator-ess-mode-keyword-args-=)
					))


(defun electric-operator-ess-comma-post-self-insert-function ()
  (when electric-operator-mode
    (electric-operator-post-self-insert-function)))

;; ess-mode binds comma to a function, so we need to advise that function to
;; also run our code. ess-mode.el was renamed to ess.el in July 2018, in a few
;; years we can remove the old `ess-mode' form.
(with-eval-after-load 'ess-mode
  (advice-add 'ess-smart-comma :after #'electric-operator-ess-comma-post-self-insert-function))
(with-eval-after-load 'ess
  (advice-add 'ess-smart-comma :after #'electric-operator-ess-comma-post-self-insert-function))




;;; Other major mode tweaks

(apply #'electric-operator-add-rules-for-mode 'ruby-mode (electric-operator-get-rules-for-mode 'prog-mode))
(electric-operator-add-rules-for-mode 'ruby-mode
                     (cons "=~" " =~ ") ; regex equality
                     )

(apply #'electric-operator-add-rules-for-mode 'perl-mode (electric-operator-get-rules-for-mode 'prog-mode))
(electric-operator-add-rules-for-mode 'perl-mode
                     (cons "=~" " =~ ") ; regex equality
                     )

;; cperl mode is another perl mode, copy the rules
(apply #'electric-operator-add-rules-for-mode 'cperl-mode (electric-operator-get-rules-for-mode 'perl-mode))

;; This is based on a syntax guide and hasn't been tested.
(apply #'electric-operator-add-rules-for-mode 'java-mode (electric-operator-get-rules-for-mode 'prog-mode))
(electric-operator-add-rules-for-mode 'java-mode

                     ;; ternary operator
                     (cons "?" " ? ")
                     (cons ":" #'electric-operator-c-mode-:) ; (or case label)

                     ;; increment/decrement
                     (cons "++" #'electric-operator-c-mode-++)
                     (cons "--" #'electric-operator-c-mode---)

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
                     (cons "/" #'electric-operator-prog-mode-/)
                     (cons "/*" " /* ")
                     (cons "//" " // ")

                     ;; Generics are hard
                     (cons "<" nil)
                     (cons ">" nil)
                     )



;; Haskell mode

(defconst electric-operator-haskell-mode-infix-binary-operators
  (list "=" "<" ">" "%" "+" "*" "&" "|" "==" "<=" ">=" "&&" "||"

        "++" ; list concat
        "!!"  ; indexing
        ".|." ; bitwise OR
        ".&." ; bitwise AND
        "$" ; delay evaluation

        ;; Monads or something like that
        ">>" ">>=" "<$>" "<*>"

        ;; Exponents, for some reason there are three of
        ;; them!
        "^" "**" "^^"
        ))

(defconst electric-operator-haskell-mode-special-infix-binary-operators
  (list "/" "-"))

(defun electric-operator-haskell-mode-infix-action (op)
  (lambda ()
    (let ((after-paren (electric-operator-looking-back-locally "(\\s-*"))
          (before-paren (looking-at (concat (electric-operator-rule-regex-with-whitespace op) ")"))))
      (cond
       ;; only thing in the parens: no spaces
       ((and after-paren before-paren) op)

       (before-paren (concat " " op))
       (after-paren (concat op " "))
       (t (concat " " op " "))))))

(defun electric-operator-haskell-mode-fixup-partial-operator-parens (operator-just-inserted)
  (when (not operator-just-inserted)
    (-each (-concat electric-operator-haskell-mode-infix-binary-operators electric-operator-haskell-mode-special-infix-binary-operators)
      (lambda (op)
        ;; If another character was typed between an operator and `)', make sure
        ;; these's a single space there.
        (when (and (looking-at "\\s-*)")
                   (electric-operator-looking-back-locally (concat (electric-operator-rule-regex-with-whitespace op) "[^\\s-]")))
          (save-excursion (replace-match " " nil nil nil 2)))

        ;; When inserting a ) delete any whitespace between it and the operator
        (when (electric-operator-looking-back-locally (concat "\\s-" op ")"))
          (save-excursion (replace-match ")" nil nil nil 0)))))))

(defun electric-operator-haskell-mode-/ ()
  (let ((base (funcall (electric-operator-haskell-mode-infix-action "/"))))
    (if (equal base " / ")
        (electric-operator-prog-mode-/)
      base)))

(defun electric-operator-haskell-mode-- ()
  (let ((base (funcall (electric-operator-haskell-mode-infix-action "-"))))
    (if (equal base " - ")
        (electric-operator-prog-mode--)
      base)))

(apply #'electric-operator-add-rules-for-mode 'haskell-mode (electric-operator-get-rules-for-mode 'prog-mode))

;; Make rules for partially evaluated binary operators inside parens
(apply #'electric-operator-add-rules-for-mode 'haskell-mode
       (-map (lambda (op) (cons op (electric-operator-haskell-mode-infix-action op)))
             electric-operator-haskell-mode-infix-binary-operators))

(electric-operator-add-rules-for-mode 'haskell-mode

                     ;; More complex infix operators
                     (cons "-" #'electric-operator-haskell-mode--)
                     (cons "/" #'electric-operator-haskell-mode-/)
                     (cons ":" nil)  ; list constructor: no spaces needed in either

                     (cons "--" "-- ") ; comment
                     (cons "<-" " <- ") ; assignment
                     (cons "->" " -> ") ; lambdas and function types
                     (cons "=>" " => ") ; typeclasses
                     (cons "::" " :: ") ; type specification
                     (cons "!=" nil) ; unused
                     (cons "~" " ~") ; lazy pattern match

                     ;; Comments?
                     (cons "{-" "{- ")
                     (cons "-}" " -}")

                     ;; Either function composition or function qualification,
                     ;; can't tell so disable it
                     (cons "." nil)
                     )



(apply #'electric-operator-add-rules-for-mode 'php-mode (electric-operator-get-rules-for-mode 'prog-mode))
(electric-operator-add-rules-for-mode 'php-mode
                     (cons "**" " ** ")
                     (cons "%=" " %= ")
                     (cons "===" " === ")
                     (cons "<>" " <> ") ; not-equal
                     (cons "!==" " !== ")
                     (cons "++" #'electric-operator-c-mode-++)
                     (cons "--" #'electric-operator-c-mode---)
                     (cons "." " . ")   ; string concat
                     (cons ".=" " .= ")
                     (cons "->" "->")
                     (cons "=>" " => ")
                     (cons "<?" "<?")

                     (cons "/" #'electric-operator-prog-mode-/)
                     (cons "/*" " /* ")
                     (cons "//" " // ")
                     )


;; Coffee script support based on http://coffeescript.org/#operators
(apply #'electric-operator-add-rules-for-mode 'coffee-mode (electric-operator-get-rules-for-mode 'prog-mode))
(electric-operator-add-rules-for-mode 'coffee-mode
                     (cons "**" " ** ")
                     (cons "//" " // ")
                     (cons "///" " /// ")
                     (cons "%%" " %% ")
                     (cons "?" "? ")
                     (cons "?=" " ?= ")
                     (cons "?." "?.")
                     (cons "->" " -> ")
                     (cons "=>" " => ")
                     )

(apply #'electric-operator-add-rules-for-mode 'sql-mode (electric-operator-get-rules-for-mode 'prog-mode))
(electric-operator-add-rules-for-mode 'sql-mode
                     (cons "-" nil)
                     (cons "=" nil)
                     (cons "%" nil)
                     (cons "*" nil))

;; Don't use either prog or text mode defaults, css is too different
(electric-operator-add-rules-for-mode 'css-mode
                     (cons ":" ": ")
                     (cons "," ", "))

(electric-operator-add-rules-for-mode 'scss-mode
                     (cons ":" ": ")
                     (cons "," ", "))




;;; Julia mode

(defun electric-operator-julia-mode-kwargs-= ()
  (cond
   ((eq (electric-operator-enclosing-paren) ?\() "=")
   (t " = ")))

(apply #'electric-operator-add-rules-for-mode 'julia-mode (electric-operator-get-rules-for-mode 'prog-mode))
(electric-operator-add-rules-for-mode 'julia-mode

                     (cons "=" #'electric-operator-julia-mode-kwargs-=)
                     (cons ";" "; ")

                     ;; Subtype comparison
                     (cons "<:" " <: ")

                     ;; Cool! Unicode!
                     (cons "÷" " ÷ ")
                     (cons "≠" " ≠ ")
                     (cons "≤" " ≤ ")
                     (cons "≥" " ≥ ")

                     ;; something about fractions
                     (cons "//" " // ")
                     (cons ".//" " .// ")
                     (cons "//=" " //= ")

                     ;; pipe
                     (cons "|>" " |> ")

                     (cons "*" " * ")
                     (cons "/" " / ")
                     (cons "%" " % ")
                     (cons "&" " & ")

                     ;; \ (escaped), for solving matrix multiplies
                     (cons "\\" " \\ ")
                     (cons "\\=" " \\= ")
                     (cons ".\\" " .\\ ")

                     ;; XOR
                     (cons "$" " $ ")

                     ;; Even more equal!
                     (cons "===" " === ")
                     (cons "!==" " !== ")

                     ;; vector operations and assign-operators
                     (cons ".^" " .^ ")
                     (cons ".*" " .* ")
                     (cons "./" " ./ ")
                     (cons ".%" " .% ")
                     (cons "<<" " << ")
                     (cons ">>" " >> ")
                     (cons ">>>" " >>> ")
                     (cons ".<<" " .<< ")
                     (cons ".>>" " .>> ")
                     (cons ".>>>" " .>>> ")
                     (cons ".+" " .+ ")
                     (cons ".-" " .- ")
                     (cons ".>" " .> ")
                     (cons ".<" " .< ")
                     (cons ".>=" " .>= ")
                     (cons ".<=" " .<= ")
                     (cons ".==" " .== ")
                     (cons ".!=" " .!= ")
                     (cons "^=" " ^= ")
                     (cons "÷=" " ÷= ")
                     (cons "%=" " %= ")
                     (cons "|=" " |= ")
                     (cons "&=" " &= ")
                     (cons "$=" " $= ")
                     (cons "<<=" " <<= ")
                     (cons ">>=" " >>= ")
                     (cons ">>>=" " >>>= ")
                     (cons ".+=" " .+= ")
                     (cons ".-=" " .-= ")
                     (cons ".*=" " .*= ")
                     (cons "./=" " ./= ")
                     (cons ".//=" " .//= ")
                     (cons ".\\=" " .\\= ")
                     (cons ".^=" " .^= ")
                     (cons ".÷=" " .÷= ")
                     (cons ".%=" " .%= "))



;;; Swift mode

(apply #'electric-operator-add-rules-for-mode 'swift-mode (electric-operator-get-rules-for-mode 'prog-mode))
(electric-operator-add-rules-for-mode 'swift-mode
                                      (cons "//" " // ")
                                      (cons "/*" " /* ")
                                      (cons "..<" nil))



(provide 'electric-operator)

;;; electric-operator.el ends here

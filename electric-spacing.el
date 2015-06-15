;;; electric-spacing.el --- Insert operators with surrounding spaces smartly

;; Copyright (C) 2004, 2005, 2007-2015 Free Software Foundation, Inc.

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 5.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Electric spacing mode is a minor mode which automatically inserts
;; surrounding spaces around operator symbols. For example, `=' becomes ` =
;; ', `+=' becomes ` += '.
;;
;; Type `M-x electric-spacing-mode' to toggle this minor mode.

;;; Acknowledgements

;; Nikolaj Schumacher <n_schumacher@web.de>, for suggesting
;; reimplementing as a minor mode and providing an initial patch for
;; that.

;;; Code:

(require 'cc-mode)
(require 'thingatpt)
(require 'dash)
(require 's)
(require 'names)

;; namespace using names.el:
;;;###autoload
(define-namespace electric-spacing-



;; Customisable variables

(defcustom double-space-docs t
  "Enable double spacing of . in document lines - e,g, type '.' => get '.  '."
  :type 'boolean
  :group 'electricity)

(defcustom enable-in-docs t
  "Enable electric-spacing in strings and comments."
  :type 'boolean
  :group 'electricity)



;; Other variables

(defvar mode-rules-table
  (make-hash-table)
  "A hash table of replacement rule lists for specific major modes")



;; Rule list helper functions

(defun -add-rule (initial new-rule)
  "Replace or append a new rule

Returns a modified copy of the rule list."
  (let* ((op (car new-rule))
         (existing-rule (assoc op initial)))
    (if existing-rule
        (-replace-first existing-rule new-rule initial)
      (-snoc initial new-rule))))

(defun -add-rule-list (initial new-rules)
  "Replace or append a list of rules

Returns a modified copy of the rule list."
  (-reduce #'-add-rule (-concat (list initial) new-rules)))

(defun add-rules (initial &rest new-rules)
  "Replace or append multiple rules

Returns a modified copy of the rule list."
  (-add-rule-list initial new-rules))

(defun add-rules-for-mode (major-mode &rest new-rules)
  "Replace or add spacing rules for major mode

Destructively modifies mode-rules-table to use the new rules for
the given major mode."
  (puthash major-mode
           (-add-rule-list (gethash major-mode mode-rules-table)
                           new-rules)
           mode-rules-table))



;; Default rule lists

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
        (cons "&=" " &= ")
        (cons "|=" " |= ")

        (cons "&&" " && ")
        (cons "||" " || ")
        )
  "Default spacing rules for programming modes")

(defvar prose-rules
  (add-rules prog-mode-rules
             (cons "." #'docs-.)
             (cons "%" nil) ; format strings
             (cons "/" nil) ; path separator
             )
  "Rules to use in comments, strings and text modes.")



;; Core functions

(defun get-rules-list ()
  "Pick which rule list is appropriate for spacing at point"
  (cond
   ;; In comment or string?
   ((and enable-in-docs (in-docs?)) prose-rules)

   ;; Try to find an entry for this mode in the table
   ((gethash major-mode mode-rules-table))

   ;; Default modes
   ((derived-mode-p 'prog-mode) prog-mode-rules)
   (t prose-rules)))

(defun rule-regex-with-whitespace (op)
  "Construct regex matching operator and any whitespace before/inside/after

For example for the operator '+=' we allow '+=', ' +=', '+ ='. etc.
"
  (s-join "\s*" (-map #'regexp-quote (s-split "" op))))

(defun longest-matching-rule (rule-list)
  "Return the rule with the most characters that applies to text before point"
  ;; Using a threading macro here would be much cleaner, but dash's
  ;; threading macros don't work with Names at the moment.
  (let* ((filtered
          (-filter (lambda (rule) (looking-back (rule-regex-with-whitespace (car rule))))
                   rule-list))
         (sorted
          (-sort (lambda (p1 p2) (> (length (car p1)) (length (car p2))))
                 filtered)))
    (car sorted)))

(defun post-self-insert-function ()
  "Check for a matching rule and apply it"
  (-let* ((rule (longest-matching-rule (get-rules-list)))
          ((operator . action) rule))
    (when (and rule action)

      ;; Delete the characters matching this rule before point
      (looking-back (rule-regex-with-whitespace operator) nil t)
      (let ((match (match-data)))
        (delete-region (nth 0 match) (nth 1 match)))

      ;; Insert correctly spaced operator
      (if (stringp action)
          (insert action)
        (insert (funcall action))))))

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



;; Helper functions

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



;; General tweaks

(defun docs-. ()
  "Double space if setting tells us to"
  (if double-space-docs
      ".  "
    ". "))

(defun prog-mode-- ()
  "Handle exponent and negative number notation"
  ;; exponent notation, e.g. 1e-10: don't space
  (cond ((looking-back "[0-9.]+[eE]") "-")

        ;; Space negative numbers as e.g. a = -1 (but don't space f(-1) or
        ;; -1 alone at all). This will proabaly need to be major mode
        ;; specific eventually.
        ((looking-back "[=,:\*\+-/]") " -")
        ((looking-back "[[(]") "-")
        ((looking-back "^") "-")

        (t " - ")))

(defun prog-mode-/ ()
  "Handle path separator in UNIX hashbangs"
  ;; First / needs a space before it, rest don't need any spaces
  (cond ((and (hashbang-line?) (looking-back "#!")) " /")
        ((hashbang-line?) "/")
        (t " / ")))



;; C mode tweaks

(apply #'add-rules-for-mode 'c-mode prog-mode-rules)
(add-rules-for-mode 'c-mode
                    (cons "->" "->")

                    ;; ternary operator
                    (cons "?" " ? ")
                    (cons ":" #'c-mode-:) ; (or case label)

                    ;; pointers
                    (cons "*" #'c-mode-*)
                    (cons "&" #'c-mode-&)
                    (cons "**" " **") ; pointer-to-pointer type

                    ;; increment/decrement
                    (cons "++" #'c-mode-++)
                    (cons "--" #'c-mode---)

                    ;; #include statements
                    (cons "<" #'c-mode-<)
                    (cons ">" #'c-mode->)
                    )


;; Use the same rules for c++
(puthash 'c++-mode (gethash 'c-mode mode-rules-table)
         mode-rules-table)

(defun c-mode-is-unary? ()
  "Try to guess if this is the unary form of an operator"
  (or (looking-back "[=,]\s*")
      (looking-back "^\s*")))

(defun c-types-regex ()
  (concat c-primitive-type-key "?"))

(defun c-mode-: ()
  "Handle the : part of ternary operator"
  (if (looking-back "\\?.+")
      " : "
    ":"))

(defun c-mode-++ ()
  "Handle ++ operator pre/postfix"
  (if (looking-back "[a-zA-Z0-9_]\s*")
      "++ "
    " ++"))

(defun c-mode--- ()
  "Handle -- operator pre/postfix"
  (if (looking-back "[a-zA-Z0-9_]\s*")
      "-- "
    " --"))

(defun c-mode-< ()
  "Handle #include brackets"
  (if (looking-back "#\s*include\s*")
      " <"
    ;; else
    " < "))

(defun c-mode-> ()
  "Handle #include brackets"
  (if (looking-back "#\s*include.*")
      ">"
    ;; else
    " > "))

(defun c-mode-& ()
  "Handle C address-of operator and reference types"
  (cond ((looking-back (c-types-regex)) " &")
        ((c-mode-is-unary?) " &")
        ((looking-back "(") "&")
        (t " & ")))

(defun c-mode-* ()
  "Handle C dereference operator and pointer types"
  (cond ((looking-back (c-types-regex)) " *")
        ((c-mode-is-unary?) " *")
        ((looking-back "(") "*")
        (t " * ")))



;; Python mode tweaks

(apply #'add-rules-for-mode 'python-mode prog-mode-rules)
(add-rules-for-mode 'python-mode
                    (cons "**" #'python-mode-**)
                    (cons "*" #'python-mode-*)
                    (cons ":" #'python-mode-:)
                    (cons "//" " // ")
                    (cons "=" #'python-mode-kwargs-=)
                    )

(defun python-mode-: ()
  "Handle python dict assignment"
  (if (and (not (in-string-p))
           (eq (enclosing-paren) ?\{))
      ": "
    ":"))

(defun python-mode-* ()
  "Handle python *args"
  ;; Can only occur after '(' ',' or on a new line, so just check for those.
  ;; If it's just after a comma then also insert a space before the *.
  (cond ((looking-back ",")  " *")
        ((looking-back "[(,^)][ \t]*")  "*")
        ;; Othewise act as normal
        (t  " * ")))

(defun python-mode-** ()
  "Handle python **kwargs"
  (cond ((looking-back ",") " **")
        ((looking-back "[(,^)][ \t]*") "**")
        (t " ** ")))

(defun python-mode-kwargs-= ()
  (if (eq (electric-spacing-enclosing-paren) ?\()
      "="
    " = "))



;; Other major mode tweaks

(puthash 'ruby-mode
         (add-rules prog-mode-rules
                    ;; regex equality
                    (cons "=~" " =~ ")
                    )
         mode-rules-table)

(puthash 'perl-mode
         (add-rules prog-mode-rules
                    ;; regex equality
                    (cons "=~" " =~ ")
                    )
         mode-rules-table)
(puthash 'cperl-mode (gethash 'cperl-mode mode-rules-table)
         mode-rules-table)

(puthash 'haskell-mode
         ;; Health warning: i haven't written much haskell recently so i'm
         ;; likely to have missed some things.
         (add-rules prog-mode-rules
                    (cons "." " . ") ;; function composition
                    (cons "++" " ++ ") ;; list concat
                    (cons "!!" " !! ") ;; indexing
                    (cons "$" " $ ")
                    (cons "<-" " <- ")
                    (cons "->" " -> ")
                    (cons ":" nil) ;; list constructor
                    (cons "::" " :: ") ;; type specification
                    (cons "!=" nil) ;; not-equal is /=
                    )
         mode-rules-table)


) ; End of names

(provide 'electric-spacing)

;;; electric-spacing.el ends here

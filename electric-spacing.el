;;; electric-spacing.el --- Insert operators with surrounding spaces smartly

;; Copyright (C) 2004, 2005, 2007-2014 Free Software Foundation, Inc.

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

;; Smart Operator mode is a minor mode which automatically inserts
;; surrounding spaces around operator symbols.  For example, `='
;; becomes ` = ', `+=' becomes ` += '.  This is most handy for writing
;; C-style source code.
;;
;; Type `M-x smart-operator-mode' to toggle this minor mode.

;;; Acknowledgements

;; Nikolaj Schumacher <n_schumacher@web.de>, for suggesting
;; reimplementing as a minor mode and providing an initial patch for
;; that.

;;; Code:

(require 'cc-mode)
(require 'thingatpt)
(require 'dash)
(require 's)

;;; electric-spacing minor mode

(defcustom electric-spacing-double-space-docs t
  "Enable double spacing of . in document lines - e,g, type '.' => get '.  '."
  :type 'boolean
  :group 'electricity)

(defcustom electric-spacing-docs t
  "Enable electric-spacing in strings and comments."
  :type 'boolean
  :group 'electricity)



(defvar electric-spacing-rules
  '(("=" . " = ")
    ("<" . " < ")
    (">" . " > ")
    ("%" . electric-spacing-%)
    ("+" . electric-spacing-+)
    ("-" . electric-spacing--)
    ("*" . electric-spacing-*)
    ("/" . electric-spacing-/)
    ("&" . electric-spacing-&)
    ("|" . " | ")
    (":" . electric-spacing-:)
    ("?" . electric-spacing-?)
    ("," . ", ")
    ("." . electric-spacing-.)
    ("^" . " ^ ")

    ("==" . " == ")
    ("!=" . " != ")
    ("<=" . " <= ")
    (">=" . " >= ")

    ("*=" . " *= ")
    ("+=" . " += ")
    ("/=" . " /= ")
    ("-=" . " -= ")
    ("&=" . " &= ")
    ("|=" . " |= ")

    ("&&" . " && ")
    ("||" . " || ")

    ;; Regex equality (ruby, perl)
    ("=~" . " =~ ")))

(defun add-rule (initial new-rule)
  "Replace or append a new rule"
  (let* ((op (car new-rule))
         (existing-rule (assoc op initial)))
    (if existing-rule
        (-replace-first existing-rule new-rule initial)
      (-snoc initial new-rule))))

(defun add-rules (initial &rest new-rules)
  (add-rule-list initial new-rules))

(defun add-rule-list (initial new-rules)
  (-reduce #'add-rule (-concat (list initial) new-rules)))

(defvar python-rules
  (add-rules electric-spacing-rules
             '("**" . electric-spacing-python-**)
             '("*" . electric-spacing-python-*)
             '("//" . " // ")))

(defvar c-rules
  (add-rules electric-spacing-rules
             '("->" . "->")))

(defvar prose-rules
  (when electric-spacing-docs
    (add-rules electric-spacing-rules
               '("." . electric-spacing-docs-.)))
  "Spacing rules to use in comments, strings and text modes.")

(defun get-rules-list ()
  (cond
   ;; In comment or string?
   ((electric-spacing-document?) prose-rules)

   ;; Other modes
   ((derived-mode-p 'python-mode) python-rules)
   ((derived-mode-p 'c-mode 'c++-mode) c-rules)

   ;; Default modes
   ((derived-mode-p 'prog-mode) electric-spacing-rules)
   (t prose-rules)))



(defun rule-regex-with-whitespace (op)
  "Construct regex matching operator and any whitespace before/inside/after

For example for the operator '+=' we allow '+=', ' +=', '+ ='. etc.
"
  (s-join "\s*" (-map #'regexp-quote (s-split "" op))))

(defun longest-matching-rule (rule-list)
  "Return the rule with the most characters that applies to text before point"
  (->> rule-list
       (-filter (lambda (rule) (looking-back (rule-regex-with-whitespace (car rule)))))
       (-sort (lambda (p1 p2) (> (length (car p1)) (length (car p2)))))
       car))

(defun electric-spacing-post-self-insert-function ()
  (when (electric-spacing-should-run?)
    (let* ((rule (longest-matching-rule (get-rules-list)))
           (operator (car rule))
           (action (cdr rule)))
      (when rule
        ;; Delete the characters matching this rule before point
        (looking-back (rule-regex-with-whitespace (car rule)))
        (let ((match (match-data)))
          (delete-region (nth 0 match) (nth 1 match)))

        ;; TODO: shouldn't need this, but it seems we do...
        (delete-horizontal-space)

        ;; Insert correctly spaced operator
        (if (stringp action)
            (insert action)
          (funcall action))))))

;;;###autoload
(define-minor-mode electric-spacing-mode
  "Toggle automatic surrounding space insertion (Electric Spacing mode).
With a prefix argument ARG, enable Electric Spacing mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

This is a local minor mode.  When enabled, typing an operator automatically
inserts surrounding spaces.  e.g., `=' becomes ` = ',`+=' becomes ` += '.  This
is very handy for many programming languages."
  :global nil
  :group 'electricity
  :lighter " _+_"

  ;; body
  (if electric-spacing-mode
      (add-hook 'post-self-insert-hook
                #'electric-spacing-post-self-insert-function nil t)
    (remove-hook 'post-self-insert-hook
                 #'electric-spacing-post-self-insert-function t)))

(defun electric-spacing-insert (op &optional only-where)
  "See `electric-spacing-insert-1'."
  (delete-horizontal-space)
  (cond ((and (electric-spacing-lispy-mode?)
              (not (electric-spacing-document?)))
         (electric-spacing-lispy op))
        (t
         (electric-spacing-insert-1 op only-where))))

(defun electric-spacing-insert-1 (op &optional only-where)
  "Insert operator OP with surrounding spaces.
e.g., `=' becomes ` = '.

When `only-where' is 'after, we will insert space at back only;
when `only-where' is 'before, we will insert space at front only;
when `only-where' is 'middle, we will not insert space."
  (pcase only-where
    (`before (insert " " op))
    (`middle (insert op))
    (`after (insert op " "))
    (_
     (let ((begin? (bolp)))
       (unless begin?
         (insert " "))
       (insert op " ")
       (when begin?
         (indent-according-to-mode))))))

(defun electric-spacing-c-types ()
  (concat c-primitive-type-key "?"))

(defun electric-spacing-document? ()
  (nth 8 (syntax-ppss)))

(defun electric-spacing-should-run? ()
  (or (not electric-spacing-docs)
      (not (electric-spacing-document?))))

(defun electric-spacing-lispy-mode? ()
  (derived-mode-p 'emacs-lisp-mode
                  'lisp-mode
                  'lisp-interaction-mode
                  'scheme-mode))

(defun electric-spacing-lispy (op)
  "We're in a Lisp-ish mode, so let's look for parenthesis.
Meanwhile, if not found after ( operators are more likely to be function names,
so let's not get too insert-happy."
  (cond
   ((save-excursion
      (backward-char 1)
      (looking-at "("))
    (if (equal op ",")
        (electric-spacing-insert-1 op 'middle)
      (electric-spacing-insert-1 op 'after)))
   ((equal op ",")
    (electric-spacing-insert-1 op 'before))
   (t
    (electric-spacing-insert-1 op 'middle))))


;;; Fine Tunings

(defun electric-spacing-< ()
  "See `electric-spacing-insert'."
  (cond
   ((or (and c-buffer-is-cc-mode
             (looking-back
              (concat "\\("
                      (regexp-opt
                       '("#include" "vector" "deque" "list" "map" "stack"
                         "multimap" "set" "hash_map" "iterator" "template"
                         "pair" "auto_ptr" "static_cast"
                         "dynmaic_cast" "const_cast" "reintepret_cast"

                         "#import"))
                      "\\)\\ *")
              (line-beginning-position)))
        (derived-mode-p 'sgml-mode))
    (insert "<>")
    (backward-char))
   (t
    (electric-spacing-insert "<"))))

(defun electric-spacing-: ()
  "See `electric-spacing-insert'."
  (cond (c-buffer-is-cc-mode
         (if (looking-back "\\?.+")
             (electric-spacing-insert ":")
           (electric-spacing-insert ":" 'middle)))
        ((derived-mode-p 'haskell-mode)
         (electric-spacing-insert ":"))
        ((derived-mode-p 'ess-mode)
         (insert ":"))
        (t
         (electric-spacing-insert ":" 'after))))

(defun electric-spacing-. ()
  "See `electric-spacing-insert'."
  (cond ((and electric-spacing-double-space-docs
              (electric-spacing-document?))
         (electric-spacing-insert "." 'after)
         (insert " "))
        ((or (looking-back "[0-9]")
             (or (and c-buffer-is-cc-mode
                      (looking-back "[a-z]"))
                 (and
                  (derived-mode-p 'ruby-mode)
                  (looking-back "[a-z\)]"))
                 (and
                  (derived-mode-p 'js-mode 'js2-mode)
                  (looking-back "[a-z\)$]"))))
         (insert "."))
        ((derived-mode-p 'cperl-mode 'perl-mode 'ruby-mode)
         ;; Check for the .. range operator
         (if (looking-back ".")
             (insert ".")
           (insert " . ")))
        (t
         (electric-spacing-insert "." 'after)
         (insert " "))))

(defun electric-spacing-docs-. ()
  ;; Double space if setting tells us to
  (if electric-spacing-double-space-docs
      (insert ".  ")
    (insert ". "))
  )

(defun electric-spacing-& ()
  "See `electric-spacing-insert'."
  (cond (c-buffer-is-cc-mode
         ;; ,----[ cases ]
         ;; | char &a = b; // FIXME
         ;; | void foo(const int& a);
         ;; | char *a = &b;
         ;; | int c = a & b;
         ;; | a && b;
         ;; `----
         (cond ((looking-back (concat (electric-spacing-c-types) " *" ))
                (electric-spacing-insert "&" 'after))
               ((looking-back "= *")
                (electric-spacing-insert "&" 'before))
               (t
                (electric-spacing-insert "&"))))
        (t
         (electric-spacing-insert "&"))))

(defun electric-spacing-* ()
  "See `electric-spacing-insert'."
  (cond (c-buffer-is-cc-mode
         ;; ,----
         ;; | a * b;
         ;; | char *a;
         ;; | char **b;
         ;; | (*a)->func();
         ;; | *p++;
         ;; | *a = *b;
         ;; `----
         (cond ((looking-back (concat (electric-spacing-c-types) " *" ))
                (electric-spacing-insert "*" 'before))
               ((looking-back "\\* *")
                (electric-spacing-insert "*" 'middle))
               ((looking-back "^[ (]*")
                (electric-spacing-insert "*" 'middle)
                (indent-according-to-mode))
               ((looking-back "= *")
                (electric-spacing-insert "*" 'before))
               (t
                (electric-spacing-insert "*"))))

        (t
         (electric-spacing-insert "*"))))

(defun electric-spacing-+ ()
  "See `electric-spacing-insert'."
  (cond ((and c-buffer-is-cc-mode (looking-back "\\+ *"))
         (when (looking-back "[a-zA-Z0-9_] +\\+ *")
           (save-excursion
             (backward-char 2)
             (delete-horizontal-space)))
         (electric-spacing-insert "+" 'middle)
         (indent-according-to-mode))
        (t
         (electric-spacing-insert "+"))))

(defun electric-spacing-- ()
  "See `electric-spacing-insert'."
  (cond ((and c-buffer-is-cc-mode (looking-back "\\- *"))
         (when (looking-back "[a-zA-Z0-9_] +\\- *")
           (save-excursion
             (backward-char 2)
             (delete-horizontal-space)))
         (electric-spacing-insert "-" 'middle)
         (indent-according-to-mode))

        ;; exponent notation, e.g. 1e-10: don't space
        ((looking-back "[0-9.]+[eE]")
         (insert "-"))

        (t
         (electric-spacing-insert "-"))))

(defun electric-spacing-? ()
  "See `electric-spacing-insert'."
  (cond (c-buffer-is-cc-mode
         (electric-spacing-insert "?"))
        (t
         (electric-spacing-insert "?" 'after))))

(defun electric-spacing-% ()
  "See `electric-spacing-insert'."
  (cond (c-buffer-is-cc-mode
         ;; ,----
         ;; | a % b;
         ;; | printf("%d %d\n", a % b);
         ;; `----
         (if (and (looking-back "\".*")
                  (not (looking-back "\",.*")))
             (insert "%")
           (electric-spacing-insert "%")))
        ;; If this is a comment or string, we most likely
        ;; want no spaces - probably string formatting
        ((and (derived-mode-p 'python-mode)
              (electric-spacing-document?))
         (insert "%"))
        (t
         (electric-spacing-insert "%"))))

(defun electric-spacing-/ ()
  "See `electric-spacing-insert'."
  ;; *nix shebangs #!
  (cond ((and (eq 1 (line-number-at-pos))
              (save-excursion
                (move-beginning-of-line nil)
                (looking-at "#!")))
         (insert "/"))
        (t
         (electric-spacing-insert "/"))))


(defun electric-spacing-enclosing-paren ()
  "Return the opening parenthesis of the enclosing parens, or nil if not inside any parens."
  (interactive)
  (let ((ppss (syntax-ppss)))
    (when (nth 1 ppss)
      (char-after (nth 1 ppss)))))

(defun electric-spacing-python-: ()
  (if (and (not (in-string-p))
           (eq (electric-spacing-enclosing-paren) ?\{))
      (electric-spacing-insert ":" 'after)
    (insert ":")))

(defun electric-spacing-python-* ()
  ;; Handle python *args. Can only occur after '(' ',' or on a new line, so
  ;; just check for those. If it's just after a comma then also insert a
  ;; space before the *.
  (cond ((looking-back ",") (insert " *"))
        ((looking-back "[(,^)][ \t]*") (insert "*"))
        ;; Othewise act as normal
        (t (electric-spacing-insert "*"))))

(defun electric-spacing-python-** ()
  "See `electric-spacing-insert'."
  ;; Handle python **kwargs
  (cond ((looking-back ",")
         (insert " **"))

        ((looking-back "[(,^)][ \t]*")
         (insert "**"))

        (t
         (electric-spacing-insert "**"))))

(provide 'electric-spacing)

;;; electric-spacing.el ends here

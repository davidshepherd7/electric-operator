;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)


(defun helper-setup-c-main ()
  "Set up a C main function context."
  (insert "int main() {\n\n}\n")
  (goto-char (point-min))
  (forward-line 1))


;; Some simple cases
(ert-deftest c-ternary-operator ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (th-type "a?b:c")
    (th-should-see "a ? b : c")))

(ert-deftest c-label ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (th-type "error:")
    (th-should-see "error:")))

;; Pointer dereference
(ert-deftest c-space-> ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (th-type "a>b")
    (th-should-see "a > b")))

(ert-deftest c-space-- ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (th-type "a-b")
    (th-should-see "a - b")))

(ert-deftest c-dont-space--> ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (th-type "a->b")
    (th-should-see "a->b")))

;; Increment/decrement
(ert-deftest c-post-increment ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (th-type "b=c+a++")
    (th-should-see "b = c + a++")))

(ert-deftest c-pre-increment ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (th-type "b=++a+c")
    (th-should-see "b = ++a + c")))

(ert-deftest c-pre-increment-parens ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (th-type "while(++a < 2)")
    (th-should-see "while(++a < 2)")))

(ert-deftest c-pre-increment-at-beginning-of-line ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (insert "  ++a;\n  ")
    (th-type "++b;")
    (th-should-see-pattern "  \\+\\+b;$")))

(ert-deftest c-post-decrement ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (th-type "b=c-a--")
    (th-should-see "b = c - a--")))

(ert-deftest c-pre-decrement ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (th-type "b=--a-c")
    (th-should-see "b = --a - c")))

(ert-deftest c-pre-decrement-parens ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (th-type "while(--a < 2)")
    (th-should-see "while(--a < 2)")))

(ert-deftest c-pre-decrement-at-beginning-of-line ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (insert "  --a;\n  ")
    (th-type "--b;")
    (th-should-see-pattern "  --b;$")))

(ert-deftest c-post-increment-with-semi-colon ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (th-type "a++;")
    (th-should-see "a++;")))

(ert-deftest c-post-decrement-with-semi-colon ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (th-type "a--;")
    (th-should-see "a--;")))

;; * operator
(ert-deftest c-multiplication ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (th-type "a*b")
    (th-should-see "a * b")))

(ert-deftest c-pointer-type ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (th-type "char*a")
    (th-should-see "char *a")))

(ert-deftest c-const-pointer-to-const-type ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (th-type "const char const*a")
    (th-should-see "const char const *a")))

(ert-deftest c-non-builtin-pointer-type ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (th-type "size_t*a")
    (th-should-see "size_t *a")))

(ert-deftest c-assign-pointer-dereference ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (th-type "a=*b")
    (th-should-see "a = *b")))

(ert-deftest c-pointer-dereference-and-increment ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (th-type "*p++")
    (th-should-see "*p++")))

(ert-deftest c-function-call-with-dereference ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (th-type "f(*p,*q)")
    (th-should-see "f(*p, *q)")))

(ert-deftest c-pointer-to-pointer-type ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (th-type "char**a")
    (th-should-see "char **a")))

;; & operator
(ert-deftest c-bitwise-and ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (th-type "a&b")
    (th-should-see "a & b")))

(ert-deftest c-reference-type ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (th-type "char&a")
    (th-should-see "char &a")))

(ert-deftest c-assign-address-of ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (th-type "a=&b")
    (th-should-see "a = &b")))

(ert-deftest c-address-of-at-beginning-of-line ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (th-type "&a")
    (th-should-see-pattern "^&a$")))

(ert-deftest c-address-of-at-beginning-of-line-with-indentation ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (insert "  ")
    (th-type "&a")
    (th-should-see-pattern "  &a$")))

(ert-deftest c-function-call-with-address-of ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (th-type "f(&p,&q)")
    (th-should-see "f(&p, &q)")))

;; Respect option to have pointer operators touching the type or the variable name
(ert-deftest c-operator-*-on-type ()
  (th-fixtures #'c-mode
    (let ((electric-operator-c-pointer-type-style 'type))
      (helper-setup-c-main)
      (th-type "int*x")
      (th-should-see "int* x"))))

(ert-deftest c-operator-*-on-variable ()
  (th-fixtures #'c-mode
    (let ((electric-operator-c-pointer-type-style 'variable))
      (helper-setup-c-main)
      (th-type "int*x")
      (th-should-see "int *x"))))

(ert-deftest c-operator-&-on-type ()
  (th-fixtures #'c-mode
    (let ((electric-operator-c-pointer-type-style 'type))
      (helper-setup-c-main)
      (th-type "int&x")
      (th-should-see "int& x"))))

(ert-deftest c-operator-&-on-variable ()
  (th-fixtures #'c-mode
    (let ((electric-operator-c-pointer-type-style 'variable))
      (helper-setup-c-main)
      (th-type "int&x")
      (th-should-see "int &x"))))

(ert-deftest c-operator-**-on-type ()
  (th-fixtures #'c-mode
    (let ((electric-operator-c-pointer-type-style 'type))
      (helper-setup-c-main)
      (th-type "int**x")
      (th-should-see "int** x"))))

(ert-deftest c-operator-**-on-variable ()
  (th-fixtures #'c-mode
    (let ((electric-operator-c-pointer-type-style 'variable))
      (helper-setup-c-main)
      (th-type "int**x")
      (th-should-see "int **x"))))

;; Comments
(ert-deftest c-division-/-still-works ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (th-type "int a = x/y")
    (th-should-see "int a = x / y")))

(ert-deftest c-//-is-not-spaced-internally ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (th-type "//")
    (th-should-see "//")))

(ert-deftest c-//-adds-space-before-if-not-on-empty-line ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (th-type "expression;//")
    (th-should-see "expression; //")))

(ert-deftest c-//-does-not-add-space-before-when-at-indentation-of-line ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (insert "  expression;\n  ")
    (th-type "//")
    (th-should-see-pattern "  expression;$")
    (th-should-see-pattern "  // $")))

(ert-deftest c-/*-is-not-spaced-internally ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (th-type "/*")
    (th-should-see "/*")))

(ert-deftest c-/*-*/-is-not-spaced-internally ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (th-type "/**/")
    (th-should-see "/* */")))

(ert-deftest c-/*-adds-space-before-if-not-on-empty-line ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (th-type "expression;/*")
    (th-should-see "expression; /*")))

(ert-deftest c-/*-does-not-add-space-before-when-at-indentation-of-line ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (insert "  expression;\n  ")
    (th-type "/*")
    (th-should-see-pattern "  expression;$")
    (th-should-see-pattern "  /\\* $")))

;; Type keywords for pointers vs multiplication
(ert-deftest c-pointer-to-struct ()
  (th-fixtures #'c-mode
    (let ((electric-operator-c-pointer-type-style 'variable))
      (helper-setup-c-main)
      (th-type "struct s*foo")
      (th-should-see "struct s *foo"))))

(ert-deftest c-variable-with-struct-in-the-name ()
  (th-fixtures #'c-mode
    (let ((electric-operator-c-pointer-type-style 'variable))
      (helper-setup-c-main)
      (th-type "struct_ure*foo")
      (th-should-see "struct_ure * foo"))))

(ert-deftest c-another-variable-with-struct-in-the-name ()
  (th-fixtures #'c-mode
    (let ((electric-operator-c-pointer-type-style 'variable))
      (helper-setup-c-main)
      (th-type "a_struct*foo")
      (th-should-see "a_struct * foo"))))

(ert-deftest c-pointer-to-struct-then-multiply ()
  (th-fixtures #'c-mode
    (let ((electric-operator-c-pointer-type-style 'variable))
      (helper-setup-c-main)
      (th-type "struct s*foo=a*b")
      (th-should-see "struct s *foo = a * b"))))

(ert-deftest c-pointer-to-oddly-named-struct ()
  (th-fixtures #'c-mode
    (let ((electric-operator-c-pointer-type-style 'variable))
      (helper-setup-c-main)
      (th-type "struct my_type_2*foo")
      (th-should-see "struct my_type_2 *foo"))))

(ert-deftest c-pointer-to-union ()
  (th-fixtures #'c-mode
    (let ((electric-operator-c-pointer-type-style 'variable))
      (helper-setup-c-main)
      (th-type "union s*foo")
      (th-should-see "union s *foo"))))

(ert-deftest c-pointer-to-enum ()
  (th-fixtures #'c-mode
    (let ((electric-operator-c-pointer-type-style 'variable))
      (helper-setup-c-main)
      (th-type "enum s*foo=a*b")
      (th-should-see "enum s *foo = a * b"))))

(ert-deftest c-multiplication-with-pointer-deref ()
  (th-fixtures #'c-mode
    (helper-setup-c-main)
    (th-type "result = foo * *bar")
    (th-should-see "result = foo * *bar")))

;; Not all tested emacs versions have the grammar installed so we can't test
;;
;; (ert-deftest c-treesitter-mode-just-works ()
;;   (th-fixtures #'c-ts-mode
;;     (th-type "a-b")
;;     (th-should-see "a - b")))

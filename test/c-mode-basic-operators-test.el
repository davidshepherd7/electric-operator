;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)

;; Some simple cases
(ert-deftest c-ternary-operator ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (electric-operator-test-type "a?b:c")
    (electric-operator-test-should-see "a ? b : c")))

(ert-deftest c-label ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (electric-operator-test-type "error:")
    (electric-operator-test-should-see "error:")))

;; Pointer dereference
(ert-deftest c-space-> ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (electric-operator-test-type "a>b")
    (electric-operator-test-should-see "a > b")))

(ert-deftest c-space-- ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (electric-operator-test-type "a-b")
    (electric-operator-test-should-see "a - b")))

(ert-deftest c-dont-space--> ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (electric-operator-test-type "a->b")
    (electric-operator-test-should-see "a->b")))

;; Increment/decrement
(ert-deftest c-post-increment ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (electric-operator-test-type "b=c+a++")
    (electric-operator-test-should-see "b = c + a++")))

(ert-deftest c-pre-increment ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (electric-operator-test-type "b=++a+c")
    (electric-operator-test-should-see "b = ++a + c")))

(ert-deftest c-pre-increment-parens ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (electric-operator-test-type "while(++a < 2)")
    (electric-operator-test-should-see "while(++a < 2)")))

(ert-deftest c-pre-increment-at-beginning-of-line ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (insert "  ++a;\n  ")
    (electric-operator-test-type "++b;")
    (electric-operator-test-should-see-pattern "  \\+\\+b;$")))

(ert-deftest c-post-decrement ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (electric-operator-test-type "b=c-a--")
    (electric-operator-test-should-see "b = c - a--")))

(ert-deftest c-pre-decrement ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (electric-operator-test-type "b=--a-c")
    (electric-operator-test-should-see "b = --a - c")))

(ert-deftest c-pre-decrement-parens ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (electric-operator-test-type "while(--a < 2)")
    (electric-operator-test-should-see "while(--a < 2)")))

(ert-deftest c-pre-decrement-at-beginning-of-line ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (insert "  --a;\n  ")
    (electric-operator-test-type "--b;")
    (electric-operator-test-should-see-pattern "  --b;$")))

(ert-deftest c-post-increment-with-semi-colon ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (electric-operator-test-type "a++;")
    (electric-operator-test-should-see "a++;")))

(ert-deftest c-post-decrement-with-semi-colon ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (electric-operator-test-type "a--;")
    (electric-operator-test-should-see "a--;")))

;; * operator
(ert-deftest c-multiplication ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (electric-operator-test-type "a*b")
    (electric-operator-test-should-see "a * b")))

(ert-deftest c-pointer-type ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (electric-operator-test-type "char*a")
    (electric-operator-test-should-see "char *a")))

(ert-deftest c-const-pointer-to-const-type ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (electric-operator-test-type "const char const*a")
    (electric-operator-test-should-see "const char const *a")))

(ert-deftest c-non-builtin-pointer-type ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (electric-operator-test-type "size_t*a")
    (electric-operator-test-should-see "size_t *a")))

(ert-deftest c-assign-pointer-dereference ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (electric-operator-test-type "a=*b")
    (electric-operator-test-should-see "a = *b")))

(ert-deftest c-pointer-dereference-and-increment ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (electric-operator-test-type "*p++")
    (electric-operator-test-should-see "*p++")))

(ert-deftest c-function-call-with-dereference ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (electric-operator-test-type "f(*p,*q)")
    (electric-operator-test-should-see "f(*p, *q)")))

(ert-deftest c-pointer-to-pointer-type ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (electric-operator-test-type "char**a")
    (electric-operator-test-should-see "char **a")))

;; & operator
(ert-deftest c-bitwise-and ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (electric-operator-test-type "a&b")
    (electric-operator-test-should-see "a & b")))

(ert-deftest c-reference-type ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (electric-operator-test-type "char&a")
    (electric-operator-test-should-see "char &a")))

(ert-deftest c-assign-address-of ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (electric-operator-test-type "a=&b")
    (electric-operator-test-should-see "a = &b")))

(ert-deftest c-address-of-at-beginning-of-line ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (electric-operator-test-type "&a")
    (electric-operator-test-should-see-pattern "^&a$")))

(ert-deftest c-address-of-at-beginning-of-line-with-indentation ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (insert "  ")
    (electric-operator-test-type "&a")
    (electric-operator-test-should-see-pattern "  &a$")))

(ert-deftest c-function-call-with-address-of ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (electric-operator-test-type "f(&p,&q)")
    (electric-operator-test-should-see "f(&p, &q)")))

;; Respect option to have pointer operators touching the type or the variable name
(ert-deftest c-operator-*-on-type ()
  (test-with-mode c-mode
    (let ((electric-operator-c-pointer-type-style 'type))
      (electric-operator-test-setup-c-main)
      (electric-operator-test-type "int*x")
      (electric-operator-test-should-see "int* x"))))

(ert-deftest c-operator-*-on-variable ()
  (test-with-mode c-mode
    (let ((electric-operator-c-pointer-type-style 'variable))
      (electric-operator-test-setup-c-main)
      (electric-operator-test-type "int*x")
      (electric-operator-test-should-see "int *x"))))

(ert-deftest c-operator-&-on-type ()
  (test-with-mode c-mode
    (let ((electric-operator-c-pointer-type-style 'type))
      (electric-operator-test-setup-c-main)
      (electric-operator-test-type "int&x")
      (electric-operator-test-should-see "int& x"))))

(ert-deftest c-operator-&-on-variable ()
  (test-with-mode c-mode
    (let ((electric-operator-c-pointer-type-style 'variable))
      (electric-operator-test-setup-c-main)
      (electric-operator-test-type "int&x")
      (electric-operator-test-should-see "int &x"))))

(ert-deftest c-operator-**-on-type ()
  (test-with-mode c-mode
    (let ((electric-operator-c-pointer-type-style 'type))
      (electric-operator-test-setup-c-main)
      (electric-operator-test-type "int**x")
      (electric-operator-test-should-see "int** x"))))

(ert-deftest c-operator-**-on-variable ()
  (test-with-mode c-mode
    (let ((electric-operator-c-pointer-type-style 'variable))
      (electric-operator-test-setup-c-main)
      (electric-operator-test-type "int**x")
      (electric-operator-test-should-see "int **x"))))

;; Comments
(ert-deftest c-division-/-still-works ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (electric-operator-test-type "int a = x/y")
    (electric-operator-test-should-see "int a = x / y")))

(ert-deftest c-//-is-not-spaced-internally ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (electric-operator-test-type "//")
    (electric-operator-test-should-see "//")))

(ert-deftest c-//-adds-space-before-if-not-on-empty-line ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (electric-operator-test-type "expression;//")
    (electric-operator-test-should-see "expression; //")))

(ert-deftest c-//-does-not-add-space-before-when-at-indentation-of-line ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (insert "  expression;\n  ")
    (electric-operator-test-type "//")
    (electric-operator-test-should-see-pattern "  expression;$")
    (electric-operator-test-should-see-pattern "  // $")))

(ert-deftest c-/*-is-not-spaced-internally ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (electric-operator-test-type "/*")
    (electric-operator-test-should-see "/*")))

(ert-deftest c-/*-*/-is-not-spaced-internally ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (electric-operator-test-type "/**/")
    (electric-operator-test-should-see "/* */")))

(ert-deftest c-/*-adds-space-before-if-not-on-empty-line ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (electric-operator-test-type "expression;/*")
    (electric-operator-test-should-see "expression; /*")))

(ert-deftest c-/*-does-not-add-space-before-when-at-indentation-of-line ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (insert "  expression;\n  ")
    (electric-operator-test-type "/*")
    (electric-operator-test-should-see-pattern "  expression;$")
    (electric-operator-test-should-see-pattern "  /\\* $")))

;; Type keywords for pointers vs multiplication
(ert-deftest c-pointer-to-struct ()
  (test-with-mode c-mode
    (let ((electric-operator-c-pointer-type-style 'variable))
      (electric-operator-test-setup-c-main)
      (electric-operator-test-type "struct s*foo")
      (electric-operator-test-should-see "struct s *foo"))))

(ert-deftest c-variable-with-struct-in-the-name ()
  (test-with-mode c-mode
    (let ((electric-operator-c-pointer-type-style 'variable))
      (electric-operator-test-setup-c-main)
      (electric-operator-test-type "struct_ure*foo")
      (electric-operator-test-should-see "struct_ure * foo"))))

(ert-deftest c-another-variable-with-struct-in-the-name ()
  (test-with-mode c-mode
    (let ((electric-operator-c-pointer-type-style 'variable))
      (electric-operator-test-setup-c-main)
      (electric-operator-test-type "a_struct*foo")
      (electric-operator-test-should-see "a_struct * foo"))))

(ert-deftest c-pointer-to-struct-then-multiply ()
  (test-with-mode c-mode
    (let ((electric-operator-c-pointer-type-style 'variable))
      (electric-operator-test-setup-c-main)
      (electric-operator-test-type "struct s*foo=a*b")
      (electric-operator-test-should-see "struct s *foo = a * b"))))

(ert-deftest c-pointer-to-oddly-named-struct ()
  (test-with-mode c-mode
    (let ((electric-operator-c-pointer-type-style 'variable))
      (electric-operator-test-setup-c-main)
      (electric-operator-test-type "struct my_type_2*foo")
      (electric-operator-test-should-see "struct my_type_2 *foo"))))

(ert-deftest c-pointer-to-union ()
  (test-with-mode c-mode
    (let ((electric-operator-c-pointer-type-style 'variable))
      (electric-operator-test-setup-c-main)
      (electric-operator-test-type "union s*foo")
      (electric-operator-test-should-see "union s *foo"))))

(ert-deftest c-pointer-to-enum ()
  (test-with-mode c-mode
    (let ((electric-operator-c-pointer-type-style 'variable))
      (electric-operator-test-setup-c-main)
      (electric-operator-test-type "enum s*foo=a*b")
      (electric-operator-test-should-see "enum s *foo = a * b"))))

(ert-deftest c-multiplication-with-pointer-deref ()
  (test-with-mode c-mode
    (electric-operator-test-setup-c-main)
    (electric-operator-test-type "result = foo * *bar")
    (electric-operator-test-should-see "result = foo * *bar")))

;; Known failure test (commented out)
;; (ert-deftest c-treesitter-mode-just-works ()
;;   (test-with-mode c-ts-mode
;;     (electric-operator-test-type "a-b")
;;     (electric-operator-test-should-see "a - b")))
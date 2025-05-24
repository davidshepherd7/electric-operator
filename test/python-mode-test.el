;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'electric-operator)
(require 'test-helper)

(ert-deftest python-dont-modify-string-literal ()
  (th-fixtures #'python-mode
    (let ((electric-operator-enable-in-docs nil))
      (th-type "'var+foo-1'")
      (th-should-see "'var+foo-1'"))))

(ert-deftest python-dont-modify-string-literal-after-operator ()
  (th-fixtures #'python-mode
    (th-type "a+''")
    (th-should-see "a + ''")))

;; * and ** operators
(ert-deftest python-space-* ()
  (th-fixtures #'python-mode
    (th-type "a*b")
    (th-should-see "a * b")))

(ert-deftest python-multiplication-after-a-function ()
  (th-fixtures #'python-mode
    (th-type "f(x)*a")
    (th-should-see "f(x) * a")))

(ert-deftest python-exponentiation-after-a-function ()
  (th-fixtures #'python-mode
    (th-type "f(x)**a")
    (th-should-see "f(x) ** a")))

(ert-deftest python-space-*-inside-function ()
  (th-fixtures #'python-mode
    (th-type "f(a*b)")
    (th-should-see "f(a * b)")))

(ert-deftest python-space-** ()
  (th-fixtures #'python-mode
    (th-type "a**b")
    (th-should-see "a ** b")))

(ert-deftest python-space-**-inside-function ()
  (th-fixtures #'python-mode
    (th-type "f(a**b)")
    (th-should-see "f(a ** b)")))

(ert-deftest python-kwargs-expansion-into-dict ()
  (th-fixtures #'python-mode
    (th-type "a = {**x,**y}")
    (th-should-see "a = {**x, **y}")))

;; Check *args works ok
(ert-deftest python-space-*args-on-its-own ()
  (th-fixtures #'python-mode
    (th-type "f(*args)")
    (th-should-see "f(*args)")))

(ert-deftest python-space-*args-with-other-args ()
  (th-fixtures #'python-mode
    (th-type "f(a,*args)")
    (th-should-see "f(a, *args)")))

(ert-deftest python-space-*args-with-a-newline-before-it ()
  (th-fixtures #'python-mode
    (insert "f(a,\n")
    (th-type "*args)")
    (th-should-see "f(a,\n*args)")))

;; And **kwargs
(ert-deftest python-space-**kwargs-on-its-own ()
  (th-fixtures #'python-mode
    (th-type "f(**kwargs)")
    (th-should-see "f(**kwargs)")))

(ert-deftest python-space-**kwargs-with-other-args ()
  (th-fixtures #'python-mode
    (th-type "f(a,**kwargs)")
    (th-should-see "f(a, **kwargs)")))

(ert-deftest python-space-**kwargs-with-a-newline-before-it ()
  (th-fixtures #'python-mode
    (insert "f(a,\n")
    (th-type "**kwargs)")
    (th-should-see "f(a,\n**kwargs)")))

;; Colons

;; keywords
(ert-deftest python-dont-space-:-in-ifs ()
  (th-fixtures #'python-mode
    (th-type "if x:")
    (th-should-not-see "if x: ")))

(ert-deftest python-leave-any-existing-space-after-:-in-ifs ()
  (th-fixtures #'python-mode
    (th-type "if x: ")
    (th-should-see "if x: ")))

(ert-deftest python-dont-space-:-in-else ()
  (th-fixtures #'python-mode
    (th-type "else:")
    (th-should-not-see "else: ")))

(ert-deftest python-dont-space-:-in-elif ()
  (th-fixtures #'python-mode
    (th-type "elif y:")
    (th-should-not-see "elif y: ")))

(ert-deftest python-dont-space-:-in-for ()
  (th-fixtures #'python-mode
               (th-type "for x in y:")
               (th-should-not-see "for x in y: ")))

(ert-deftest python-dont-space-:-in-while ()
  (th-fixtures #'python-mode
               (th-type "while x:")
               (th-should-not-see "while x: ")))

(ert-deftest python-dont-space-:-in-try ()
  (th-fixtures #'python-mode
               (th-type "try:")
               (th-should-not-see "try: ")))

(ert-deftest python-dont-space-:-in-with ()
  (th-fixtures #'python-mode
               (th-type "with X as Y:")
               (th-should-not-see "with X as Y: ")))

(ert-deftest python-colon-after-multiline-def-is-not-spaced ()
  (th-fixtures #'python-mode
               (th-type "    ):")
               (th-should-not-see "): ")))

;; python dictionaries
(ert-deftest python-space-after-:-in-dict ()
  (th-fixtures #'python-mode
    (th-type "{a:1}")
    (th-should-see "{a: 1}")))

;; Lambda functions
(ert-deftest python-space-after-lambda-arguments ()
  (th-fixtures #'python-mode
    (th-type "lambda x:x")
    (th-should-see "lambda x: x")))

(ert-deftest python-space-after-lambda-arguments-inside-dict ()
  (th-fixtures #'python-mode
    (th-type "{a:lambda x:x, b:2}")
    (th-should-see "{a: lambda x: x, b: 2}")))

(ert-deftest python-lambda-containing-dict ()
  (th-fixtures #'python-mode
    (th-type "lambda x:{a:x, b:2}")
    (th-should-see "lambda x: {a: x, b: 2}")))

(ert-deftest python-lambda-containing-slice ()
  (th-fixtures #'python-mode
    (th-type "lambda x:x[1:2]")
    (th-should-see "lambda x: x[1:2]")))

(ert-deftest python-lambda-with-default-argument-containing-dict ()
  :expected-result :failed
  (th-fixtures #'python-mode
    (th-type "lambda x={a:1}:print x")
    (th-should-see "lambda x={a: 1}: print x")))

(ert-deftest python-lambda-with-default-argument-containing-slice ()
  :expected-result :failed
  (th-fixtures #'python-mode
    (th-type "lambda x=y[1:5]:print x")
    (th-should-see "lambda x=y[1:5]: print x")))

(ert-deftest python-lambda-inside-another-word-doesnt-affect-spacing ()
  (th-fixtures #'python-mode
    (th-type "foolambdabar=1")
    (th-should-see "foolambdabar = 1")))

(ert-deftest python-lambda-at-the-start-of-another-word-doesnt-affect-spacing ()
  (th-fixtures #'python-mode
    (th-type "lambdabar=1")
    (th-should-see "lambdabar = 1")))

(ert-deftest python-lambda-inside-another-word-with-_-doesnt-affect-spacing ()
  (th-fixtures #'python-mode
    (th-type "foo_lambdabar=1")
    (th-should-see "foo_lambdabar = 1")))

(ert-deftest python-zero-argument-lambda ()
  (th-fixtures #'python-mode
    (th-type "lambda: x, y=1")
    (th-should-see "lambda: x, y = 1")))

(ert-deftest python-space-after-lambda-arguments-even-inside-a-function ()
  (th-fixtures #'python-mode
    (th-type "foo(lambda x:x")
    (th-should-see "foo(lambda x: x")))

;; Slice operator
(ert-deftest python-dont-space-:-inside-slices ()
  (th-fixtures #'python-mode
    (th-type "a[1:2]")
    (th-should-see "a[1:2]")))

(ert-deftest python-dont-space-negative-slices ()
  (th-fixtures #'python-mode
    (th-type "a[-1:-2]")
    (th-should-see "a[-1:-2]")))

;; Types
(ert-deftest python-types-in-function-declarations ()
  (th-fixtures #'python-mode
               (th-type "def foo(x:int)->str:")
               (th-should-see "def foo(x: int) -> str:")
               (th-should-not-see "str: ")))

(ert-deftest python-types-in-variable-declarations ()
  (th-fixtures #'python-mode
    (th-type "self._first_name:str = first_name")
    (th-should-see "self._first_name: str = first_name")))

(ert-deftest python-types-in-variable-declarations-with-names-ending-in-keywords ()
  (th-fixtures #'python-mode
    (th-type "is_first_try:str =")
    (th-should-see "is_first_try: str =")))

;; Member access
(ert-deftest python-dont-space-accessing-class-members ()
  (th-fixtures #'python-mode
    (th-type "my_class.a")
    (th-should-see "my_class.a")))

;; Keyword argument =
(ert-deftest python-space-standard-assignment-as-normal ()
  (th-fixtures #'python-mode
    (th-type "a=b")
    (th-should-see "a = b")))

(ert-deftest python-dont-space-assignment-inside-function-call ()
  (th-fixtures #'python-mode
    (th-type "f(a=b)")
    (th-should-see "f(a=b)")))

(ert-deftest python-dont-space-default-args-in-lambda ()
  (th-fixtures #'python-mode
    (th-type "lambda x=1: print x")
    (th-should-see "lambda x=1: print x")))

(ert-deftest python-default-arguments-after-types-in-function-declarations ()
  (th-fixtures #'python-mode
    (th-type "def foo(x:int=timedelta(days=1))")
    (th-should-see "def foo(x: int = timedelta(days=1))")))

(ert-deftest python-default-arguments-after-compound-types-in-function-declarations ()
  (th-fixtures #'python-mode
    (th-type "def foo(x: Tuple[int,int,int]=timedelta(days=1))")
    (th-should-see "def foo(x: Tuple[int, int, int] = timedelta(days=1))")))

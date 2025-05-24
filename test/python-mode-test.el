;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)

(ert-deftest python-dont-modify-string-literal ()
  (test-with-mode python-mode
    (let ((electric-operator-enable-in-docs nil))
      (electric-operator-test-type "'var+foo-1'")
      (electric-operator-test-should-see "'var+foo-1'"))))

(ert-deftest python-dont-modify-string-literal-after-operator ()
  (test-with-mode python-mode
    (electric-operator-test-type "a+''")
    (electric-operator-test-should-see "a + ''")))

;; * and ** operators
(ert-deftest python-space-* ()
  (test-with-mode python-mode
    (electric-operator-test-type "a*b")
    (electric-operator-test-should-see "a * b")))

(ert-deftest python-multiplication-after-a-function ()
  (test-with-mode python-mode
    (electric-operator-test-type "f(x)*a")
    (electric-operator-test-should-see "f(x) * a")))

(ert-deftest python-exponentiation-after-a-function ()
  (test-with-mode python-mode
    (electric-operator-test-type "f(x)**a")
    (electric-operator-test-should-see "f(x) ** a")))

(ert-deftest python-space-*-inside-function ()
  (test-with-mode python-mode
    (electric-operator-test-type "f(a*b)")
    (electric-operator-test-should-see "f(a * b)")))

(ert-deftest python-space-** ()
  (test-with-mode python-mode
    (electric-operator-test-type "a**b")
    (electric-operator-test-should-see "a ** b")))

(ert-deftest python-space-**-inside-function ()
  (test-with-mode python-mode
    (electric-operator-test-type "f(a**b)")
    (electric-operator-test-should-see "f(a ** b)")))

(ert-deftest python-kwargs-expansion-into-dict ()
  (test-with-mode python-mode
    (electric-operator-test-type "a = {**x,**y}")
    (electric-operator-test-should-see "a = {**x, **y}")))

;; Check *args works ok
(ert-deftest python-space-*args-on-its-own ()
  (test-with-mode python-mode
    (electric-operator-test-type "f(*args)")
    (electric-operator-test-should-see "f(*args)")))

(ert-deftest python-space-*args-with-other-args ()
  (test-with-mode python-mode
    (electric-operator-test-type "f(a,*args)")
    (electric-operator-test-should-see "f(a, *args)")))

(ert-deftest python-space-*args-with-a-newline-before-it ()
  (test-with-mode python-mode
    (insert "f(a,\n")
    (electric-operator-test-type "*args)")
    (electric-operator-test-should-see "f(a,\n*args)")))

;; And **kwargs
(ert-deftest python-space-**kwargs-on-its-own ()
  (test-with-mode python-mode
    (electric-operator-test-type "f(**kwargs)")
    (electric-operator-test-should-see "f(**kwargs)")))

(ert-deftest python-space-**kwargs-with-other-args ()
  (test-with-mode python-mode
    (electric-operator-test-type "f(a,**kwargs)")
    (electric-operator-test-should-see "f(a, **kwargs)")))

(ert-deftest python-space-**kwargs-with-a-newline-before-it ()
  (test-with-mode python-mode
    (insert "f(a,\n")
    (electric-operator-test-type "**kwargs)")
    (electric-operator-test-should-see "f(a,\n**kwargs)")))

;; Colons

;; keywords
(ert-deftest python-dont-space-:-in-ifs ()
  (test-with-mode python-mode
    (electric-operator-test-type "if x:")
    (electric-operator-test-should-not-see "if x: ")))

(ert-deftest python-leave-any-existing-space-after-:-in-ifs ()
  (test-with-mode python-mode
    (electric-operator-test-type "if x: ")
    (electric-operator-test-should-see "if x: ")))

(ert-deftest python-dont-space-:-in-else ()
  (test-with-mode python-mode
    (electric-operator-test-type "else:")
    (electric-operator-test-should-not-see "else: ")))

(ert-deftest python-dont-space-:-in-elif ()
  (test-with-mode python-mode
    (electric-operator-test-type "elif y:")
    (electric-operator-test-should-not-see "elif y: ")))

(ert-deftest python-dont-space-:-in-for ()
  (test-with-mode python-mode
    (electric-operator-test-type "for x in y:")
    (electric-operator-test-should-not-see "for x in y: ")))

(ert-deftest python-dont-space-:-in-while ()
  (test-with-mode python-mode
    (electric-operator-test-type "while x:")
    (electric-operator-test-should-not-see "while x: ")))

(ert-deftest python-dont-space-:-in-try ()
  (test-with-mode python-mode
    (electric-operator-test-type "try:")
    (electric-operator-test-should-not-see "try: ")))

(ert-deftest python-dont-space-:-in-with ()
  (test-with-mode python-mode
    (electric-operator-test-type "with X as Y:")
    (electric-operator-test-should-not-see "with X as Y: ")))

(ert-deftest python-colon-after-multiline-def-is-not-spaced ()
  (test-with-mode python-mode
    (electric-operator-test-type "    ):")
    (electric-operator-test-should-not-see "): ")))

;; python dictionaries
(ert-deftest python-space-after-:-in-dict ()
  (test-with-mode python-mode
    (electric-operator-test-type "{a:1}")
    (electric-operator-test-should-see "{a: 1}")))

;; Lambda functions
(ert-deftest python-space-after-lambda-arguments ()
  (test-with-mode python-mode
    (electric-operator-test-type "lambda x:x")
    (electric-operator-test-should-see "lambda x: x")))

(ert-deftest python-space-after-lambda-arguments-inside-dict ()
  (test-with-mode python-mode
    (electric-operator-test-type "{a:lambda x:x, b:2}")
    (electric-operator-test-should-see "{a: lambda x: x, b: 2}")))

(ert-deftest python-lambda-containing-dict ()
  (test-with-mode python-mode
    (electric-operator-test-type "lambda x:{a:x, b:2}")
    (electric-operator-test-should-see "lambda x: {a: x, b: 2}")))

(ert-deftest python-lambda-containing-slice ()
  (test-with-mode python-mode
    (electric-operator-test-type "lambda x:x[1:2]")
    (electric-operator-test-should-see "lambda x: x[1:2]")))

;; Known failure tests (commented out)
;; (ert-deftest python-lambda-with-default-argument-containing-dict ()
;;   (test-with-mode python-mode
;;     (electric-operator-test-type "lambda x={a:1}:print x")
;;     (electric-operator-test-should-see "lambda x={a: 1}: print x")))

;; (ert-deftest python-lambda-with-default-argument-containing-slice ()
;;   (test-with-mode python-mode
;;     (electric-operator-test-type "lambda x=y[1:5]:print x")
;;     (electric-operator-test-should-see "lambda x=y[1:5]: print x")))

(ert-deftest python-lambda-inside-another-word-doesnt-affect-spacing ()
  (test-with-mode python-mode
    (electric-operator-test-type "foolambdabar=1")
    (electric-operator-test-should-see "foolambdabar = 1")))

(ert-deftest python-lambda-at-the-start-of-another-word-doesnt-affect-spacing ()
  (test-with-mode python-mode
    (electric-operator-test-type "lambdabar=1")
    (electric-operator-test-should-see "lambdabar = 1")))

(ert-deftest python-lambda-inside-another-word-with-_-doesnt-affect-spacing ()
  (test-with-mode python-mode
    (electric-operator-test-type "foo_lambdabar=1")
    (electric-operator-test-should-see "foo_lambdabar = 1")))

(ert-deftest python-zero-argument-lambda ()
  (test-with-mode python-mode
    (electric-operator-test-type "lambda: x, y=1")
    (electric-operator-test-should-see "lambda: x, y = 1")))

(ert-deftest python-space-after-lambda-arguments-even-inside-a-function ()
  (test-with-mode python-mode
    (electric-operator-test-type "foo(lambda x:x")
    (electric-operator-test-should-see "foo(lambda x: x")))

;; Slice operator
(ert-deftest python-dont-space-:-inside-slices ()
  (test-with-mode python-mode
    (electric-operator-test-type "a[1:2]")
    (electric-operator-test-should-see "a[1:2]")))

(ert-deftest python-dont-space-negative-slices ()
  (test-with-mode python-mode
    (electric-operator-test-type "a[-1:-2]")
    (electric-operator-test-should-see "a[-1:-2]")))

;; Types
(ert-deftest python-types-in-function-declarations ()
  (test-with-mode python-mode
    (electric-operator-test-type "def foo(x:int)->str:")
    (electric-operator-test-should-see "def foo(x: int) -> str:")
    (electric-operator-test-should-not-see "str: ")))

(ert-deftest python-types-in-variable-declarations ()
  (test-with-mode python-mode
    (electric-operator-test-type "self._first_name:str = first_name")
    (electric-operator-test-should-see "self._first_name: str = first_name")))

(ert-deftest python-types-in-variable-declarations-with-names-ending-in-keywords ()
  (test-with-mode python-mode
    (electric-operator-test-type "is_first_try:str =")
    (electric-operator-test-should-see "is_first_try: str =")))

;; Member access
(ert-deftest python-dont-space-accessing-class-members ()
  (test-with-mode python-mode
    (electric-operator-test-type "my_class.a")
    (electric-operator-test-should-see "my_class.a")))

;; Keyword argument =
(ert-deftest python-space-standard-assignment-as-normal ()
  (test-with-mode python-mode
    (electric-operator-test-type "a=b")
    (electric-operator-test-should-see "a = b")))

(ert-deftest python-dont-space-assignment-inside-function-call ()
  (test-with-mode python-mode
    (electric-operator-test-type "f(a=b)")
    (electric-operator-test-should-see "f(a=b)")))

(ert-deftest python-dont-space-default-args-in-lambda ()
  (test-with-mode python-mode
    (electric-operator-test-type "lambda x=1: print x")
    (electric-operator-test-should-see "lambda x=1: print x")))

(ert-deftest python-default-arguments-after-types-in-function-declarations ()
  (test-with-mode python-mode
    (electric-operator-test-type "def foo(x:int=timedelta(days=1))")
    (electric-operator-test-should-see "def foo(x: int = timedelta(days=1))")))

(ert-deftest python-default-arguments-after-compound-types-in-function-declarations ()
  (test-with-mode python-mode
    (electric-operator-test-type "def foo(x: Tuple[int,int,int]=timedelta(days=1))")
    (electric-operator-test-should-see "def foo(x: Tuple[int, int, int] = timedelta(days=1))")))

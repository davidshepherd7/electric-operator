;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'electric-operator)
(require 'test-helper)
(require 'rust-mode)


(defun helper-setup-rust-function ()
  "Set up a Rust function context."
  (insert "fn foo() -> i32 {\n\n}\n")
  (goto-char (point-min))
  (forward-line 1))


(ert-deftest rust-comments ()
  (th-fixtures #'rust-mode
    (th-type "//a comment")
    (th-should-see "// a comment")))

(ert-deftest rust-doc-comments ()
  (th-fixtures #'rust-mode
    (th-type "///a comment")
    (th-should-see "/// a comment")))

(ert-deftest rust-division-slash-still-works ()
  (th-fixtures #'rust-mode
    (th-type "int a = x/y")
    (th-should-see "int a = x / y")))

(ert-deftest rust-double-slash-adds-space-before-if-not-on-empty-line ()
  (th-fixtures #'rust-mode
    (th-type "expression;//")
    (th-should-see "expression; //")))

(ert-deftest rust-double-slash-does-not-add-space-before-when-at-indentation-of-line ()
  (th-fixtures #'rust-mode
    (th-type "   //")
    (th-should-see-pattern "^   // $")))

(ert-deftest rust-slash-star-adds-space-before-if-not-on-empty-line ()
  (th-fixtures #'rust-mode
    (th-type "expression;/*")
    (th-should-see "expression; /*")))

(ert-deftest rust-slash-star-does-not-add-space-before-when-at-indentation-of-line ()
  (th-fixtures #'rust-mode
    (th-type "   /*")
    (th-should-see-pattern "^   /\\* $")))

(ert-deftest rust-function-return-value ()
  (th-fixtures #'rust-mode
    (th-type "fn foo()->i32")
    (th-should-see "fn foo() -> i32")))

(ert-deftest rust-pattern-matching-fat-arrow ()
  (th-fixtures #'rust-mode
    (helper-setup-rust-function)
    (insert "match x {\n")
    (th-type "1=>pri")
    (th-should-see "1 => pri")))


;;; Angle brackets as generic/template brackets

;; Turbofish (::< in expression context)
(ert-deftest rust-turbofish-parse ()
  (th-fixtures #'rust-mode
    (helper-setup-rust-function)
    (th-type "let x=foo::<i32>()")
    (th-should-see "let x = foo::<i32>()")))

(ert-deftest rust-turbofish-collect ()
  :expected-result :failed
  (th-fixtures #'rust-mode
    (helper-setup-rust-function)
    (th-type "let v:Vec<i32>=iter.collect::<Vec<i32>>()")
    (th-should-see "let v: Vec<i32> = iter.collect::<Vec<i32>>()")))

(ert-deftest rust-turbofish-multiple-type-args ()
  (th-fixtures #'rust-mode
    (helper-setup-rust-function)
    (th-type "let x=foo::<i32, u64>()")
    (th-should-see "let x = foo::<i32, u64>()")))

;; Type annotations after :
(ert-deftest rust-type-annotation-simple ()
  :expected-result :failed
  (th-fixtures #'rust-mode
    (helper-setup-rust-function)
    (th-type "let x:Vec<i32>=vec![]")
    (th-should-see "let x: Vec<i32> = vec![]")))

(ert-deftest rust-type-annotation-option ()
  :expected-result :failed
  (th-fixtures #'rust-mode
    (helper-setup-rust-function)
    (th-type "let x:Option<String>=None")
    (th-should-see "let x: Option<String> = None")))

(ert-deftest rust-type-annotation-result ()
  :expected-result :failed
  (th-fixtures #'rust-mode
    (helper-setup-rust-function)
    (th-type "let x:Result<i32, String>=Ok(1)")
    (th-should-see "let x: Result<i32, String> = Ok(1)")))

(ert-deftest rust-type-annotation-nested-generics ()
  :expected-result :failed
  (th-fixtures #'rust-mode
    (helper-setup-rust-function)
    (th-type "let x:Vec<Option<i32>>=vec![]")
    (th-should-see "let x: Vec<Option<i32>> = vec![]")))

(ert-deftest rust-type-annotation-hashmap ()
  :expected-result :failed
  (th-fixtures #'rust-mode
    (helper-setup-rust-function)
    (th-type "let m:HashMap<String, Vec<i32>>=HashMap::new()")
    (th-should-see "let m: HashMap<String, Vec<i32>> = HashMap::new()")))

;; Return type after ->
(ert-deftest rust-return-type-generic ()
  (th-fixtures #'rust-mode
    (th-type "fn bar()->Vec<i32>")
    (th-should-see "fn bar() -> Vec<i32>")))

(ert-deftest rust-return-type-result ()
  (th-fixtures #'rust-mode
    (th-type "fn bar()->Result<String, Error>")
    (th-should-see "fn bar() -> Result<String, Error>")))

(ert-deftest rust-return-type-option ()
  (th-fixtures #'rust-mode
    (th-type "fn bar()->Option<i32>")
    (th-should-see "fn bar() -> Option<i32>")))

;; Function definition type parameters
(ert-deftest rust-fn-type-param ()
  :expected-result :failed
  (th-fixtures #'rust-mode
    (th-type "fn foo<T>(x:T)->T")
    (th-should-see "fn foo<T>(x: T) -> T")))

(ert-deftest rust-fn-multiple-type-params ()
  :expected-result :failed
  (th-fixtures #'rust-mode
    (th-type "fn foo<T, U>(x:T, y:U)->T")
    (th-should-see "fn foo<T, U>(x: T, y: U) -> T")))

(ert-deftest rust-fn-type-param-with-bound ()
  :expected-result :failed
  (th-fixtures #'rust-mode
    (th-type "fn foo<T:Clone>(x:T)->T")
    (th-should-see "fn foo<T: Clone>(x: T) -> T")))

;; Struct definitions
(ert-deftest rust-struct-type-param ()
  (th-fixtures #'rust-mode
    (th-type "struct Foo<T>")
    (th-should-see "struct Foo<T>")))

(ert-deftest rust-struct-multiple-type-params ()
  (th-fixtures #'rust-mode
    (th-type "struct Foo<T, U>")
    (th-should-see "struct Foo<T, U>")))

;; Enum definitions
(ert-deftest rust-enum-type-param ()
  (th-fixtures #'rust-mode
    (th-type "enum MyOption<T>")
    (th-should-see "enum MyOption<T>")))

;; Impl blocks
(ert-deftest rust-impl-type-param ()
  (th-fixtures #'rust-mode
    (th-type "impl<T> Foo<T>")
    (th-should-see "impl<T> Foo<T>")))

(ert-deftest rust-impl-concrete-type ()
  (th-fixtures #'rust-mode
    (th-type "impl Foo<i32>")
    (th-should-see "impl Foo<i32>")))

(ert-deftest rust-impl-trait-for-type ()
  (th-fixtures #'rust-mode
    (th-type "impl<T> Clone for Foo<T>")
    (th-should-see "impl<T> Clone for Foo<T>")))

;; Trait definitions
(ert-deftest rust-trait-type-param ()
  (th-fixtures #'rust-mode
    (th-type "trait MyTrait<T>")
    (th-should-see "trait MyTrait<T>")))

;; Where clauses
(ert-deftest rust-where-clause ()
  :expected-result :failed
  (th-fixtures #'rust-mode
    (th-type "where T:Iterator<Item=i32>")
    (th-should-see "where T: Iterator<Item = i32>")))

;; Function argument types
(ert-deftest rust-fn-arg-generic-type ()
  :expected-result :failed
  (th-fixtures #'rust-mode
    (th-type "fn foo(x:Vec<i32>)")
    (th-should-see "fn foo(x: Vec<i32>)")))

(ert-deftest rust-fn-arg-reference-generic ()
  :expected-result :failed
  (th-fixtures #'rust-mode
    (th-type "fn foo(x:&Vec<i32>)")
    (th-should-see "fn foo(x: &Vec<i32>)")))


;;; Angle brackets as comparison operators

(ert-deftest rust-less-than ()
  :expected-result :failed
  (th-fixtures #'rust-mode
    (helper-setup-rust-function)
    (th-type "let b=x<y")
    (th-should-see "let b = x < y")))

(ert-deftest rust-greater-than ()
  :expected-result :failed
  (th-fixtures #'rust-mode
    (helper-setup-rust-function)
    (th-type "let b=x>y")
    (th-should-see "let b = x > y")))

(ert-deftest rust-less-than-number ()
  :expected-result :failed
  (th-fixtures #'rust-mode
    (helper-setup-rust-function)
    (th-type "let b=1<2")
    (th-should-see "let b = 1 < 2")))

(ert-deftest rust-greater-than-number ()
  :expected-result :failed
  (th-fixtures #'rust-mode
    (helper-setup-rust-function)
    (th-type "let b=1>2")
    (th-should-see "let b = 1 > 2")))

(ert-deftest rust-comparison-in-if ()
  :expected-result :failed
  (th-fixtures #'rust-mode
    (helper-setup-rust-function)
    (th-type "if x<10")
    (th-should-see "if x < 10")))

(ert-deftest rust-comparison-in-while ()
  :expected-result :failed
  (th-fixtures #'rust-mode
    (helper-setup-rust-function)
    (th-type "while i<len")
    (th-should-see "while i < len")))

(ert-deftest rust-comparison-in-assert ()
  :expected-result :failed
  (th-fixtures #'rust-mode
    (helper-setup-rust-function)
    (th-type "assert!(x<10)")
    (th-should-see "assert!(x < 10)")))

(ert-deftest rust-comparison-after-return ()
  :expected-result :failed
  (th-fixtures #'rust-mode
    (helper-setup-rust-function)
    (th-type "return x>0")
    (th-should-see "return x > 0")))

(ert-deftest rust-comparison-chained ()
  :expected-result :failed
  (th-fixtures #'rust-mode
    (helper-setup-rust-function)
    (th-type "let b=x>0&&y<10")
    (th-should-see "let b = x > 0 && y < 10")))

(ert-deftest rust-less-than-or-equal ()
  (th-fixtures #'rust-mode
    (helper-setup-rust-function)
    (th-type "let b=x<=y")
    (th-should-see "let b = x <= y")))

(ert-deftest rust-greater-than-or-equal ()
  (th-fixtures #'rust-mode
    (helper-setup-rust-function)
    (th-type "let b=x>=y")
    (th-should-see "let b = x >= y")))


;;; Mixed: generics and comparisons in the same function

(ert-deftest rust-generic-then-comparison ()
  :expected-result :failed
  (th-fixtures #'rust-mode
    (th-type "fn foo(v:Vec<i32>)->bool {\n")
    (th-type "v.len()>0")
    (th-should-see "fn foo(v: Vec<i32>) -> bool {")
    (th-should-see "v.len() > 0")))

(ert-deftest rust-comparison-with-method-on-generic ()
  :expected-result :failed
  (th-fixtures #'rust-mode
    (helper-setup-rust-function)
    (th-type "let v:Vec<i32>=vec![1, 2, 3];\n")
    (th-type "if v.len()>0")
    (th-should-see "let v: Vec<i32> = vec![1, 2, 3];")
    (th-should-see "if v.len() > 0")))

(ert-deftest rust-turbofish-then-comparison ()
  :expected-result :failed
  (th-fixtures #'rust-mode
    (helper-setup-rust-function)
    (th-type "let x=iter.collect::<Vec<i32>>();\n")
    (th-type "if x.len()>0")
    (th-should-see "let x = iter.collect::<Vec<i32>>();")
    (th-should-see "if x.len() > 0")))


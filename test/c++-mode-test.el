;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)

(ert-deftest c++-assignment-at-beginning-of-line ()
  (th-fixtures #'c++-mode
    (th-type "=")
    (th-should-see-pattern "^=$")))

(ert-deftest c++-assignment-at-beginning-of-line-with-indentation ()
  (th-fixtures #'c++-mode
    (th-type "  =")
    (th-should-see-pattern "^  =$")))

(ert-deftest c++-automatic-type-references ()
  (th-fixtures #'c++-mode
    (th-type "auto&thing")
    (th-should-see "auto &thing")))

;; Move constructor type
(ert-deftest c++-and-operator-still-works ()
  (th-fixtures #'c++-mode
    (insert "int main() {\n")
    (th-type "x=a&&b")
    (th-should-see "a && b")))

(ert-deftest c++-move-constructor-works ()
  (th-fixtures #'c++-mode
    (th-type "A(A&&a)")
    (th-should-see "A(A &&a)")))

(ert-deftest c++-move-constructor-inside-class ()
  :expected-result :failed
  (th-fixtures #'c++-mode
    (insert "struct A {\n\n}")
    (goto-line 2)
    (th-type "A(A&&a)")
    (th-should-see "A(A &&a)")))

;; Rvalue reference declarations and C pointer type style
(ert-deftest c++-const-rvalue-references ()
  (th-fixtures #'c++-mode
    (th-type "int const&&x")
    (th-should-see "int const &&x")))

(ert-deftest c++-operator-&&-on-type ()
  (th-fixtures #'c++-mode
    (let ((electric-operator-c-pointer-type-style 'type))
      (th-type "int&&x")
      (th-should-see "int&& x"))))

(ert-deftest c++-operator-&&-on-variable ()
  (th-fixtures #'c++-mode
    (let ((electric-operator-c-pointer-type-style 'variable))
      (th-type "int&&x")
      (th-should-see "int &&x"))))

(ert-deftest c++-greater-than-still-works ()
  :expected-result :failed
  (th-fixtures #'c++-mode
    (insert "int main() {\n")
    (th-type "bool x=0>1")
    (th-should-see "bool x = 0 > 1")))

(ert-deftest c++-less-than-still-works ()
  :expected-result :failed
  (th-fixtures #'c++-mode
    (insert "int main() {\n")
    (th-type "bool x=0<1")
    (th-should-see "bool x = 0 < 1")))

(ert-deftest c++->>-still-works ()
  :expected-result :failed
  (th-fixtures #'c++-mode
    (insert "int main() {\n")
    (th-type "std::cin>>x;")
    (th-should-see "std::cin>>x;")))

(ert-deftest c++-template-in-function-rvalue ()
  (th-fixtures #'c++-mode
    (th-type "MyType<double> f()")
    (th-should-see "MyType<double> f()")))

(ert-deftest c++-template-in-function-argument ()
  (th-fixtures #'c++-mode
    (th-type "void f(MyType<double> x)")
    (th-should-see "void f(MyType<double> x)")))

(ert-deftest c++-nested-template-in-function-argument ()
  (th-fixtures #'c++-mode
    (th-type "void f(MyType<d<int>> x)")
    (th-should-see "void f(MyType<d<int>> x)")))

(ert-deftest c++-template-type-definition ()
  (th-fixtures #'c++-mode
    (insert "int main() {\n")
    (th-type "MyType<double> x")
    (th-should-see "MyType<double> x")))

(ert-deftest c++-nested-template-type-definition ()
  :expected-result :failed
  (th-fixtures #'c++-mode
    (insert "int main() {\n")
    (th-type "MyType<Template<double>> x")
    (th-should-see "MyType<Template<double>> x")))

;; Colon operator
(ert-deftest c++-for-each-loops ()
  (th-fixtures #'c++-mode
    (th-type "for(int x:list)")
    (th-should-see "for(int x : list)")))

(ert-deftest c++-standard-ternary-operator-still-works ()
  (th-fixtures #'c++-mode
    (th-type "a?b:c")
    (th-should-see "a ? b : c")))

(ert-deftest c++-ternary-operator-in-parens ()
  (th-fixtures #'c++-mode
    (th-type "(a?b:c)")
    (th-should-see "(a ? b : c)")))

(ert-deftest c++-namespaces-in-parens ()
  (th-fixtures #'c++-mode
    (th-type "f(a::foo::bar(x))")
    (th-should-see "f(a::foo::bar(x))")))

(ert-deftest c++-inheritance ()
  (th-fixtures #'c++-mode
    (th-type "class Foo:public Bar")
    (th-should-see "class Foo : public Bar")))

(ert-deftest c++-public-methods ()
  (th-fixtures #'c++-mode
    (th-type "public:")
    (th-should-see "public:")))

(ert-deftest c++-private-methods ()
  (th-fixtures #'c++-mode
    (th-type "private:")
    (th-should-see "private:")))

(ert-deftest c++-fully-qualified-public-inheritance ()
  (th-fixtures #'c++-mode
    (th-type "struct A:public::testing::Test")
    (th-should-see "struct A : public ::testing::Test")))

(ert-deftest c++-fully-qualified-protected-inheritance ()
  (th-fixtures #'c++-mode
    (th-type "struct A:protected::testing::Test")
    (th-should-see "struct A : protected ::testing::Test")))

(ert-deftest c++-fully-qualified-private-inheritance ()
  (th-fixtures #'c++-mode
    (th-type "struct A:private::testing::Test")
    (th-should-see "struct A : private ::testing::Test")))

(ert-deftest c++-fully-qualified-inheritance-without-access-specifier ()
  (th-fixtures #'c++-mode
    (th-type "struct A:::testing::Test")
    (th-should-see "struct A : ::testing::Test")))

;; Operator overloads should never be spaced
(ert-deftest c++-operator<< ()
  (th-fixtures #'c++-mode
    (th-type "operator<<")
    (th-should-see "operator<<")))

(ert-deftest c++-operator+ ()
  (th-fixtures #'c++-mode
    (th-type "operator+")
    (th-should-see "operator+")))

(ert-deftest c++-operator= ()
  (th-fixtures #'c++-mode
    (th-type "operator=")
    (th-should-see "operator=")))

;; Lambdas
(ert-deftest c++-ref-inside-operator-brackets-still-unspaced ()
  (th-fixtures #'c++-mode
    (insert "int main() {\n")
    (th-type "mymap[&a_pt]")
    (th-should-see "mymap[&a_pt]")))

(ert-deftest c++-reference-captures-in-lambda-functions-unspaced ()
  (th-fixtures #'c++-mode
    (insert "int main() {\n")
    (th-type "[&ticket, &i]")
    (th-should-see "[&ticket, &i]")))

(ert-deftest c++-value-captures-in-lambda-functions-unspaced ()
  (th-fixtures #'c++-mode
    (insert "int main() {\n")
    (th-type "[=ticket, =i]")
    (th-should-see "[=ticket, =i]")))

(ert-deftest c++-space->-in-simple-lambda-return-type ()
  (th-fixtures #'c++-mode
    (insert "int main() {\n")
    (th-type "[] ()->bool {")
    (th-should-see "[] () -> bool {")))

(ert-deftest c++-space->-in-mutable-lambda-return-type ()
  (th-fixtures #'c++-mode
    (insert "int main() {\n")
    (th-type "[] () mutable->bool {")
    (th-should-see "[] () mutable -> bool {")))

(ert-deftest c++-default-capture-by-value ()
  (th-fixtures #'c++-mode
    (insert "int main() {\n")
    (th-type "const auto x = [=]")
    (th-should-see "const auto x = [=]")))

(ert-deftest c++-default-capture-by-reference ()
  (th-fixtures #'c++-mode
    (insert "int main() {\n")
    (th-type "const auto x = [&]")
    (th-should-see "const auto x = [&]")))

(ert-deftest c++-default-capture-by-value-with-autopair ()
  (th-fixtures #'c++-mode
    (insert "int main() {\n")
    (insert "const auto x = []")
    (goto-char (- (point) 1))
    (th-type "=")
    (th-should-see "const auto x = [=]")))

(ert-deftest c++-default-capture-by-reference-with-autopair ()
  (th-fixtures #'c++-mode
    (insert "int main() {\n")
    (insert "const auto x = []")
    (goto-char (- (point) 1))
    (th-type "&")
    (th-should-see "const auto x = [&]")))

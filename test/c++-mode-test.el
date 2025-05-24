;;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)

(ert-deftest c++-assignment-at-beginning-of-line ()
  (test-with-mode c++-mode
    (electric-operator-test-type "=")
    (electric-operator-test-should-see-pattern "^=$")))

(ert-deftest c++-assignment-at-beginning-of-line-with-indentation ()
  (test-with-mode c++-mode
    (electric-operator-test-type "  =")
    (electric-operator-test-should-see-pattern "^  =$")))

(ert-deftest c++-automatic-type-references ()
  (test-with-mode c++-mode
    (electric-operator-test-type "auto&thing")
    (electric-operator-test-should-see "auto &thing")))

;; Move constructor type
(ert-deftest c++-and-operator-still-works ()
  (test-with-mode c++-mode
    (insert "int main() {\n")
    (electric-operator-test-type "x=a&&b")
    (electric-operator-test-should-see "a && b")))

(ert-deftest c++-move-constructor-works ()
  (test-with-mode c++-mode
    (electric-operator-test-type "A(A&&a)")
    (electric-operator-test-should-see "A(A &&a)")))

(ert-deftest c++-move-constructor-inside-class ()
  :expected-result :failed
  (test-with-mode c++-mode
    (insert "struct A {\n\n}")
    (goto-line 2)
    (electric-operator-test-type "A(A&&a)")
    (electric-operator-test-should-see "A(A &&a)")))

;; Rvalue reference declarations and C pointer type style
(ert-deftest c++-const-rvalue-references ()
  (test-with-mode c++-mode
    (electric-operator-test-type "int const&&x")
    (electric-operator-test-should-see "int const &&x")))

(ert-deftest c++-operator-&&-on-type ()
  (test-with-mode c++-mode
    (let ((electric-operator-c-pointer-type-style 'type))
      (electric-operator-test-type "int&&x")
      (electric-operator-test-should-see "int&& x"))))

(ert-deftest c++-operator-&&-on-variable ()
  (test-with-mode c++-mode
    (let ((electric-operator-c-pointer-type-style 'variable))
      (electric-operator-test-type "int&&x")
      (electric-operator-test-should-see "int &&x"))))

(ert-deftest c++-greater-than-still-works ()
  :expected-result :failed
  (test-with-mode c++-mode
    (insert "int main() {\n")
    (electric-operator-test-type "bool x=0>1")
    (electric-operator-test-should-see "bool x = 0 > 1")))

(ert-deftest c++-less-than-still-works ()
  :expected-result :failed
  (test-with-mode c++-mode
    (insert "int main() {\n")
    (electric-operator-test-type "bool x=0<1")
    (electric-operator-test-should-see "bool x = 0 < 1")))

(ert-deftest c++->>-still-works ()
  :expected-result :failed
  (test-with-mode c++-mode
    (insert "int main() {\n")
    (electric-operator-test-type "std::cin>>x;")
    (electric-operator-test-should-see "std::cin>>x;")))

(ert-deftest c++-template-in-function-rvalue ()
  (test-with-mode c++-mode
    (electric-operator-test-type "MyType<double> f()")
    (electric-operator-test-should-see "MyType<double> f()")))

(ert-deftest c++-template-in-function-argument ()
  (test-with-mode c++-mode
    (electric-operator-test-type "void f(MyType<double> x)")
    (electric-operator-test-should-see "void f(MyType<double> x)")))

(ert-deftest c++-nested-template-in-function-argument ()
  (test-with-mode c++-mode
    (electric-operator-test-type "void f(MyType<d<int>> x)")
    (electric-operator-test-should-see "void f(MyType<d<int>> x)")))

(ert-deftest c++-template-type-definition ()
  (test-with-mode c++-mode
    (insert "int main() {\n")
    (electric-operator-test-type "MyType<double> x")
    (electric-operator-test-should-see "MyType<double> x")))

(ert-deftest c++-nested-template-type-definition ()
  :expected-result :failed
  (test-with-mode c++-mode
    (insert "int main() {\n")
    (electric-operator-test-type "MyType<Template<double>> x")
    (electric-operator-test-should-see "MyType<Template<double>> x")))

;; Colon operator
(ert-deftest c++-for-each-loops ()
  (test-with-mode c++-mode
    (electric-operator-test-type "for(int x:list)")
    (electric-operator-test-should-see "for(int x : list)")))

(ert-deftest c++-standard-ternary-operator-still-works ()
  (test-with-mode c++-mode
    (electric-operator-test-type "a?b:c")
    (electric-operator-test-should-see "a ? b : c")))

(ert-deftest c++-ternary-operator-in-parens ()
  (test-with-mode c++-mode
    (electric-operator-test-type "(a?b:c)")
    (electric-operator-test-should-see "(a ? b : c)")))

(ert-deftest c++-namespaces-in-parens ()
  (test-with-mode c++-mode
    (electric-operator-test-type "f(a::foo::bar(x))")
    (electric-operator-test-should-see "f(a::foo::bar(x))")))

(ert-deftest c++-inheritance ()
  (test-with-mode c++-mode
    (electric-operator-test-type "class Foo:public Bar")
    (electric-operator-test-should-see "class Foo : public Bar")))

(ert-deftest c++-public-methods ()
  (test-with-mode c++-mode
    (electric-operator-test-type "public:")
    (electric-operator-test-should-see "public:")))

(ert-deftest c++-private-methods ()
  (test-with-mode c++-mode
    (electric-operator-test-type "private:")
    (electric-operator-test-should-see "private:")))

(ert-deftest c++-fully-qualified-public-inheritance ()
  (test-with-mode c++-mode
    (electric-operator-test-type "struct A:public::testing::Test")
    (electric-operator-test-should-see "struct A : public ::testing::Test")))

(ert-deftest c++-fully-qualified-protected-inheritance ()
  (test-with-mode c++-mode
    (electric-operator-test-type "struct A:protected::testing::Test")
    (electric-operator-test-should-see "struct A : protected ::testing::Test")))

(ert-deftest c++-fully-qualified-private-inheritance ()
  (test-with-mode c++-mode
    (electric-operator-test-type "struct A:private::testing::Test")
    (electric-operator-test-should-see "struct A : private ::testing::Test")))

(ert-deftest c++-fully-qualified-inheritance-without-access-specifier ()
  (test-with-mode c++-mode
    (electric-operator-test-type "struct A:::testing::Test")
    (electric-operator-test-should-see "struct A : ::testing::Test")))

;; Operator overloads should never be spaced
(ert-deftest c++-operator<< ()
  (test-with-mode c++-mode
    (electric-operator-test-type "operator<<")
    (electric-operator-test-should-see "operator<<")))

(ert-deftest c++-operator+ ()
  (test-with-mode c++-mode
    (electric-operator-test-type "operator+")
    (electric-operator-test-should-see "operator+")))

(ert-deftest c++-operator= ()
  (test-with-mode c++-mode
    (electric-operator-test-type "operator=")
    (electric-operator-test-should-see "operator=")))

;; Lambdas
(ert-deftest c++-ref-inside-operator-brackets-still-unspaced ()
  (test-with-mode c++-mode
    (insert "int main() {\n")
    (electric-operator-test-type "mymap[&a_pt]")
    (electric-operator-test-should-see "mymap[&a_pt]")))

(ert-deftest c++-reference-captures-in-lambda-functions-unspaced ()
  (test-with-mode c++-mode
    (insert "int main() {\n")
    (electric-operator-test-type "[&ticket, &i]")
    (electric-operator-test-should-see "[&ticket, &i]")))

(ert-deftest c++-value-captures-in-lambda-functions-unspaced ()
  (test-with-mode c++-mode
    (insert "int main() {\n")
    (electric-operator-test-type "[=ticket, =i]")
    (electric-operator-test-should-see "[=ticket, =i]")))

(ert-deftest c++-space->-in-simple-lambda-return-type ()
  (test-with-mode c++-mode
    (insert "int main() {\n")
    (electric-operator-test-type "[] ()->bool {")
    (electric-operator-test-should-see "[] () -> bool {")))

(ert-deftest c++-space->-in-mutable-lambda-return-type ()
  (test-with-mode c++-mode
    (insert "int main() {\n")
    (electric-operator-test-type "[] () mutable->bool {")
    (electric-operator-test-should-see "[] () mutable -> bool {")))

(ert-deftest c++-default-capture-by-value ()
  (test-with-mode c++-mode
    (insert "int main() {\n")
    (electric-operator-test-type "const auto x = [=]")
    (electric-operator-test-should-see "const auto x = [=]")))

(ert-deftest c++-default-capture-by-reference ()
  (test-with-mode c++-mode
    (insert "int main() {\n")
    (electric-operator-test-type "const auto x = [&]")
    (electric-operator-test-should-see "const auto x = [&]")))

(ert-deftest c++-default-capture-by-value-with-autopair ()
  (test-with-mode c++-mode
    (insert "int main() {\n")
    (insert "const auto x = []")
    (goto-char (- (point) 1))
    (electric-operator-test-type "=")
    (electric-operator-test-should-see "const auto x = [=]")))

(ert-deftest c++-default-capture-by-reference-with-autopair ()
  (test-with-mode c++-mode
    (insert "int main() {\n")
    (insert "const auto x = []")
    (goto-char (- (point) 1))
    (electric-operator-test-type "&")
    (electric-operator-test-should-see "const auto x = [&]")))

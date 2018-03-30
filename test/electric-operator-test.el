(require 'electric-operator)

(ert-deftest trie-create ()
  (let ((trie (make-electric-operator--trie)))
    (should (equal (electric-operator--trie-value trie) nil))))

(ert-deftest trie-basic ()
  (let ((trie (make-electric-operator--trie)))

    (electric-operator--trie-put '(*) trie " * ")
    (electric-operator--trie-put '(a b) trie "foo")
    (electric-operator--trie-put '(a x) trie "xyz")
    (electric-operator--trie-put '(a) trie "bar")

    (should (equal (electric-operator--trie-get '(*) trie) " * "))
    (should (equal (electric-operator--trie-get '(a b) trie) "foo"))
    (should (equal (electric-operator--trie-get '(a) trie) "bar"))
    (should (equal (electric-operator--trie-get '(a b c) trie) nil))))

(ert-deftest string-to-trie-key ()
  (should (equal (electric-operator--string-to-trie-key "abc")
                 '(?c ?b ?a)))
  (should (equal (electric-operator--string-to-trie-key " a b   c")
                 '(?c ?b ?a)))
  (should (equal (electric-operator--string-to-trie-key "tab:	x")
                 '(?x ?: ?b ?a ?t))))

(ert-deftest trie-operators ()
  (let ((trie (make-electric-operator--trie)))
    (electric-operator--trie-put-operator "*=" " *= " trie)
    (electric-operator--trie-put-operator "/=" " /= " trie)
    (should (equal (electric-operator--trie-get-operator "*=" trie) " *= "))
    (should (equal (electric-operator--trie-get-operator "++" trie) nil))))


;; Local Variables:
;; nameless-current-name: "electric-operator"
;; End:

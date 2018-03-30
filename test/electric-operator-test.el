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
    (should (equal (electric-operator--trie-get-operator "++" trie) nil))
    (should (equal (electric-operator--trie-get-operator "a*=" trie) " *= "))

    ;; This should only recurse down the trie a few times despite the long
    ;; input, to check this use `(trace-function #'electric-operator--trie-find)'.
    (should (equal (electric-operator--trie-get-operator
                    "Nam a sapien.
Lorem ipsum dolor sit amet, consectetuer adipiscing elit.
Mauris mollis tincidunt felis.
Lorem ipsum dolor sit amet, consectetuer adipiscing elit.
Nulla posuere. Aliquam posuere. Phasellus purus. Aliquam posuere.
Fusce suscipit, wisi nec facilisis facilisis, est dui fermentum leo, quis tempor ligula erat quis odio.
Proin neque massa, cursus ut, gravida ut, lobortis eget, lacus.
Aliquam erat volutpat.
Aenean in sem ac leo mollis blandit. xyz /=" trie) " /= "))

    ))


(ert-deftest trie-get-all ()
  (let ((trie (make-electric-operator--trie)))

    (electric-operator--trie-put '(*) trie " * ")
    (electric-operator--trie-put '(a b) trie '("1" "2" "3"))
    (electric-operator--trie-put '(a b c) trie "6")
    (electric-operator--trie-put '(a x) trie "xyz")
    (electric-operator--trie-put '(a) trie "bar")

    ;; We don't actually care about the order
    (should (equal (electric-operator--trie-get-all trie)
                   (nreverse '(" * " ("1" "2" "3") "6" "xyz" "bar"))))))


;; Local Variables:
;; nameless-current-name: "electric-operator"
;; End:

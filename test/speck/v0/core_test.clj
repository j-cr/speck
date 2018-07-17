(ns speck.v0.core-test
  (:require [speck.v0.core :as speck :refer [| => gen-spec gen-specs-in-ns]]
            [clojure.test :as test :refer [deftest testing is are]]
            [clojure.spec.alpha :as s]
            ;; [clojure.spec.test.alpha :as s.test]
            [orchestra.spec.test :as orchestra]
            [expound.alpha :as expound]))

;; (set! spec/*explain-out* expound/printer)
;; (set! spec/*explain-out* spec/explain-printer)

(defmacro throws? [form]
  `(is (~'thrown? clojure.lang.ExceptionInfo ~form)))

;; --------------------------------------------------------------------------------

(deftest syntax-sugar-test
  (are [e1 e2] (= (macroexpand e1) (macroexpand e2))
    `(| ~'_ ~'=> number?)
    `(=> [] number?)

    `(| number? ~'=> number?)
    `(=> [number?] number?)

    `(| number? ~'=> number?, number? string? ~'=> string?)
    `(=> [number?] number?, [number? string?] string?)
    ))


(defn single-clause
  {:speck (=> [number?] odd?)}
  [x] (inc x))
(gen-spec single-clause)
(orchestra/instrument `[single-clause])

(defn multi-clause
  {:speck (=> [] odd?
              [number?] odd?
              [number? string?] (s/and string? #(= (count %) 2)))}
  ([] 0)
  ([x] (inc x))
  ([x s] (str (inc x) s)))
(gen-spec multi-clause)
(orchestra/instrument `[multi-clause])

(defn optional-arg
  {:speck (| (s/? number?) string? => string?)}
  ([s] s)
  ([x s] (str x s)))
(gen-spec optional-arg)
(orchestra/instrument `[optional-arg])


(deftest basic-tests

  (testing "single-clause"
    (is (= (single-clause 0) 1))
    (throws? (single-clause "0"))
    (throws? (single-clause 1)))

  (testing "multi-clause"
    (is (= (multi-clause 0) 1))
    (is (= (multi-clause 0 "x") "1x"))
    (throws? (multi-clause "0")) ;; pre
    (throws? (multi-clause "0" "x"))
    (throws? (multi-clause 0 1))
    (throws? (multi-clause)) ;; post
    (throws? (multi-clause 1))
    (throws? (multi-clause 0 "xx")))

  (testing "optional-arg"
    (is (= (optional-arg "x") "x"))
    (is (= (optional-arg 1 "x") "1x"))
    (throws? (optional-arg :x))
    (throws? (optional-arg "1" "x"))
    (throws? (optional-arg 1 2 "x")))
  )



(defn varargs
  {:speck (=> [string? (s/* number?)] coll?)}
  [x & xs] (cons x xs))
(gen-spec varargs)
(orchestra/instrument `[varargs])

(defn multi-varargs
  {:speck (| int? => odd?
             int? int? => string?
             int? int? (s/* int?) => string?)}
  ([x] x)
  ([x y] (str x y))
  ([x y & more] (apply str x y more)))
(gen-spec multi-varargs)
(orchestra/instrument `[multi-varargs])

(s/def ::a #{1})
(s/def ::b #{2})
(defn kw-args
  {:speck (| int? (s/keys* :opt-un [::a ::b]) => string?)}
  [x & {:keys [a b]}] (str x a b))
(gen-spec kw-args)
(orchestra/instrument `[kw-args])


(deftest varargs-tests

  (testing "varargs"
    (is (= (varargs "x") ["x"]))
    (is (= (varargs "x" 1 2) ["x" 1 2]))
    (throws? (varargs 0))
    (throws? (varargs 0 1 2))
    (throws? (varargs "x" "y" "z")))

  (testing "multi-varargs"
    (is (= (multi-varargs 1) 1))
    (is (= (multi-varargs 1 2) "12"))
    (is (= (multi-varargs 1 2 3) "123"))
    (throws? (multi-varargs "1"))
    (throws? (multi-varargs 0))
    (throws? (multi-varargs 1 "2"))
    (throws? (multi-varargs 1 2 3 "4")))

  (testing "kw-args"
    (is (= (kw-args 0) "0"))
    (is (= (kw-args 0 :a 1) "01"))
    (is (= (kw-args 0 :a 1 :b 2) "012"))
    (throws? (kw-args "0"))
    (throws? (kw-args 0 :a "1"))
    (throws? (kw-args 0 :a 1 :b "2")))
  )


(defn error-args
  {:speck (| int? => int?, int? int? => int?)}
  [x] (inc x))

(deftest wrong-specks-test
  (is (thrown? AssertionError (macroexpand `(gen-spec error-args)))))



;; --------------------------------------------------------------------------------
;; (test/successful? (test/run-tests))

(ns speck.v1.core-test
  (:require [speck.v1.core :as speck :refer [|]]
            [clojure.test :as test :refer [deftest testing is are]]
            [clojure.spec.alpha :as s]
            ;; [clojure.spec.test.alpha :as s.test]
            ;; [orchestra.spec.test :as orchestra]
            ))


;; setup --------------------------------------------------------------------------

(set! *data-readers* (assoc *data-readers* '| #'speck/speck-reader))

(test/use-fixtures
  :once (fn [body]
          (binding [speck/*auto-define-opts* {:enabled false}]
            (body))))


;; tests --------------------------------------------------------------------------

(deftest syntax-spec-tests
  (are [in out]
    (= (s/conform ::speck/syntax in) out)


    '[_ => ret]
    '{:clauses [{:argslist [:unnamed []], :=> =>, :ret ret}]}


    '[a? b? => ret]
    '{:clauses [{:argslist [:unnamed [a? b?]], :=> =>, :ret ret}]}


    '[x :- a?, y :- b? => ret]
    '{:clauses [{:argslist [:named ([x a?] [y b?])], :=> =>, :ret ret}]}


    '[a => b, c => d, :ret r]
    '{:clauses [{:argslist [:unnamed [a]], :=> =>, :ret b}
                {:argslist [:unnamed [c]], :=> =>, :ret d}],
      :opts {:ret r}}


    '[a => b  |- args-expr |= fn-expr]
    '{:clauses [{:argslist [:unnamed [a]], :=> =>, :ret b
                 :args-expr args-expr
                 :fn-expr fn-expr}]}


    '[a => b |- args1,  c => d |- args2,  :gen g]
    '{:clauses [{:argslist [:unnamed [a]], :=> =>, :ret b, :args-expr args1}
                {:argslist [:unnamed [c]], :=> =>, :ret d, :args-expr args2}]
      :opts {:gen g}}

    ))



(deftest main-tests


  (testing "unnamed args"
    (let [fspec (| [odd? even? => pos?])]

      (are [expr res] (= res (s/valid? (:args fspec) expr))
        [1 2] true
        [2 1] false
        []    false )

      (are [expr res] (= res (s/valid? (:ret fspec) expr))
        1 true
        0 false ) ))


  (testing "named args"
    (let [spec (-> #|[x :- odd?, y :- even? => any?] :speck :args)]
      (is (= (s/conform spec [1 2]) {:x 1 :y 2}))
      ))


  (testing "zero arg spec"
    (let [spec (-> #|[_ => any?] :speck :args)]
      (is (s/valid? spec []))
      (is (not (s/valid? spec [:foo])))
      ))


  (testing "optional args and varargs"
    (let [s1 (-> #|[(s/? odd?) even? => any?] :speck :args)
          s2 (-> #|[odd? (s/* even?) => any?] :speck :args)]

      (are [expr res] (= res (s/valid? s1 expr))
        [1 2] true
        [  2] true
        [2 1] false)

      (are [expr res] (= res (s/valid? s2 expr))
        [1 2]   true
        [1  ]   true
        [1 2 2] true
        [2 1]   false ) ))


  (testing "fspec opts: args, ret and fn"
    (let [fspec (| [x :- odd?, y :- even?  =>  pos?
                    :args #(< (:x %) (:y %))
                    :ret even?
                    :fn #(= (-> % :args :x) (-> % :ret))])]

      (are [expr res] (= res (s/valid? (:args fspec) expr))
        [1 2] true
        [3 2] false
        [2 1] false )

      (are [expr res] (= res (s/valid? (:ret fspec) expr))
        2 true
        1 false
        0 false )

      (are [expr res] (= res (s/valid? (:fn fspec) expr))
        {:args {:x 42} :ret 42}  true
        {:args {:x 41} :ret 42}  false ) ))


  (testing "zero-clause speck (opts only)"
    (let [fspec (| [:args (s/tuple odd? even?), :ret even?])]

      (are [expr res] (= res (s/valid? (:args fspec) expr))
        [1 2] true
        [1  ] false
        [2 1] false )

      (are [expr res] (= res (s/valid? (:ret fspec) expr))
        2 true
        1 false ) ))


  (testing "multi-clause"
    (let [fspec (| [pos? => pos?, odd? even? => neg?])]

      (are [expr res] (= res (s/valid? (:args fspec) expr))
        [1] true
        [1 2] true
        [2 1] false
        [1 2 3] false
        )

      (are [expr res] (= res (s/conform (:args fspec) expr))
        [1]   [:arity-1 {:%1 1}]
        [1 2] [:arity-2 {:%1 1, :%2 2}]
        )

      (are [expr res] (= res (s/valid? (:fn fspec) expr))
        {:args (first {:arity-1 {:%1 42}})      :ret 42} true
        {:args (first {:arity-1 {:%1 42}})      :ret -1} false
        {:args (first {:arity-2 {:%1 1 :%2 2}}) :ret -1} true
        {:args (first {:arity-2 {:%1 1 :%2 2}}) :ret 42} false
        ) ))


  (testing "multi-clause with fspec opts"
    (let [fspec (| [pos? => pos?, odd? even? => neg?
                    :args #(->> % val vals (every? (fn [x] (> x 10))))
                    :ret  even?
                    :fn   #(->> % :ret odd?)])]

      (are [expr res] (= res (s/valid? (:args fspec) expr))
        [1] false
        [11] true
        [1 2] false
        [11 12] true )

      (are [expr res] (= res (s/valid? (:fn fspec) expr))
        {:args (first {:arity-1 {:%1 42}})      :ret 42} false
        {:args (first {:arity-1 {:%1 41}})      :ret 41} true
        {:args (first {:arity-2 {:%1 1 :%2 2}}) :ret -2} false
        {:args (first {:arity-2 {:%1 1 :%2 2}}) :ret -1} true
        ) ))


  (testing "|- and |= (single clause)"
    (let [fspec (| [x :- odd?, y :- even?  =>  pos?
                    |-  (< x y)
                    |=  (= x %) ])]

      (are [expr res] (= res (s/valid? (:args fspec) expr))
        [1 2] true
        [3 2] false
        [2 4] false )

      (are [expr res] (= res (s/valid? (:fn fspec) expr))
        {:args {:x 41} :ret 41}  true
        {:args {:x 41} :ret 42}  false
        ) ))


  (testing "|- and |= (multi clause)"
    (let [fspec (| [pos?       => pos?  |- (< %1 10)  |= (= %1 %)
                    odd? even? => neg?  |- (< %1 %2)  |= (< %1 %2 %)
                    ])]

      (are [expr res] (= res (s/valid? (:args fspec) expr))
        [1] true
        [11] false
        [-1] false
        [1 2] true
        [2 1] false
        [3 2] false )

      (are [expr res] (= res (s/valid? (:fn fspec) expr))
        ;; pos? and (= %1 %)
        {:args (first {:arity-1 {:%1 42}}) :ret 42} true
        {:args (first {:arity-1 {:%1 -1}}) :ret -1} false
        {:args (first {:arity-1 {:%1 41}}) :ret 42} false

        ;; neg? and (< %1 %2 %)
        {:args (first {:arity-2 {:%1 -3 :%2 -2}}) :ret -1} true
        {:args (first {:arity-2 {:%1 -3 :%2 -2}}) :ret  1} false
        {:args (first {:arity-2 {:%1 -1 :%2 -2}}) :ret -3} false
        ) ))


  (testing "|- and |= (single, with opts)"
    (let [fspec (| [x :- odd? => any?
                    |-     (>   x    10)  |=   (>    %     10)
                    :args #(< (:x %) 20)  :fn #(< (:ret %) 20)
                    ])]

      (are [expr res] (= res (s/valid? (:args fspec) expr))
        [1] false
        [11] true
        [12] false
        [21] false )

      (are [expr res] (= res (s/valid? (:fn fspec) expr))
        {:ret 1} false
        {:ret 11} true
        {:ret 21} false )
      ))


  (testing "|- and |= (multi, with opts)"
    ;; same test cases as in previous, except now args are with s/or tag
    (let [fspec (| [_ => any?, x :- odd? => odd?
                    |- (> x 10)
                    |= (> % 10)
                    :args #(< (-> % val :x) 20)  ; [:arity-1 {:x ..}]
                    :fn #(< (:ret %) 20)         ; {:args [:arity-1 ...], :ret ..}
                    ])]

      (are [expr res] (= res (s/valid? (:args fspec) expr))
        [1] false
        [11] true
        [12] false
        [21] false )

      (are [expr res] (= res (->> {:args (first {:arity-1 :any}) :ret expr}
                                  (s/valid? (:fn fspec) ,,,)))
        1 false
        11 true
        12 false
        21 false )
      ))
  )



(comment

  (|[x1 => r1     |- args1  |= fn1
     x2 y2 => r2  |- args2  |= fn2
     :args opts-args
     :ret  opts-ret
     :fn   opts-fn])

  ;; expands to:

  (s/fspec
   :args (s/and
          (s/or :arity-1 (s/and (s/cat :%1 x1       ) (fn [{:keys [%1   ]}] args1))
                :arity-2 (s/and (s/cat :%1 x2 :%2 y2) (fn [{:keys [%1 %2]}] args2)))
          opts-args)
   :ret (s/and any? opts-ret)
   :fn (s/and
        (fn [%]
          (case (-> % :args key)
            :arity-1 (and
                      (s/valid? r1 (-> % :ret))
                      (let [{:keys [%1 %2]} (-> % :args val)
                            % (-> % :ret)]
                        fn1))
            :arity-2 (and
                      (s/valid? r2 (-> % :ret))
                      (let [{:keys [%1 %2]} (-> % :args val) (-> % :args val)
                            % (-> % :ret)]
                        fn2))))
        opts-fn))
  )



;; --------------------------------------------------------------------------------
;; (if (test/successful? (test/run-tests)) :ok :FAIL)

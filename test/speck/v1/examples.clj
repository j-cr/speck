(ns speck.v1.examples
  (:require [speck.v1.core :as speck]
            [clojure.test :as test :refer [deftest testing is are]]
            [clojure.spec.alpha :as s]
            [orchestra.spec.test :as orchestra]))


;; This namespace contains some examples of simple specked functions. Feel free
;; to fire up the REPL and follow along.

;; before you start, register the speck literal if you haven't done so via
;; data_readers.clj file:
(set! *data-readers* (assoc *data-readers* '| #'speck/speck-reader))
;; if you're not in emacs/cider, you might need to do this instead:
;; (alter-var-root #'*data-readers* assoc '| #'speck/speck-reader)

;; to enable :ret and :fn spec checking, tell speck to use orchestra
;; instrumentation instead of the default one:
(alter-var-root #'speck/*auto-define-opts* assoc
  :instrument-fn orchestra/instrument)


(defmacro throws?
  "Checks if the form throws an exception and the exception's message contains s"
  ([form]   `(is (~'thrown? clojure.lang.ExceptionInfo ~form)))
  ([form s] `(is (~'thrown-with-msg? clojure.lang.ExceptionInfo (re-pattern ~s) ~form))))


;; --------------------------------------------------------------------------------


(defn join
  #|[(s/? any?) (s/coll-of any?) => string?]
  ([coll]     (apply str coll))
  ([sep coll] (join (interpose sep coll))))

(deftest join:test
  (is (= (join [1 2 3]) "123"))
  (is (= (join "," [1 2 3]) "1,2,3"))
  (throws? (join 1 2 3)))


(defn pad-string
  #| [string? nat-int? (s/? char?) => string?]
  ([s width]     (pad-string s width \space))
  ([s width pad] (str (->> pad (repeat (- width (count s))) (apply str))
                      s)))

(deftest pad-string:test
  (is (=   (pad-string "!"  3   ) "  !"))
  (is (=   (pad-string "!"  3 \.) "..!"))
  (throws? (pad-string "!" -1   ) )
  (throws? (pad-string "!"  1 :-) ))


(defn rand-digits
  #|[nat-int? (s/? nat-int?) => string?]
  ([n]      (rand-digits n 2))
  ([n base] (apply str (repeatedly n #(rand-int base)))))

(deftest rand-digits:test
  (is (every? #{\0 \1} (rand-digits 10)))
  (throws? (rand-digits -1))
  (throws? (rand-digits 1 -1)))


(defn rect-area
  #|[side :- pos-int?  =>  pos-int?
     width :- pos-int?, height :- pos-int?  =>  pos-int?]
  ([w]   (rect-area w w))
  ([w h] (* w h)))

(deftest rect-area:test
  (is (= (rect-area 2) 4))
  (is (= (rect-area 2 3) 6))
  (throws? (rect-area 0))
  (throws? (rect-area 1 0)))


(defn ranged-rand
  #|[start :- int?, end :- int?  =>  int?
     |-  (< start end)
     |=  (and (>= % start) (< % end))]
  [start end]
  (- start (long (rand (- end start)))))

(deftest ranged-rand:test
  (throws? (ranged-rand 10 1)) ; pre - incorrect args
  (comment (ranged-rand 0 10)) ; post - incorrect implementation
  (is (= 0 (ranged-rand 0 1))) ; this one call happens to work correctly
  )


;; --------------------------------------------------------------------------------
(speck/define-specs-in-current-ns speck/*auto-define-opts*)
;; (test/successful? (test/run-tests))

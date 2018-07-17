(ns speck.v0.core
  (:require [clojure.spec.alpha :as s]))


(defn maybe-conform [spec x]
  (let [res (s/conform spec x)]
    (if (= res ::s/invalid)
      (throw (ex-info (s/explain-str spec x)
                      (s/explain-data spec x)))
      res)))


(s/def ::=>-args
  (s/+ (s/cat :args vector?
              :ret any?)))

(s/fdef => :args ::=>-args)
(defmacro =>
  "Create a spec definition for a function. The syntax is as follows:

      (=> [args*] ret)                       ; single-clause
      (=> [args1*] ret1, [args2*] ret2, ...) ; multi-clause

  - where args and rets represent specs. Each of the args will be
  matched with the argument it checks based on the order of the
  arguments in the fn's arglists. For multi-arity functions,
  multi-clause form can be used, and different ret specs can be
  specified for each arity. Alternatively, single-clause form with
  optional (via `s/?`) arguments can be used; then the number of
  argument specs must match the number of args in the largest arity of
  the function. For variable arity functions, the last argument
  spec (presumably defined with `s/*`) is matched with the list of
  varargs.

  This macro does not register the spec definition of which it
  creates; for that, you must add the definition as :speck metadata
  of the corresponding function, and then call `gen-spec` or
  `gen-specs-in-ns`."
  [& args+rets]
  `(quote ~args+rets))


(defn argname [expr]
  ;; kws created by `keyword` don't have to be readable, so we don't care if
  ;; expr is a symbol or e.g. a destructuring form
  ;; TODO: try to always generate readable names? Make sure they are unique?
  (keyword (str expr)))

(defn args-form [{:keys [args arglist]}]
  ;; in varargs specs, the last :args spec matches the coll of the variable args
  ;; so we simply remove the `&` to get the correct pairing
  `(s/cat ~@(interleave
             (->> arglist (remove #{'&}) (map argname))
             args)))

(defn single-clause-fdef [name {:keys [args ret] :as m}]
  `(s/fdef ~name
     :args ~(args-form m)
     :ret  ~ret))

(defn arity [arglist]
  (if (some #{'&} arglist)
    :arity-n
    (keyword (str "arity-" (count arglist)))))

(defn multi-clause-fdef [name clause-maps]
  `(s/fdef ~name
     :args (s/or ~@(mapcat (juxt :arity args-form) clause-maps))
     :ret any? ;; :ret has to be present in order for :fn to work
     :fn #(-> (case (-> % :args key)
                ~@(mapcat (juxt :arity :ret) clause-maps))
              (s/valid? (:ret %)))))

(defmacro gen-spec
  "Generate and define (via `clojure.spec.alpha/fdef`) a spec for the
  specked function named by sym. A function is specked when it
  contains :clojure.spec.alpha/def key in its metadata, with the value
  being the output of `=>` or `|`. Usually you should prefer
  `gen-specs-in-ns` to calling this directly."
  [sym]
  (let [meta (-> sym resolve meta)
        ;; :args + :ret
        clause-maps (some->> (:speck meta) (maybe-conform ::=>-args))]
    (assert (not (nil? clause-maps))
            (str (-> sym resolve) ": can't find the speck def in the var's metadata"))
    (if (= (count clause-maps) 1)
      ;; grab the last (i.e. the fullest) arglist:
      (single-clause-fdef sym (-> (first clause-maps)
                                  (assoc :arglist (last (:arglists meta)))))
      ;; otherwise, match specs with arglists:
      (do (assert (= (count clause-maps) (count (:arglists meta)))
                  (str (-> sym resolve) ": number of multi-arity speck clauses doesn't match the number of arglists"))
          (multi-clause-fdef sym (map #(assoc %1 :arglist %2 :arity (arity %2))
                                      clause-maps
                                      (:arglists meta)))))))


(defn filter-specked-vars [ns]
  (->> (vals (ns-interns ns))
       (filter #(some #{:speck} (-> % meta keys)))))

(defn var-full-name [v]
  (symbol (-> v meta :ns str)
          (-> v meta :name str)))

(defmacro gen-specs-in-ns
  "Generate and define specs (via `gen-spec`) for every specked
  function in the current ns. You should put a call to this macro at
  the end of your file, after all of the functions in the ns has been
  defined. Note that this does NOT instrument your functions - you
  should do it yourself with `clojure.spec.test.alpha/instrument` or
  `orchestra.spec.test/instrument`."
  []
  (->> (for [v (filter-specked-vars *ns*)]
         `(gen-spec ~(var-full-name v)))
       (cons `do)))


(s/def ::|-args
  (s/+ (s/cat :args  (s/+ #(not= % (symbol "=>")))
              :arrow         #(= % (symbol "=>"))
              :ret        #(not= % (symbol "=>")))))

(s/fdef | :args ::|-args)
(defmacro |
  "Syntax sugar for `=>`.

  (| args* => ret, ...) desugars to (=> [args] ret, ...). You can use
  _ in place of an empty argument vector. Note that the arrow =>
  inside the body of this macro is part of the syntax and is not the
  same as the macro `=>`."
  [& body]
  (->> (maybe-conform ::|-args body)
       (mapcat (fn [{:keys [args ret]}]
                 `[[~@(remove #{'_} args)] ~ret]))
       (cons `=>)))

(ns speck.v1.core
  (:require
   [clojure.spec.alpha :as s]
   [clojure.core.async :as a] ; for auto-redefining
   [clojure.spec.test.alpha]  ; instrument
   [speck.v1.utils :refer [maybe-conform]]
   ))


(comment ;; datastructure given by conforming the input looks like this:
  {:clauses [{:argslist [:unnamed (spec ...)] | [:named ([name spec] ...)]
              :ret spec
              :args-expr expr ; |-
              :fn-expr expr   ; |=
              } ...]
   :opts {:args .. :ret .. :fn .. :gen ..}})



;; main macro implementation ------------------------------------------------------


(defn- join-specs [s1 s2]
  (cond
    (and s1 s2) `(s/and ~s1 ~s2)
    s1 s1
    s2 s2))


(defn- arity-name [clause]
  (keyword (str "arity-" (-> clause :argslist count))))


(defn- arg-names [clause]
  (->> clause :argslist (map first)))


;;; args-form:


(defn- with-default-arg-names [specs]
  ;; %1, %2, etc
  (let [names (map (fn [n] (symbol (str "%" (inc n)))) (range))]
    (map vector names specs)))


(defn- ensure-names [[tag specs]]
  ;; [:unnamed [spec ...]] | [:named ([name spec] ...)] => ([name spec] ...)
  (if (= tag :unnamed)
    (with-default-arg-names specs)
    specs))


(defn- build-args-expr-spec [clause]
  (when-some [expr (:args-expr clause)]
    `(fn [{:keys [~@(arg-names clause)]}]
       ~expr)))


(defn- maybe-add-args-expr [form clause]
  ;; (s/cat ...) => (s/and (s/cat ...) args-expr)
  (join-specs form (build-args-expr-spec clause)))


(defn- argslist-spec [{:keys [argslist]}]
  ;; ([name spec] ...) => (s/cat :name spec ...)
  `(s/cat ~@(mapcat (fn [[k v]] [(keyword k) v]) argslist)))


(defn- build-spec-form [clause]
  (-> clause
      (assoc  :form (argslist-spec clause))
      (update :form maybe-add-args-expr clause)))


(defn- join-with-or [clauses]
  (->> clauses
       (mapcat #(vector (arity-name %) (:form %)))
       (concat `(s/or ,,,))))


(defn- maybe-join-with-or [clauses]
  (case (count clauses)
    0 nil
    1 (-> clauses first :form)
    (join-with-or clauses)))


(defn- args-form [clauses]
  (->> clauses
       (map build-spec-form)
       (maybe-join-with-or)))


;;; ret-form:

(defn- ret-form [clauses]
  ;; in multi-arity specs ret checking is happening in :fn
  ;; so just use any? for :ret
  (case (count clauses)
    0 nil
    1 (-> clauses first :ret)
    `any?))


;;; fn-form:

;; the map that is passed to multi-arity :fn specs looks like this:
;; {:args [:arity-2 {:x .. :y ..}], :ret ...}
;;
;; the generated code then looks something like this:
;; #(case (-> % :args key)
;;    :arity-1 (s/valid? ret-spec-1 (-> % :ret))
;;    :arity-2 (s/valid? ret-spec-2 (-> % :ret)))


(defn- build-fn-spec-single [clause]
  (when-some [body (:fn-expr clause)]
    `(fn [{~'% :ret, {:keys [~@(arg-names clause)]} :args}]
       ~body)))


(defn- build-fn-expr-multi [clause]
  ;; % is: {:args [:arity-1 {:%1 42}] :ret 42}
  (when-some [body (:fn-expr clause)]
    ;; XXX: rebinding % from the whole input map to ret value; may be confusing!
    `(let [{:keys [~@(arg-names clause)]} (-> ~'% :args val)
           ~'% (-> ~'% :ret)]
       ~body)))


(defn- arity+ret-and-fn-expr [clause]
  [(arity-name clause)
   (let [ret-expr `(s/valid? ~(:ret clause) (-> ~'% :ret))]
     (if-let [fn-expr (build-fn-expr-multi clause)]
       `(and ~ret-expr ~fn-expr)
       ret-expr))])


(defn- fn-form [clauses]
  (case (count clauses)
    0 nil
    1 (build-fn-spec-single (first clauses))
    `(fn [~'%]
       (case (-> ~'% :args key)
         ~@(mapcat arity+ret-and-fn-expr clauses)))))


;;; impl:

(defn- impl-fspec [clauses opts]
  (let [clauses (map #(update % :argslist ensure-names) clauses)]
    `(s/fspec :args ~(-> (args-form clauses) (join-specs (:args opts)))
              :ret  ~(-> (ret-form  clauses) (join-specs (:ret opts)))
              :fn   ~(-> (fn-form   clauses) (join-specs (:fn opts)))
              :gen  ~(:gen opts))))


(defn- impl [{:keys [clauses opts]}]
  ;; TODO: rename
  `(do (maybe-define-specs) ~(impl-fspec clauses opts)))



;; syntax specs -------------------------------------------------------------------


(s/def ::args any?)
(s/def ::ret any?)
(s/def ::fn any?)
(s/def ::gen any?)
(s/def ::opts (s/keys* :opt-un [::args ::ret ::fn ::gen]))


(defn- arrow? [x] (= x (symbol "=>")))
(defn- expr? [x] (not (arrow? x)))


(s/def ::syntax:single-named-arg
  (s/cat :name (s/and symbol? (comp not arrow?))
         :-    #(= % (keyword "-"))
         :spec expr?))


(s/def ::syntax:named-args
  (s/& (s/+ ::syntax:single-named-arg)
       (s/conformer #(->> % (map (juxt :name :spec))))))


(s/def ::syntax:unnamed-args
  ;; FIXME: should be s/*, but due to a bug in spec it doesn't work; use _ => ... for now
  (s/& (s/+ expr?)
       (s/conformer #(remove #{(symbol "_")} %))))


(s/def ::syntax:single-clause
  (s/cat
   :argslist (s/alt :named ::syntax:named-args
                    :unnamed ::syntax:unnamed-args)
   :=> arrow?
   :ret expr?
   :args-expr (s/& (s/? (s/cat :|- #{(symbol "|-")} :expr expr?)) (s/conformer #(:expr %)))
   :fn-expr   (s/& (s/? (s/cat :|= #{(symbol "|=")} :expr expr?)) (s/conformer #(:expr %)))
   ))


(s/def ::syntax
  (s/cat :clauses (s/* ::syntax:single-clause)
         :opts ::opts))


(def ^:dynamic *prod-mode*
  "If non-nil, the body of the `|` macro always expands to nil. Defaults to the
  value of CLJ_SPECK_PROD_MODE env variable."
  (System/getenv "CLJ_SPECK_PROD_MODE"))


(s/fdef | :args (s/cat :body (s/spec ::syntax)))

(defmacro |
  "Return an fspec object given its specification, and optionally (re-)register
  it (via s/def) and instrument the corresponding function. The resulting fspec
  object must be placed under the `:speck` key in the var metadata of the
  function it specs.

  The syntax is: `(| [clauses* opts*])`.

  Each clause defines a spec for a specific arity and has the form:
  `args+ => ret  |- args-expr  |= fn-expr`
  where:

  - `args` are either specs or triples `name :- spec`, where each spec
  positionally corresponds to an argument; for zero-arity clauses use `_` in
  place of args. If no names are given, default names %1, %2, etc are used.

  - `ret` is the return spec for this arity.

  - (optional) `args-expr` is a boolean expression used as an additional check
  in the resulting :args spec. All of the arguments are lexically bound by their
  names in the context of this expression.

  - (optional) `fn-expr` is a boolean expression used as an additional check in
  the resulting :fn spec. In addition to the arguments, the return value is
  bound to the name `%` in the context of this expression.

  `opts` are key+val pairs as in fspec\fdef; available opts are `:gen`, `:args`,
  `:ret` and `:fn`. The generator is passed directly to the underlying fspec
  call, while all other options are joined via `s/and` as the last conforming
  step to the corresponding specs.

  Note that the conformed values differ in single-arity and multi-arity specs.
  In case of multi-arity specs, the args map (as appearing in :args and :fn
  specs) is wrapped into a map-entry of the form `[:arity-N {...}]`, where N is
  the number of arguments in this arity.

  To negate the effects of this macro in production: see `*prod-mode*`.

  For details on registering and instrumenting: see `*auto-define-opts*`. Note
  that currently ALL of the specks in the current namespace are redefined on
  each `|` call."
  [body]
  (when-not *prod-mode*
    (impl (maybe-conform ::syntax body))))


;; redefining ---------------------------------------------------------------------


(defn- define-spec [var instrument-fn]
  (let [spec (-> var meta :speck)
        name (@#'s/->sym var)]
    (@#'s/def-impl name nil spec)
    (when instrument-fn (instrument-fn name))))


(defn- get-specked-vars [ns]
  (->> (vals (ns-interns ns))
       (filter #(some #{:speck} (-> % meta keys)))))


(defn- define-specs-in-current-ns [opts]
  (doseq [var (get-specked-vars *ns*)]
    (define-spec var (:instrument-fn opts))))



(def ^:dynamic *auto-define-opts*
  "Controls the behavior of the `|` macro in regards to spec registering and
  instrumenting on recompilation. Available opts are:

  :enabled - when set to false, `(| ...)` calls will only return the fspec object
  and will not try to register the resulting spec. Defaults to true.

  :timeout - time in ms to wait after a `(| ...)` call recompilation before
  redefining the spec. Defaults to 100.

  :verbose - whether to print a message to stdout each time the specs are being
  redefined. Defaults to false.

  :instrument-fn - an instrumenting function to be invoked after redefining a
  spec. Defaults to `orchestra.spec.test/instrument` or, if that is not found on
  classpath, to `clojure.spec.test.alpha/instrument`."
  {:enabled true
   :timeout 100
   :verbose false
   :instrument-fn (or (try (require '[orchestra.spec.test])
                           (resolve 'orchestra.spec.test/instrument)
                           (catch Exception _ nil))
                      clojure.spec.test.alpha/instrument)
   })


(defn maybe-define-specs
  "This function is an implementation detail and should not be used directly."
  []
  (let [{:keys [enabled timeout verbose] :as opts} *auto-define-opts*]
    (when enabled
      (a/go (a/<!! (a/timeout timeout))
            (when verbose (println "[speck] redefining specs in" (str *ns*)))
            (define-specs-in-current-ns opts)))))



;; reader literal -----------------------------------------------------------------


(defn speck-reader
  "Expands `#|[...]` to `{:speck (| [...])}`. Expected to be used in place of the
  attr-map inside a defn call. To register it, add:
  `{| speck.v1.core/speck-reader}`
  to the `data_readers.clj` file."
  [form]
  (cond
    (vector? form) {:speck `(| ~form)}
    :else          {:speck form}))


(defn speck-reader-bypass
  "When used instead of `speck-reader`, ignores its argument. Consider using
  `*prod-mode*` instead."
  [form]
  {})


;; (set! *data-readers* (assoc *data-readers* '| #'speck-reader))

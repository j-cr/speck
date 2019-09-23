# speck

> speck /spɛk/
> 
> 1. a tiny spot.

[Speck][] is a tiny library for your tiny [specs][]. It allows you to write
concise [function specs][] right inside your `defn`s and plays nice with others
because it doesn't introduce any custom defn wrappers. See for yourself:

[Speck]: https://github.com/j-cr/speck
[specs]: https://clojure.org/guides/spec
[function specs]: https://clojure.org/guides/spec#_spec_ing_functions

```clojure
(defn say-hello
  #|[(s/? string?) => string?]
  ([]     (say-hello "world"))
  ([name] (str "Hello, " name "!"))) 
```

Ok, admittedly this is a rather stupid example, so here's one from the official
docs instead:

```clojure
;;; before:

(s/fdef ranged-rand
  :args (s/and (s/cat :start int? :end int?)
               #(< (:start %) (:end %)))
  :ret int?
  :fn (s/and #(>= (:ret %) (-> % :args :start))
             #(< (:ret %) (-> % :args :end))))

(defn ranged-rand [start end]
  (- start (long (rand (- end start)))))


;;; after:

(defn ranged-rand
  #|[start :- int?, end :- int?  =>  int?
     |-  (< start end)
     |=  (and (>= % start) (< % end))]
  [start end]
  (- start (long (rand (- end start))))) 
```



## Setup

[![Clojars Project](https://img.shields.io/clojars/v/speck.svg)](https://clojars.org/speck)

Add this to your `project.clj`:

    [speck "1.0.0"]

After that, you'll need to register a [reader tag][] for speck; you can do it by
creating a file named `data_readers.clj` in the root of your classpath (i.e. in
the `src` directory) with the following content:

    {| speck.v1.core/speck-reader}

Alternatively, you can register it from REPL by executing this code:

    (set! *data-readers* (assoc *data-readers* '| #'speck.v1.core/speck-reader))

In order to enable :ret and :fn spec checking (**highly recommended**) you'll
need to add [orchestra][] too:

    [orchestra "2018.12.06-2"] ; check its github page for the latest version info

Speck will try to automatically use it instead of vanilla instrumentation when
it's available, but to make sure it's being used you can setup it manually:


```clojure 
(ns your.app
  (:require [speck.v1.core :as speck]
            [orchestra.spec.test :as orchestra]
            ...))
    
(alter-var-root #'speck/*auto-define-opts* assoc
  :instrument-fn orchestra/instrument)
```

That's it, you're good to go!

[reader tag]: https://clojure.org/reference/reader#tagged_literals
[orchestra]: https://github.com/jeaye/orchestra



## Usage

So how does that work?

Just put a `#|[...]` form inside your defn where the `attr-map` usually goes
(after the name, but before the argument list) and you're done! Under the hood,
this will expand to `{:speck (| [...])}`, where `|` is the macro that generates
the fspec and attaches it to your function.


The features included are:

- named and unnamed positional args specs
- lightweight syntax for args and fn specs
- arity overloading with separate return and fn specs for each arity
- varargs, keyword args and optional args are supported too
- automatic instrumentation
- specs are automatically redefined on defn's recompilation


### Syntax

Skip to the next section if you want examples. The general syntax looks
something like this:

```clojure 
 #|[arg-x       => ret-1  |- args-expr-1  |= fn-expr-1
    arg-x arg-y => ret-2  |- args-expr-2  |= fn-expr-2
    ...
    opts*] 
```

where

- `arg-x` and `arg-y` can be either specs or triplets `name :- spec`; if no
  names are given, default argument names `%1`, `%2`, etc are used

- `_` is used to indicate zero-argument clause, i.e. `#|[_ => ret ...]`

- `ret-1` and `ret-2` are ret specs for corresponding arities

- `args-expr`s are boolean expressions used to generate :args specs for
  corresponding arities; arguments are available by name

- `fn-expr`s are similar to `args-expr`s, except they generate :fn specs and in
  addition to the arguments the symbol `%` is available which refers to the
  return value
  
- `opts*` are `s/fspec` arguments; `:gen` is passed directly and all other opts
  are `s/and`ed to the corresponding specs


### Examples

You can find some simple testable examples [here](/test/speck/v1/examples.clj); for a reference of
all/most possible options check out the [test suite](/test/speck/v1/core_test.clj).

```clojure

;; basic rule is: input => output
(defn abs
  #|[number? => (s/and number? pos?)]
  [x] ...)


;; if there are no inputs, use `_`:
(defn pandorandom
  #|[_ => any?]
  [] ...)


;; you can add names to the arguments, though it is optional:
(defn fraction
  #|[numerator :- int?, denominator :- pos?  =>  ratio?]
  [num den] ...)


;; specs are always matched with args based on their order (think s/cat):
(defn rotate
  #|[direction :- ::vec-2d, angle :- ::radians  =>  ::vec-2d]
  [{:keys [x y]} a]
  ...)


;; different arities can have different ret specs: 
(defn map
  #|[fn? => ::transducer, fn? (s/+ seqable?) => seq?]
  ([f] ...)
  ([f coll & colls] ...))


;; note that you can use `s/?` for optional args:
(defn join
  #|[(s/? any?) (s/coll-of any?) => string?]
  ([coll] ...)
  ([sep coll] ...))
  

;; use `s/keys*` for keyword args:
(defn start-server
  #|[fn? (s/keys* :opt-un [::host ::port])  =>  ::server]
  [handler & {:keys [host port]}]
  ...)


;; to check predicates against several args at once, use |- syntax:
(defn interval
  #|[start :- number?, end :- number?  =>  ::interval
     |- (< start end)]
  [a b] ...)


;; note that unnamed args will get default names (%1, %2, ...)
;; this is equivalent to the previous example:
(defn interval
  #|[number? number? => ::interval |- (< %1 %2)]
  [start end] ...)


;; use |= to check invariants connecting the arguments and the return value;
;; the return value is bound to the `%` symbol:
(defn select-keys
  #|[m :- map?, ks :- (s/coll-of any?)  =>  map?
     |=  (= (set ks) (set (keys %)))]
  [m ks]
  ...)


;; unlike in clojure's anonymous functions, `%` is NOT the same as `%1`!
;; this is equivalent to the previous example (but much more confusing):
(defn select-keys
  #|[map?, (s/coll-of any?)  =>  map?
     |=  (= (set %2) (set (keys %)))]
  [m ks]
  ...)


;; finally, you can directly specify fspec opts, such as :gen...
;; :args, :ret and :fn opts will be added to the corresponding specs via s/and:
(defn frobnicate
  #|[x? => foo?    ;-> (s/and foo? qux?)
     x? y? => baz? ;-> (s/and baz? qux?)
     :ret qux?]
  ...)


;; in fact, you can eschew the speck syntax completely and use vanilla clojure
;; spec syntax if that's your thing:
(defn abs
  #|[:args (s/cat :x int?), :ret nat-int?]
  ...)


;; oh, and by the way, you can entirely bypass using the reader literal; this is
;; more wordy, but can be useful when you need to attach more meta to your fn:
;; (:require [speck.v1.core :as speck :refer [|]])
(defn foo
  {:speck (| [...])
   :some-other meta}
  ...)


;; importantly, all of this can be used with any defn-like macro:
(defn foo #|[...])
(defn- foo #|[...])
(defmacro foo #|[...])
(defmulti foo #|[...])  ; but see https://clojure.atlassian.net/browse/CLJ-2450
(defun foo #|[...])     ; from https://github.com/killme2008/defun
(defroutes foo #|[...]) ; not sure why you'd want that... but you get the idea!

```


### Automatic redefining and instrumentation

When you recompile a *specked* function, speck will detect that and redefine all
*specks* in the same namespace (including the one you've just recompiled);
better granularity cannot be achieved unfortunately, but it works good enough in
practice.

To control this behavior (enable\disable it, turn debug printing on and off,
etc) alter the var `speck.v1.core/*auto-define-opts*`.

Upon redefining, the affected functions will also be instrumented using the
functions specified under `:instrument-fn` in `*auto-define-opts*`.

You can manually (re)define the *specks* by calling `define-specs-in-current-ns`:

    (speck/define-specs-in-current-ns {:instrument-fn orchestra/instrument})


### Production mode

You can change the tagged literal reader from `speck-reader` to
`speck-reader-bypass` to eliminate all the `#|[...]` forms from your code.

TODO: env knob


## FAQ

#### Is this library stable?

I consider v1 API to be pretty much finished, thus no major breaking changes
should happen here. Note however that spec itself is in alpha, so when the
[next version](https://github.com/clojure/spec-alpha2/) will be released this
library *might* get a breaking v2 release too.


#### Is ClojureScript supported? 

[Not yet](https://github.com/j-cr/speck/issues/2). Cljs supports static metadata
on vars, so the port should be pretty straightforward; I just haven't done it
yet.


#### My ret\fn specs don't work, is that a bug?

Default clojure's `instrument` [only checks args specs](https://clojure.org/guides/spec#_instrumentation)
(don't ask me why, I don't know); use [orchestra](https://github.com/jeaye/orchestra) instead.


#### Isn't this library abusing the tagged literals feature?

I guess so... but for a worthy cause though! (Right?) You can always use the
longer syntax `{:speck (| [...])}` if you so wish.


#### [...but is there any advantage to that over a custom defn macro?][weavejester]
[weavejester]:(https://www.reddit.com/r/Clojure/comments/d7wx0h/new_better_version_of_speck_a_concise_syntax_for/f167hm9/)

My take on this is as follows: custom defn-wrapping macros don't compose, so as
clojure ecosystem grows and new feature are developed you might face a situation
where you have to choose between two (or more) incompatible defn
wrappers. (Also, abstractions that compose poorly are just bad in general.)

On the other hand speck is compatible with any defn-like macro that produces a
var and accepts a metadata map. For example, you can use it with defmacro,
defmulti or custom 3rd-party macros like e.g. [defun](https://github.com/killme2008/defun).

Also, it's compatible with any libraries that extend your definitions in the
same way speck does, although admittedly you would have to use the longer syntax
for that:

```clojure
(defn foo
  {:speck (| [...])
   :shpec (something (completely different))}
  ...)
```

In the end, clojure does provide this elegant extension point via vars+metadata, so why not use that?


#### Should I abuse this library by writing absurdly huge inline specs and never using vanilla `s/fdef` again even where it's more appropriate?
No.


#### Is this library any good?
[Yes.](https://news.ycombinator.com/item?id=3067434)



## License

Copyright © 2018-2019 https://github.com/j-cr

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.


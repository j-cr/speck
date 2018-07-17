# speck

> speck /spɛk/
> 
> 1. a tiny spot.

[Speck][] is a tiny library for your tiny [specs][]. It allows you to write concise
[function specs][] right inside your `defn`s and does not introduce any custom
defn wrappers. See for yourself:

```clojure
(defn say-hello
  {:speck (| (s/? string?) => string?)}
  ([]     (say-hello "world"))
  ([name] (str "Hello, " name "!")))
```


[Speck]: https://github.com/j-cr/speck
[specs]: https://clojure.org/guides/spec
[function specs]: https://clojure.org/guides/spec#_spec_ing_functions

## Setup

[![Clojars Project](https://img.shields.io/clojars/v/speck.svg)](https://clojars.org/speck)

**Alpa software warning: the API is not stable yet. Proceed with caution and
expect things to break[⁽*⁾](/#faq).**

Add this to your `project.clj`:

    [speck "0.0.1"]

You'll probably want to add [expound][] and [orchestra][] too:

    [expound "0.7.1"]           ; or whatever version is the latest - 
    [orchestra "2017.11.12-1"]  ;  check their respective github pages

[expound]: https://github.com/bhb/expound
[orchestra]: https://github.com/jeaye/orchestra

## Usage

So how does that work?

1. Put spec definitions under a `:speck` key in the metadata of your
   functions.
2. Put a call to `gen-specs-in-ns` at the end of your source file - that will
   generate `s/fdef`s for all the functions in that file. You can also use
   `(gen-spec your-fn)` if you want to spec a single function.

Speck supports varargs, keyword args, optional args and arity overloading with
different return specs for each arity. It also provides 2 options for the
syntax: 

```clojure
;; the lispy one:
(=> [args] ret, [more args] other-ret)

;; the "arrows in nice places" one:
(| args => ret, more args => other-ret)
```

More examples:

```clojure

(ns your.ns
  (:require [speck.v0.core :as speck :refer [=> |]]
            [clojure.spec.alpha :as s]))

;; `=>` expects pairs of [arg-specs*] and ret-spec:
(defn abs
  {:speck (=> [number?] (s/and number? pos?))}
  [x] ...)

;; specs are matched with args based on their order (think s/cat):
(defn fraction
  {:speck (=> [int? pos?] ratio?)}
  [num den] ...)

;; different arities can have different ret specs:
(defn i-can-haz-cheezburger?
  {:speck (=> [bread? meat?]         #{:no :nope}
              [bread? meat? cheese?] #{:yes :yup})}
  ([a b] ...)
  ([a b c] ...))

;; use `s/*` for varargs:
(defn sandwich
  {:speck (=> [bread? (s/* good-stuff?)] ::sandwich)}
  [b & more] ...)

;; note that you can use `s/?` for optional args:
(defn burger
  {:speck (=> [(s/? cheese?) patty? bun?] ::burger)}
  ([b c] ...)
  ([a b c] ...))

;; keyword args are the same as varargs - just use `s/keys*`:
(defn enhance-burger
  {:spec (=> [::burger (s/keys* :opt-un [::topping ::seasoning])]
             ::burger)}
  [burger & {:keys [topping seasoning]}]
  ...)

;; you can use everything of the above with the alternative syntax if you prefer:
(defn burger
  {:speck (| (s/? cheese?) patty? bun? => ::burger)}
  ([b c] ...)
  ([a b c] ...))

;; call this after all the functions in a namespace have been defined:
(speck/gen-specs-in-ns)

```

Note that speck relies on `:arglists` metadata in order to work, so if it's
messed up for some reason (maybe you're trying to *speck* a macro with a
manually set arglists meta? (spoiler: don't do that)) bad things will happen.

Also note that **ret specs won't work if you use
`clojure.spec.test.alpha/instrument`**. You *have* to use [orchestra][]
instead.

[orchestra]: https://github.com/jeaye/orchestra

## FAQ

#### Why `:speck`? Aren't non-namespaced keywords bad?

I know it's NOT GOOD™ to use non-namespaced keywords in var's metadata (in fact,
I believe they are considered to be reserved for clojure's own use), but
something like `::speck/def (=> ...)` would be just too much typing, and for
library like this user convenience is more important; after all, in practice
clojure would hardly ever use a key named "speck" as a part of its standard var
metadata.

However, having said that, for the next iteration I'm thinking about changing
the syntax to:

    (defn my-inc
      {=> '([number?] better-number?)}  ; `=>` is just an alias for :speck.v0.core/=>
      [x] ...)

It's shorter and better conveys the idea that this is all just data - nothing
actually happens until you call `gen-spec`. Tell me what you think in the
[issues](https://github.com/j-cr/speck/issues/3).


#### Is ClojureScript supported? 

[Not yet](https://github.com/j-cr/speck/issues/2). Cljs supports static metadata
on vars, so the port should be pretty straightforward; I just haven't done it
yet.


#### Are `:fn` specs supported?

Not yet, but it's the first on my todo list. Feedback wanted! I'm thinking about
something along this lines:

    (=> (& [int? int?] (all-good? :x :y))  ; keywords refer to the names of the args
        (& number?     (< :x % :y)))       ; % refers to the return value

Please tell me your thoughts on the proposed syntax in the
[issues](https://github.com/j-cr/speck/issues/1).


#### Should I abuse this library by writing absurdly huge inline specs and never using vanilla `s/fdef` again even where it's more appropriate?
No.


#### Is this library any good?
[Yes.](https://news.ycombinator.com/item?id=3067434)



## License

Copyright © 2018 https://github.com/j-cr

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.


(ns speck.v1.utils
  (:require [clojure.spec.alpha :as s]))

(defn maybe-conform [spec x]
  (let [res (s/conform spec x)]
    (if (= res ::s/invalid)
      (throw (ex-info (s/explain-str spec x)
                      (s/explain-data spec x)))
      res)))

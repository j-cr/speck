(defproject speck "1.1.0"
  :description "Concise syntax for your function specs"
  :url "https://github.com/j-cr/speck"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1" :scope "provided"]
                 [org.clojure/core.async "0.4.474"]]
  :profiles {:dev {:dependencies [[orchestra "2018.12.06-2"]]}})

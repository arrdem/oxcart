(defproject me.arrdem/oxcart (slurp "VERSION")
  :description "An experimental optimizing compiler for Clojure code"
  :url "http://github.com/arrdem/oxcart"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                 [org.clojure/clojure            "1.5.1"]
                 [org.clojure/tools.analyzer     "0.2.3"]
                 [org.clojure/tools.analyzer.jvm "0.2.2"]
                 [org.clojure/tools.emitter.jvm  "0.0.1-SNAPSHOT"]
                 [org.clojure/tools.reader       "0.8.5"]
                 [com.taoensso/timbre            "3.2.1"]
                 ]
  :whitelist #"oxcart"
  :injections [(set! *print-length* 10)
               (set! *print-level*  10)])

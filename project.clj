(defproject me.arrdem/oxcart (slurp "VERSION")
  :description "An experimental optimizing compiler for Clojure code"
  :url "http://github.com/arrdem/oxcart"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                 [org.clojure/clojure "1.7.0"]
                 [org.clojure/tools.analyzer "0.5.2-SNAPSHOT"]
                 [org.clojure/tools.analyzer.jvm "0.5.3-SNAPSHOT"]
                 [org.clojure/tools.emitter.jvm "0.1.0-SNAPSHOT"]
                 [org.clojure/tools.reader "0.10.0"]
                 [com.taoensso/timbre "4.2.0"]
                 ]
  :main oxcart.core
  :min-lein-version "2.0.0"
  :source-paths ["src/main/clojure"
                 "src/bench/clojure"]
  :java-source-paths ["src/main/java"]
  :whitelist #"oxcart"
  :injections [(set! *print-length* 10)
               (set! *print-level*  10)])

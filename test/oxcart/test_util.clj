(ns oxcart.test-util
  (:refer-clojure :exclude [macroexpand-1 macroexpand])
  (:require [clojure.test :refer :all]
            [clojure.tools.analyzer.jvm :as ana.jvm]
            [clojure.tools.analyzer
             :refer [macroexpand-1
                     macroexpand]
             :as ana]))

(defmacro is-not [form]
  `(is (not ~form)))

(defmacro ast [mform]
  `(binding [ana/macroexpand-1 ana.jvm/macroexpand-1
             ana/create-var    ana.jvm/create-var
             ana/parse         ana.jvm/parse
             ana/var?          var?]
     (ana.jvm/analyze (quote ~mform)
                      (ana.jvm/empty-env))))

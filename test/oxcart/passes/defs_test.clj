;;   Copyright (c) Reid McKenzie, Rich Hickey & contributors. The use
;;   and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0
;;   (http://opensource.org/licenses/eclipse-1.0.php) which can be
;;   found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be
;;   bound by the terms of this license.  You must not remove this
;;   notice, or any other, from this software.

(ns oxcart.passes.defs-test
  (:require [oxcart.core :as oxcart]
            [oxcart.passes.defs :refer :all]
            [oxcart.util :refer [ast]]
            [oxcart.test-util :refer [is-not]]
            [clojure.set :refer :all]
            [clojure.test :refer :all]))


(def victim-ast
  (let [forms (atom {})]
    (oxcart/eval
     '(do (def ^:private foo 3)
          (def ^:const quxx 4)
          (def bar 5)
          (def ^:dynamic blat 6)
          (defn baz [z]
            (mod (+ foo quxx bar z) z)))
     {:forms forms})
    @forms))


(deftest locate-defs-test
  (let [annotated-ast (locate-defs victim-ast {})

        {:keys [symbols public private const dynamic]}
                      (get annotated-ast 'oxcart.passes.defs-test)]

    (doseq [sym ['foo 'quxx 'bar 'baz]]
      (is (contains? symbols sym)
          (str sym "not found in the defs!")))

    (is (= #{'foo} private))
    (is (= #{'bar 'baz 'quxx 'blat} public))
    (is (= #{'quxx 'foo 'bar 'baz} const))
    (is (= #{'blat} dynamic))))


(deftest locate-var-as-value-test
  (let [{:keys [var-usage] :as ast}
        (locate-var-as-value victim-ast {})]
    (doseq [var [#'oxcart.passes.defs-test/foo
                 #'oxcart.passes.defs-test/quxx
                 #'oxcart.passes.defs-test/bar
                 #'oxcart.passes.defs-test/blat]]
      (is (= (var-usage var) :value)))
    (is (= :target (var-usage #'clojure.core/mod)))))

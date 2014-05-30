;;   Copyright (c) Reid McKenzie, Rich Hickey & contributors. The use
;;   and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0
;;   (http://opensource.org/licenses/eclipse-1.0.php) which can be
;;   found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be
;;   bound by the terms of this license.  You must not remove this
;;   notice, or any other, from this software.

(ns oxcart.passes.lambda-lift-test
  (:require [oxcart.passes.lambda-lift :as ll]
            [oxcart.passes.emit-clj :as eclj]
            [oxcart]
            [clojure.test :refer :all]))


(deftest lambda-lift-tests

  ;; Basic test case
  ;; ---------------
  (let [case '(do (def foo
                    (fn [x y]
                      (#(+ x %1) y)))
                  (foo 3 5))]
    (is (= (eval case)
           (oxcart/eval case)
           (-> (let [forms (atom {})]
                 (oxcart/eval case {:forms forms})
                 @forms)
               (ll/lift-lambdas {})
               (eclj/emit-clojure {})
               (eval)))))

  ;; Closure of closures case
  ;; ------------------------
  (let [case '(let [x 1]
                (letfn [(bar [z] (+ x z))
                        (baz [z] (dec (bar z)))
                        (wat [z] (* 3 z (baz z)))]
                  ((juxt bar baz wat) x)))]

    (is (= (eval case)
           (oxcart/eval case)
           (-> (let [forms (atom {})]
                 (oxcart/eval case {:forms forms})
                 @forms)
               (ll/lift-lambdas {})
               (eclj/emit-clojure {})
               (eval))))))

;;   Copyright (c) Reid McKenzie, Rich Hickey & contributors. The use
;;   and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0
;;   (http://opensource.org/licenses/eclipse-1.0.php) which can be
;;   found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be
;;   bound by the terms of this license.  You must not remove this
;;   notice, or any other, from this software.

(ns oxcart.passes.fn-reduction-tests
  (:require [oxcart.core :as oxcart]
            [oxcart.passes.fn-reduction :refer :all]
            [oxcart.emitter.clj :as eclj]
            [oxcart.test-util :refer [is-not]]
            [clojure.test :refer :all]))

(deftest fn-reduction-tests

  ;; Basic test case
  ;; -------------------
  (let [case '(do (def foo
                    (fn ([x]   x)
                       ([x y] y)))

                  [(foo 3 5)
                   (foo 3)])]

    (is (= (eval case)
           (oxcart/eval case)
           (-> (let [forms (atom {})]
                 (oxcart/eval case {:forms forms})
                 @forms)
               (reduce-fn-arities {})
               (eclj/emit {})
               (eval)))))

  ;; Taken as value case
  ;; -----------------------
  (let [case '(do (def foo
                    (fn ([x]   x)
                       ([x y] y)))

                  [(map foo (range 10))
                   (foo 3)])]

    (is (= (eval case)
           (oxcart/eval case)
           (-> (let [forms (atom {})]
                 (oxcart/eval case {:forms forms})
                 @forms)
               (reduce-fn-arities {})
               (eclj/emit {})
               (eval)))))

  ;; Recurs through name case
  ;; ----------------------------
  (let [case '(do (def foo
                    (fn quxx
                      ([x]   3)
                      ([x y] (quxx x))))

                  [(map foo (range 10))
                   (foo 3)])]

    (is (= (eval case)
           (oxcart/eval case)
           (-> (let [forms (atom {})]
                 (oxcart/eval case {:forms forms})
                 @forms)
               (reduce-fn-arities {})
               (eclj/emit {})
               (eval)))))

  ;; Recurs through name which shadows def
  ;; -----------------------------------------
  (let [case '(do (def quxx 
                    (fn [x] 5))

                  (def foo
                    (fn quxx
                      ([x]   3)
                      ([x y] (quxx x))))

                  [(map foo (range 10))
                   (foo 3)
                   (quxx 4)])]

    (is (= (eval case)
           (oxcart/eval case)
           (-> (let [forms (atom {})]
                 (oxcart/eval case {:forms forms})
                 @forms)
               (reduce-fn-arities {})
               (eclj/emit {})
               (eval))))))

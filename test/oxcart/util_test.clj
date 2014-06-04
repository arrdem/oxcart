;;   Copyright (c) Reid McKenzie, Rich Hickey & contributors. The use
;;   and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0
;;   (http://opensource.org/licenses/eclipse-1.0.php) which can be
;;   found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be
;;   bound by the terms of this license.  You must not remove this
;;   notice, or any other, from this software.

(ns oxcart.util-test
  (:require [oxcart.util :refer :all]
            [clojure.test :refer :all]))


(deftest take-when-test
  (is (= [1 [2 3]]
         (take-when (constantly true)
                    [1 2 3])))

  (is (= [nil [1 2 3]]
         (take-when (constantly false)
                    [1 2 3]))))


(deftest map-vals-test
  (is (= {:a 2}
         (map-vals {:a 1} inc))))


(deftest fix-test
  (is (= 2
         (fix #(mod %1 3) 5)))

  (is (= 1
         (fix (fn [x]
                (if (>= x 2)
                  (/ x 2)
                  x))
              1024))))


(deftest update-tests
  (is (= {:foo 4}
         (update {:foo 3}
                 :foo inc)))

  (is (= {:foo {:bar 4}}
         (update {:foo {}}
                 :foo #(assoc %1 :bar 4)))))

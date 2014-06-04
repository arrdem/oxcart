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

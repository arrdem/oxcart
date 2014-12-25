(ns oxcart.passes.tree-shake-test
  (:require [oxcart.passes.tree-shake :refer :all]
            [clojure.test :refer :all]))

(deftest global-reach-set-test
  (let [tree {:foo #{}
              :bar #{:foo}
              :baz #{:bar}}
        tree (global-reach-set tree)]
    (is (= (:foo tree) #{}))
    (is (= (:bar tree) #{:foo}))
    (is (= (:baz tree) #{:bar :foo}))))

(deftest deps-to-uses-test
  ;; The tested function is trivially correct, but generative testing
  ;; is fun.
  (let [keywords (->> (repeatedly gensym)
                      (map (comp keyword name))
                      (take 100)
                      (vec))
        keys     (shuffle keywords)
        deps     (->> (for [k keys]
                        [k (-> keywords
                               count
                               rand-int
                               (take (shuffle keywords))
                               set)])
                      (into {}))
        uses     (deps-to-uses deps)]
    (doseq [[k deps] deps
            d  deps]
      (is (contains? (get uses d) k)))))

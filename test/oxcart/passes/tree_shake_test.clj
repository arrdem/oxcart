;;   Copyright (c) Reid McKenzie, Rich Hickey & contributors. The use
;;   and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0
;;   (http://opensource.org/licenses/eclipse-1.0.php) which can be
;;   found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be
;;   bound by the terms of this license.  You must not remove this
;;   notice, or any other, from this software.

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

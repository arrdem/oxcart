;;   Copyright (c) Reid McKenzie, Rich Hickey & contributors. The use
;;   and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0
;;   (http://opensource.org/licenses/eclipse-1.0.php) which can be
;;   found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be
;;   bound by the terms of this license.  You must not remove this
;;   notice, or any other, from this software.

(ns oxcart.passes.lambda-lift-test
  (:require [oxcart.passes.lambda-lift :refer :all]
            [oxcart.test-util :refer :all]
            [clojure.test :refer :all]))


;; Basic test case
;; ---------------

(def foo
  (fn [x y]
    (#(+ x %1) y)))

;; This should lift to

(def fn#
  (fn [x _1]
    (+ x _1)))

(def foo
  (fn [x y]
    ((partial fn# x) y)))


;; Letfn case
;; -------------------
;;
;; (defn foo [x]
;;   (letfn [(bar [z] (+ x z))
;;           (baz [z] (dec (bar z)))
;;           (wat [z] (* 3 z (baz z)))]
;;     ((juxt bar baz wat) x)))

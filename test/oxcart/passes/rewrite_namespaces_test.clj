;;   Copyright (c) Reid McKenzie, Rich Hickey & contributors. The use
;;   and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0
;;   (http://opensource.org/licenses/eclipse-1.0.php) which can be
;;   found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be
;;   bound by the terms of this license.  You must not remove this
;;   notice, or any other, from this software.

(ns oxcart.passes.rewrite-namespaces-test
  (:refer-clojure :exclude [eval])
  (:require [oxcart.util :refer [ast]]
            [oxcart.passes.rewrite-namespaces :refer :all]
            [clojure.test :refer :all]))


(deftest import*-tests
  (let [ast (ast '(do (ns victim)
                      (import 'clojure.lang.RT)))]
    (is (import? (:ret ast)))
    (is (= [''clojure.lang.RT] (import->form (:ret ast))))))


(deftest require*-tests
  (let [ast (ast '(do (ns victim)
                      (require 'clojure.core.cache)))]
    (is (require? (:ret ast)))
    (is (= [''clojure.core.cache] (require->form (:ret ast))))))


(deftest use*-tests
  (let [ast (ast '(do (ns victim)
                      (use 'clojure.core.cache)))]
    (is (use? (:ret ast)))
    (is (= [''clojure.core.cache] (use->form (:ret ast))))))

(ns oxcart.pattern-test
  (:require [oxcart.pattern :as p]
            [oxcart.test-util :refer :all]
            [clojure.test :refer :all]))


(def foo
  (ast
   (defn foo [x]
     (inc x))))


(def bar
  (ast
   (def bar
     (fn [x] (dec x)))))


(def fail
  (ast
   (fn [] 1)))


(deftest def?-tests
  (is (p/def? foo))
  (is (p/def? bar))
  (is-not (p/def? fail)))


(deftest def->symbol-tests
  (is (= 'foo (p/def->symbol foo)))
  (is (= 'bar (p/def->symbol bar)))
  (is (nil? (p/def->symbol fail))))


(def private-defn
  (ast
   (defn ^:private baz [x]
     (* x x))))


(def private-def
  (ast
   (def ^:private quxx
     (fn [x] (mod x 2)))))


(deftest public?-tests
  (is (p/public? foo))
  (is (p/public? bar))
  (is-not (p/public? fail))
  (is-not (p/public? private-defn)))


(deftest private?-tests
  (is (p/private? private-defn))
  (is (p/private? private-def))
  (is-not (p/private? foo))
  (is-not (p/private? bar)))


(def dynamic-defn
  (ast
   (defn ^:dynamic fred [x]
     (rand-int (mod x 4)))))


(def dynamic-def
  (ast
   (def ^:dynamic *foo* nil)))


(deftest dynamic?-test
  (is (p/dynamic? dynamic-defn))
  (is (p/dynamic? dynamic-def))
  (is-not (p/dynamic? private-defn))
  (is-not (p/dynamic? private-def))
  (is-not (p/dynamic? foo))
  (is-not (p/dynamic? bar))
  (is-not (p/dynamic? fail)))


(def const-defn
  (ast
   (defn ^:const corge [x]
     (/ x x))))


(def const-def
  (ast
   (def ^:const grault
     (fn [x] (- x 2)))))


(deftest const?-tests
  (is (p/const? const-defn))
  (is (p/const? const-def))
  (is (p/const? private-defn))
  (is (p/const? private-def))
  (is-not (p/const? dynamic-defn))
  (is-not (p/const? dynamic-def)))

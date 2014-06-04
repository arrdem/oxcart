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


(def addition
  (ast (let [x 1] (+ x 2))))


(deftest def?-tests
  (is (p/def? foo))
  (is (p/def? bar))
  (is-not (p/def? fail)))


(deftest def->symbol-tests
  (is (= 'foo (p/def->symbol foo)))
  (is (= 'bar (p/def->symbol bar)))
  (is (nil? (p/def->symbol fail))))


(deftest def->var-tests
  (is (var? (p/def->var foo)))
  (is (var? (p/def->var bar)))
  (is-not (var? (p/def->var fail))))


(deftest top-level?-tests
  (is (p/top-level? foo))
  (is (p/top-level? bar))
  (is (p/top-level? fail))
  (is-not (p/top-level?
           (-> addition
               :body
               :ret))))


(deftest fn?-tests
  (is (p/fn? (:init foo)))
  (is (p/fn? (:init bar)))
  (is (p/fn? fail))
  (is-not (p/fn? addition)))


(deftest fn->name-tests
  (is (p/fn->name (:init foo)))
  (is (p/fn->name (:init bar)))
  (is (p/fn->name fail))
  (is-not (p/fn->name addition)))


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

(ns oxcart.pattern-test
  (:require [oxcart.pattern :refer :all]
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
  (is (def? foo))
  (is (def? bar))
  (is-not (def? fail)))

(deftest def->symbol-tests
  (is (= 'foo (def->symbol foo)))
  (is (= 'bar (def->symbol bar)))
  (is (nil? (def->symbol fail))))

(def private-defn
  (ast
   (defn ^:private baz [x]
     (* x x))))

(def private-def
  (ast
   (def ^:private quxx
     (fn [x] (mod x 2)))))

(deftest public?-tests
  (is (public? foo))
  (is (public? bar))
  (is-not (public? fail))
  (is-not (public? private-defn)))

(deftest private?-tests
  (is (private? private-defn))
  (is (private? private-def))
  (is-not (private? foo))
  (is-not (private? bar)))

(def dynamic-defn
  (ast
   (defn ^:dynamic fred [x]
     (rand-int (mod x 4)))))

(def dynamic-def
  (ast
   (def ^:dynamic *foo* nil)))

(deftest dynamic?-test
  (is (dynamic? dynamic-defn))
  (is (dynamic? dynamic-def))
  (is-not (dynamic? private-defn))
  (is-not (dynamic? private-def))
  (is-not (dynamic? foo))
  (is-not (dynamic? bar))
  (is-not (dynamic? fail)))

(def const-defn
  (ast
   (defn ^:const corge [x]
     (/ x x))))

(def const-def
  (ast
   (def ^:const grault
     (fn [x] (- x 2)))))

(deftest const?-tests
  (is (const? const-defn))
  (is (const? const-def))
  (is (const? private-defn))
  (is (const? private-def))
  (is-not (const? dynamic-defn))
  (is-not (const? dynamic-def)))

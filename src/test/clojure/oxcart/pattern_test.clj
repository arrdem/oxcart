(ns oxcart.pattern-test
  (:require [oxcart.pattern :as p]
            [oxcart.util :refer [ast]]
            [oxcart.test-util :refer [is-not]]
            [clojure.test :refer :all]))

(def ops
  #{:binding :catch :const :def :do :fn :fn-method
    :host-call :host-field :host-interop :if :invoke
    :let :letfn :local :loop :map :maybe-class
    :maybe-host-form :new :quote :recur :set :set!
    :throw :try :var :vector :with-meta})

(def foo
  (ast
   '(defn foo [x]
      (inc x))))

(def bar
  (ast
   '(def bar
      (fn [x] (dec x)))))

(def fail
  (ast
   '(fn [] 1)))

(def addition
  (ast '(let [x 1] (+ x 2))))

(deftest def?-tests
  (doseq [op ops]
    (if (not= op :def)
      (is-not (p/def? {:op op}))
      (is (p/def? {:op op}))))
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
  (is-not (p/top-level?
           (-> addition
               :body
               :ret))))

(deftest fn?-tests
  (doseq [op ops]
    (if (not= op :fn)
      (is-not (p/fn? {:op op}))
      (is (p/fn? {:op op}))))
  (is (p/fn? (:init foo)))
  (is (p/fn? (:init bar)))
  (is (p/fn? fail))
  (is-not (p/fn? addition)))

(deftest fn->name-tests
  (is (p/fn->name (:init foo)))
  (is (p/fn->name (:init bar)))
  (is (p/fn->name fail))
  (is (= "bar" (p/fn->name {:op :fn :internal-name "bar"})))
  (is-not (p/fn->name addition)))

(deftest let?-tests
  (doseq [op ops]
    (if (not= op :let)
      (is-not (p/let? {:op op}))
      (is (p/let? {:op op}))))
  (is-not (p/let? foo))
  (is-not (p/let? (:init foo)))
  (is-not (p/let? bar))
  (is-not (p/let? (:init bar)))
  (is (p/let? addition)))

(deftest letfn?-tests
  (doseq [op ops]
    (if (not= op :letfn)
      (is-not (p/letfn? {:op op}))
      (is (p/letfn? {:op op}))))
  (is-not (p/letfn? addition))
  (is-not (p/letfn? foo))
  (is-not (p/letfn? (:init foo)))
  (is-not (p/letfn? bar))
  (is-not (p/letfn? (:init bar)))
  (is-not (p/letfn? fail))
  (is-not (p/letfn? (:init fail))))

(deftest binding?-tests
  (doseq [op ops]
    (if (not= op :binding)
      (is-not (p/binding? {:op op}))
      (is (p/binding? {:op op})))))

(deftest binding->symbol-tests
  ;; FIXME:
  ;;   This test depends on internals of t.a.jvm's renaming and
  ;;   _technically_ is implementation defined but it's probably safe
  ;;   for now.
  (let [b (first (:bindings addition))]
    (is (= 'x__#0 (p/binding->symbol b)))))

;; FIXME:
;;   Tests for binding->value should go here, however as with
;;   binding->symbol it's a pretty worthless and definitional
;;   test. Omitted for now, fix later.

(deftest local?-tests
  (doseq [op ops]
    (if (not= op :local)
      (is-not (p/local? {:op op}))
      (is (p/local? {:op op}))))

  (is (p/local?
       (-> addition :body :ret :args first))))

(def private-defn
  (ast
   '(defn ^:private baz [x]
      (* x x))))

(def private-def
  (ast
   '(def ^:private quxx
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
   '(defn ^:dynamic fred [x]
      (rand-int (mod x 4)))))

(def dynamic-def
  (ast
   '(def ^:dynamic *foo* nil)))

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
   '(defn ^:const corge [x]
      (/ x x))))

(def const-def
  (ast
   '(def ^:const grault
      (fn [x] (- x 2)))))

(deftest const?-tests
  (is (p/const? const-defn))
  (is (p/const? const-def))
  (is (p/const? private-defn))
  (is (p/const? private-def))
  (is-not (p/const? dynamic-defn))
  (is-not (p/const? dynamic-def)))

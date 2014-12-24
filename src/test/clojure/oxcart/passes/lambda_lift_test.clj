(ns oxcart.passes.lambda-lift-test
  (:require [oxcart.passes.lambda-lift :as ll]
            [oxcart.emitter.clj :as eclj]
            [oxcart.core :as oxcart]
            [clojure.test :refer :all]))

(deftest lambda-lift-tests

  ;; Basic test case
  ;; ---------------
  (let [case '(do (def foo
                    (fn [x y]
                      (#(+ x %1) y)))
                  (foo 3 5))]

    (is (= (eval case)
           (oxcart/eval case)
           (-> (let [forms (atom {})]
                 (oxcart/eval case {:forms forms})
                 @forms)
               (ll/lift-lambdas {})
               (eclj/emit {})
               (eval)))))

  ;; Nested letfn case
  ;; -----------------
  (let [case '(let [x 3]
                (letfn [(g [y] (dec y))]
                  (letfn [(h [z] (* 2 (g z)))]
                    (h x))))]

    (is (= (eval case)
           (oxcart/eval case)
           (-> (let [forms (atom {})]
                 (oxcart/eval case {:forms forms})
                 @forms)
               (ll/lift-lambdas {})
               (eclj/emit {})
               (eval)))))

  ;; Side by side letfn case
  ;; -----------------------
  (let [case '(let [x 1]
                (letfn [(bar [z] (+ x z))]
                  (bar x))
                (letfn [(bar [z] (* (wat z) 3))
                        (wat [z] (* 3 z))]
                        ((juxt bar wat) x)))]

    (is (= (eval case)
           (oxcart/eval case)
           (-> (let [forms (atom {})]
                 (oxcart/eval case {:forms forms})
                 @forms)
               (ll/lift-lambdas {})
               (eclj/emit {})
               (eval)))))

  ;; Local rebinding case
  ;; --------------------
  (let [case '(let [x 1]
                (letfn [(bar [z] (+ x z))]
                  (letfn [(bar [z] (* (wat z) 3))
                          (wat [z] (* 3 z))]
                    ((juxt bar wat) x))))]

    (is (= (eval case)
           (oxcart/eval case)
           (-> (let [forms (atom {})]
                 (oxcart/eval case {:forms forms})
                 @forms)
               (ll/lift-lambdas {})
               (eclj/emit {})
               (eval)))))

  ;; Closure of closures case
  ;; ------------------------
  (let [case '(let [x 1]
                (letfn [(bar [z] (+ x z))
                        (baz [z] (dec (bar z)))
                        (wat [z] (* 3 z (baz z)))]
                  ((juxt bar baz wat) x)))]

    (is (= (eval case)
           (oxcart/eval case)
           (-> (let [forms (atom {})]
                 (oxcart/eval case {:forms forms})
                 @forms)
               (ll/lift-lambdas {})
               (eclj/emit {})
               (eval))))))

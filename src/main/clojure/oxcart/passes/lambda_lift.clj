;;   Copyright (c) Reid McKenzie, Rich Hickey & contributors. The use
;;   and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0
;;   (http://opensource.org/licenses/eclipse-1.0.php) which can be
;;   found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be
;;   bound by the terms of this license.  You must not remove this
;;   notice, or any other, from this software.

(ns oxcart.passes.lambda-lift
  {:doc "Implements lambda lifting designed to be run before symbol
        def analysis."
   :added "0.0.3"
   :author "Reid McKenzie"}
  (:require [oxcart.util :as util]
            [oxcart.passes :refer [record-pass clobber-passes]]
            [oxcart.pattern :as pattern]
            [clojure.tools.analyzer.ast :as ast]
            [clojure.tools.analyzer.passes.jvm.emit-form :refer [emit-form]]
            [clojure.tools.analyzer.passes.collect
             :refer [collect-closed-overs]]
            [clojure.set :refer [union]]))


(defn fn*->cannonical-fn*
  [[_fn* & more]]
  (let [[_sym more] (util/take-when symbol? more)]
    (if (vector? (first more))
      `(fn* (~@more))
      `(fn* ~@more))))


(defn rewrite-fn*
  [form locals]
  (let [bodies (rest form)]
    `(fn* ~@(for [[params & body] bodies]
              `([~@locals ~@params]
                  ~@body)))))


(defn rewrite-locals
  [mapping ast]
  (if (pattern/local? ast)
    (get mapping (:name ast) ast)
    ast))


(defn merge-deps
  [mapping]
  (->> (for [[k v] mapping]
         [k (->> v
                 (map mapping)
                 (keep identity)
                 (reduce union v))])
       (into {})))


(defn rewrite-closed-overs
  [mapping binding]
  {:pre [(map? mapping)
         (pattern/binding? binding)]}
  (assert (pattern/binding? binding))
  (let [name         (pattern/binding->symbol binding)
        closed-overs (get mapping name)]
    (assoc-in binding [:init :closed-overs]
              (->> (for [[l le] (:locals (:env binding))
                         :when (contains? closed-overs
                                          (:name le))]
                     [l le])
                   (into {})))))


(defn lift-fns
  [defs-atom ast]
  (cond (and (pattern/fn? ast)
             (not (pattern/top-level? ast)))
        ;; lift to a new def, creating a binding and inserting a
        ;; partial expression in its place

        (let [used-locals  (map :form (vals (:closed-overs ast)))
              sym          (pattern/fn->name ast)
              def-form     `(def ~(symbol sym)
                              ~(-> (emit-form ast)
                                   fn*->cannonical-fn*
                                   (rewrite-fn* used-locals)))
              partial-form `(partial ~(symbol sym) ~@used-locals)
              def-ast      (util/ast def-form     (:env ast))
              partial-ast  (util/ast partial-form (:env ast))]

          ;; create the new def
          (swap! defs-atom conj def-ast)

          ;; and yield the partial form
          partial-ast)


        (pattern/letfn? ast)
        ;; letfns are weird and hard because they need to be rewritten
        ;; into lets and a declare form. The fns are already correctly
        ;; promoted to defs and replaced with partials, so if the
        ;; naming of lifted letfn fns can be resolved it will be
        ;; correct to simply rewrite letfn forms to lets with partials
        ;; as the partials are ording insensitive.
        ;;
        ;; 1. Create a mapping from letfn bound fn names to the
        ;;    internal (de-aliased) fn names.
        ;;
        ;; 2. Create a (declare <fns>) in the defs atom.
        ;;
        ;; 3. Compute the closure over the closed-overs, that is
        ;;    update the closed-overs of each function in the letfn
        ;;    to be the util/fixed point union of the closed-overs.
        ;;
        ;; 4. Having computed the updated closed-over sets, rewrite
        ;;    all the functions to eliminate the closed over
        ;;    arguments. Note that at each step of this rewrite all of
        ;;    the function bodies must be traversed
        ;;
        ;; 5. For each fn in the letfn rewrite the letfn bodies using
        ;;    the mapping from 1.
        ;;
        ;; 6. Emit lifted defs as would be normal for lambda lifting.
        ;;
        ;; 7. Replace the letfn* with a simple let* that binds the
        ;;    partials appropriately so that I don't have to rewrite
        ;;    all the locals of the body expression.

        (let [bindings             (:bindings ast)
              locals->vars         (atom {})
              locals->closed-overs (atom {})
              env                  (:env ast)]

          ;; create the forward defs
          (doseq [b bindings]
            (let [sym                      (-> b
                                               pattern/binding->value
                                               pattern/fn->name
                                               symbol)
                  {:keys [statements ret]} (util/ast `(do (def ~sym)
                                                          ~sym)
                                                     env)
                  def-ast                  (first statements)]

              ;; save the closed-overs
              (swap! locals->closed-overs
                     assoc (pattern/binding->symbol b)
                           (->> b
                                pattern/binding->value
                                :closed-overs
                                vals
                                (map :name)
                                set))

              ;; save the def ast
              (swap! defs-atom conj def-ast)

              ;; save the var ast
              (swap! locals->vars assoc (pattern/binding->symbol b) ret)))


          ;; compute the util/fixed point over the local bindings
          (swap! locals->closed-overs
                 (fn [locals->closed-overs]
                   (util/fix merge-deps
                        locals->closed-overs)))

          (let [;; rewrite the bindings to share closed-over information
                bindings' (mapv (partial rewrite-closed-overs
                                         @locals->closed-overs)
                                bindings)

                fns       (zipmap (map pattern/binding->symbol bindings')
                                  (map pattern/binding->value  bindings'))

                ;; compute the mapping from symbols to partial applications
                fns'      (util/map-vals fns
                                    (partial lift-fns (atom [])))]

            (-> ast
                (assoc :bindings (->> bindings'
                                      (mapv #(ast/prewalk %
                                                          (partial rewrite-locals
                                                                   fns')))
                                      (mapv #(ast/prewalk %
                                                          (partial rewrite-locals
                                                                   @locals->vars)))))

                ;; force to a let form
                (assoc :op :let))))


        true
        ;; This operation is a noop. Note that we do not need to
        ;; special case the :invoke operation as it takes its function
        ;; term as a chile, which will be handled by the local case.
        ast))


(defn push-down-top-level
  "Helper pass which serves to push down the :top-level annotation to
  the direct children of top level forms. This allows the lambda
  lifting operation to escape lifting (def* (fn*)) forms recursively
  forever."
  [ast]
  (if (pattern/top-level? ast)
    (ast/update-children
     ast
     (fn [node]
       (assoc node :top-level true)))
    ast))


(defn lift-lambdas-in-module
  "λ Module → Module

  Helper for lift-lambdas that applies the lambda lifting transform
  over a single module and handles the involved updates to the top
  level forms list."
  [{:keys [forms] :as module}]
  (->> (for [form forms]
         (let [defs    (atom [])
               new-ast (-> form
                           (collect-closed-overs {:what #{:closed-overs} :where #{:fn}})
                           (push-down-top-level)
                           (ast/prewalk (partial lift-fns defs)))]
           (conj @defs new-ast)))
       (reduce concat)
       (assoc module :forms)))


(defn lift-lambdas
  "λ Whole-AST → options → AST

  Walks the input AST, doing lambda lifting of closed over functions
  to named but private top level definitons.

  Closed over lambda function definitons are rewritten to eliminate
  closures by making environment values explicit parameters and are
  then promoted to top level definition status.

  Note that this pass does not handle closed over def promotion, only
  lambda function lifting.

  options:
    At present there are no options accepted by this pass."
  [{:keys [modules] :as ast} options]
  (-> (->> (for [module modules]
             (binding [*ns* module]
               [module (util/fix lift-lambdas-in-module (get ast module))]))
           (into {})
           (merge ast))
      (clobber-passes)
      (record-pass lift-lambdas)))

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
            [oxcart.pattern :as pattern]
            [clojure.tools.analyzer.ast :as ast]
            [clojure.tools.analyzer.passes.collect 
             :refer [collect-closed-overs]]
            [clojure.set :refer [union]]))


(defn fn->name-or-gensym
  "λ AST → (Option Symbol)

  If the argument AST is a fn, attempts to return the internal name of
  the fn defaulting to a freshly generated symbol."
  [ast]
  (when (pattern/fn? ast)
    (-> ast :internal-name)))


(defn take-when
  "When pred is true of the head of seq, return [head tail]. Otherwise
  [nil seq]. Used as a helper for parsing optinal typed elements out
  of sequences. Say docstrings out of argument seqs."
  [pred seq]
  (if (pred (first seq))
    ((juxt first rest) seq)
    [nil seq]))


(defn fn*->cannonical-fn*
  [[_fn* & more]]
  (let [[_sym more] (take-when symbol? more)]
    (if (vector? (first more))
      `(fn* (~@more))
      `(fn* ~@more))))


(defn rewrite-fn*
  [form locals]
  (let [bodies (rest form)]
    `(fn* ~@(for [[params & body] bodies]
              `([~@locals ~@params]
                  ~@body)))))


(defn lift-fns
  [defs-atom ast]
  (cond (and (pattern/fn? ast)
             (not (pattern/top-level? ast)))
        ;; lift to a new def, creating a binding and inserting a
        ;; partial expression in its place

        (let [used-locals  (map :form (vals (:closed-overs ast)))
              sym          (fn->name-or-gensym ast)
              def-form     `(def ~(symbol sym)
                                 ~(-> (:form ast)
                                      (fn*->cannonical-fn*)
                                      (rewrite-fn* used-locals)))
              partial-form `(partial ~(symbol sym) ~@used-locals)
              def-ast      (util/ast def-form     (:env ast))
              partial-ast  (util/ast partial-form (:env ast))]

          ;; create the new def
          (swap! defs-atom conj def-ast)

          ;; and yield the partial form
          partial-ast)


        ;; FIXME:
        ;;    support letfn

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
  (->> (for [module modules]
         (binding [*ns* module]
           [module (lift-lambdas-in-module (get ast module))]))
       (into {})
       (merge ast)))

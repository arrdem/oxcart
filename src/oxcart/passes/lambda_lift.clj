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
            [oxcart.bindings :as b]
            [oxcart.pattern :as pattern]
            [clojure.tools.analyzer.ast :as ast]
            [clojure.set :refer [union]]))



;; Using this binding structure we can now push symbols onto a virtual
;; stack, check to see how far back up the virtual stack a symbol is
;; bound and life should be good as a result.
;;
;; Lambda lifting is now a recursive traverse building a bindings
;; tree, while finding pattern/fn? nodes which we have to rewrite to
;; eliminated closed over variables as implicit aruments and add them
;; to the parameters list.


(defn collect-used-locals
  "λ Bindings → level → AST → (Set Symbol)

  Walks the argument AST recursively, building and retaining the
  appropriate bindings stack. Returns the set of used locals which are
  bound above the `level` value."
  [bindings level ast]
  (->> (for [child (ast/children ast)]
         (collect-used-locals bindings level child))

       ;; Union the nested sets
       (reduce union)

       ;; Union with the local sets
       (union
        (->> (ast/children ast)
             (filter pattern/local?)
             (filter #(b/get-level
                       bindings
                       (pattern/local->symbol %1)))
             (map :form)
             (reduce conj #{})))))


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
  (println "Rewriting " form locals)
  (let [bodies (rest form)]
    `(fn* ~@(for [[params & body] bodies]
              `([~@locals ~@params]
                  ~@body)))))


(defn rewrite-helper
  [bindings-atom defs-atom ast]
  (cond (pattern/fn? ast)
        ;; lift to a new def, creating a binding and inserting a
        ;; partial expression in its place

        (let [bindings     @bindings-atom
              depth        (b/depth bindings)
              used-locals  (collect-used-locals bindings depth ast)
              sym          (fn->name-or-gensym ast)
              def-form     `(def ~sym ~(-> (:form ast)
                                           (fn*->cannonical-fn*)
                                           (rewrite-fn* used-locals)))
              partial-form `(partial ~sym ~@used-locals)
              def-ast      (util/ast def-form)
              partial-ast  (util/ast partial-form)]

          (println def-form)

          ;; create the new def
          (swap! defs-atom conj def-ast)

          ;; save the partial for later use
          (swap! bindings-atom b/bind sym partial-ast)

          ;; and yield the partial form
          partial-ast)


        (pattern/local? ast)
        ;; determine whether the value is a lifted lambda, (meaning
        ;; that it's binding value is not nil)
        (let [bindings @bindings-atom
              sym      (pattern/local->symbol ast)
              value    (b/get-value bindings sym)]
          (if (nil? value)
            ast
            value))


        (pattern/binding? ast)
        ;; As bindings are applied sequentially (with one exception)
        ;; rather than special casing the management of binding frames
        ;; just accumulate bindings sequentially by terms.
        (do (swap! bindings-atom b/bind
                   (pattern/binding->symbol ast)
                   (pattern/binding->value ast))
            ast)


        ;; FIXME:
        ;;    support letfn

        true
        ;; This operation is a noop. Note that we do not need to
        ;; special case the :invoke operation as it takes its function
        ;; term as a chile, which will be handled by the local case.
        ast))


(defn on-enter
  "Helper function for lift-lambdas that implements the prewalk and
  update step of the AST walk operation."
  [bindings-atom defs-atom ast]
  ;; FIXME:
  ;;   support letfn
  (when (or (pattern/let? ast)
            (pattern/fn-method? ast))
    (swap! bindings-atom b/push-bindings))
  (rewrite-helper bindings-atom defs-atom ast))


(defn on-exit
  [bindings-atom defs-atom ast]
  ;; FIXME:
  ;;    support letfn
  (when (or (pattern/let? ast)
            (pattern/fn-method? ast))
    (swap! bindings-atom b/pop-bindings))
  ast)


(defn lift-lambdas-in-module
  "λ Module → Module

  Helper for lift-lambdas that applies the lambda lifting transform
  over a single module and handles the involved updates to the top
  level forms list."
  [{:keys [forms] :as module}]
  (->> (for [form forms]
         (let [bindings              (atom {})
               defs                  (atom [])
               update-bindings       (partial on-enter bindings defs)
               lift-and-pop-bindings (partial on-exit bindings defs)
               new-ast               (ast/walk form
                                               update-bindings
                                               lift-and-pop-bindings)]
           (conj @defs new-ast)))
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
           (println "Lifting lambdas in module" module)
           [module (lift-lambdas-in-module (get ast module))]))
       (into {})
       (merge ast)))

;;   Copyright (c) Reid McKenzie, Rich Hickey & contributors. The use
;;   and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0
;;   (http://opensource.org/licenses/eclipse-1.0.php) which can be
;;   found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be
;;   bound by the terms of this license.  You must not remove this
;;   notice, or any other, from this software.

(ns oxcart.passes.fn-reduction
  {:doc "This namespace implements a multi-arity to single arity
        reduction over functions intended for use in enabling static
        method/function emission and other transformations."
   :author "Reid McKenzie"
   :added "0.0.11"}
  (:require [oxcart.util :refer :all]
            [oxcart.passes :refer :all]
            [oxcart.passes.lambda-lift :refer [lift-lambdas]]
            [oxcart.pattern :as pattern]
            [clojure.tools.analyzer.ast :refer [postwalk]]
            [clojure.tools.analyzer.passes.jvm.emit-form :refer [emit-form]]))


(defn munge-symbol
  [sym arity variadic]
  (symbol
   (str sym "$arity$"
        (if variadic
          (- arity 2)
          arity)
        (when variadic
          "+"))))


(defn method-variadic?
  [method]
  (if (list? method)
    (some (partial = '&) (first method))
    (if (map? method)
      (or (:variadic? method)
          (some :variadic?
                (:params method))))))


(defn munge-fn-name
  [wrapping-def
   {:keys [params] :as method}]
  (munge-symbol
   (pattern/def->symbol wrapping-def)
   (count params)
   (method-variadic? method)))


(defn fn-is-multiple-arity?
  "λ AST → Boolean

  Returns true if and only of the argument AST is both a function node
  and a function of more than one arity."
  [{:keys [op methods]}]
  (and (= op :fn)
       (> (count methods) 1)))


(defn promote-method
  "λ AST → List → Env → AST

  raw-method   is a raw list form
  env          is the containing environment
  wrapping-def is the def this is being lifted from

  Builds and yields a new AST representing a fn with a munged name
  having the provided method body for its only arity."
  [wrapping-def raw-method env]
  (let [new-name (with-meta
                   (munge-symbol
                    (pattern/def->symbol wrapping-def)
                    (count (first raw-method))
                    (some (partial = '&)
                          (first raw-method)))
                   {:single true :static true})
        new-def `(def ~new-name (fn* ~raw-method))]
    (eval-in new-def env)
    (ast new-def env)))


(defn write-body
  [wrapping-def
   raw-method
   {:keys [env] :as promoted-method}]
  (let [[params & forms] raw-method
        new-params       (for [p params
                               :when (not (= p '&))]
                           (gensym "OX__P"))

        variadic?          (some (partial = '&) params)

        new-form         (if variadic?
                           (list (vec (concat (butlast new-params) ['&] [(last new-params)]))
                                 (concat (list 'apply)
                                         (list (pattern/def->symbol promoted-method))
                                         new-params))
                           (list (vec new-params)
                                 (concat (list (pattern/def->symbol promoted-method))
                                         new-params)))]

    new-form))


(defn rewrite-fn
  [{:keys [methods env] :as fn-ast}
   wrapping-def
   prefix-forms-atom
   munged-fns-atom]
  (if (not (or (pattern/fn? fn-ast)
               (fn-is-multiple-arity? fn-ast)))
    ast
    (let [[def-ast methods env]
          (if-let [fn-name (-> fn-ast :local :name)]
            ;; everything goes to shit and we need to do extra bindings work
            (let [new-name        (gensym "OX__L")

                  def-ast         (ast `(def ~new-name
                                          ~(pattern/def->symbol wrapping-def))
                                       env)

                  fn-ast          (postwalk fn-ast
                                            (fn [{:keys [op name] :as node}]
                                              (if (and (= op :local)
                                                       (= name fn-name))
                                                (ast new-name env)
                                                node)))

                  [_fn & methods] (emit-form fn-ast)
                  [name methods]  (take-when symbol? methods)]
              [def-ast methods (:env def-ast)])

            ;; this is the easy case because we don't have to rewrite
            ;; the fn body before we get the methods.
            (let [[_fn & methods] (emit-form fn-ast)
                  [name methods]  (take-when symbol? methods)]
              [nil methods env]))

          promotes (mapv (fn [m]
                           (promote-method wrapping-def m env))
                         methods)

          new-fn   `(fn* ~@(map
                            (partial write-body wrapping-def)
                            methods
                            promotes))]

      (swap! munged-fns-atom
             assoc (pattern/def->var wrapping-def)
                   (->> (map (fn [m p]
                               [(if (:variadic? (:init p))
                                  :variadic
                                  (count (first (:arglists p))))
                                (pattern/def->var p)])
                             methods
                             promotes)
                        (into {})))

      (swap! munged-fns-atom
             merge (->> (for [p promotes]
                          (let [v (pattern/def->var p)]
                            [v {(if (:variadic? (:init p))
                                  :variadic
                                  (count (first (:arglists p))))
                                v}]))
                        (into {})))

      (reset! prefix-forms-atom (vec (cons def-ast promotes)))

      (ast new-fn env))))


(defn rewrite-fn-decls
  "λ AST → (atom {Var → (λ int → Var)}) → (U AST (Vec AST))

  Rewrites top level forms into one or more top level forms. If a top
  level form is the def of a fn with multiple arities, the def is
  rewritten into N defs, one for each arity of the fn. The original
  def is rewritten to be static arity based dispatch to the rewritten
  member arities.

  Because this function is a delegate of the reduce-fn-arities pass
  which requires the lambda-lifting pass, when this function is run
  the input program can contain no fns which are _not_ top level as
  the lambda lifting pass rewrites all fns to the top level.

  Thus it is correct for this pass to handle only those fns which are
  bound with defs, as by the requirement of lambda lifting this must
  be the set of all fns in the input program."
  [ast munged-fns-atom]
  (if-not (pattern/def? ast)
    ast
    (if (or (contains? @munged-fns-atom
                       (pattern/def->var ast))
            (-> ast :meta :val :single))
      ast
      (let [top-level-forms (atom [])
            new-ast         (update ast :init
                                    update-through-meta
                                    rewrite-fn
                                    ast
                                      top-level-forms
                                      munged-fns-atom)]
        (-> @top-level-forms
            vec
            (conj new-ast))))))


(defn rewrite-fn-invokes
  "λ AST → (Atom {Var → {(U Number :variadic) → Var}}) → AST

  Walks the argument AST, rewriting invoke nodes where it is possible
  to call a previously emitted single arity function rather than a
  multiple arity function by doing static arity resolution defaulting
  to the variadic implementation if present and failing back to the
  original unaltered function in all cases. Returns an updated top
  level form."
  [top-ast munged-fns]
  (let [munged-fns @munged-fns]
    (postwalk top-ast
              (fn [{:keys [op fn args env] :as node}]
                (if (and (= op :invoke)
                         (#{:var :the-var} (:op fn)))
                  (if-let [arities (munged-fns (-> fn :var))]
                    (if-let [var (get munged-fns
                                      (count args)
                                      (get munged-fns :variadic))]
                      (assoc node :fn (ast (var->sym var) env))
                      node)
                    node)
                  node)))))


(defn reduce-fn-arities
  "λ Whole-AST → Options → Whole-AST

  This pass walks the argument AST, examining function definitions and
  rewriting multiple arity functions into single arity functions when
  possible. Note that functions already of a single arity are not
  rewritten.

  In order to be arity reduced, a fn must
   1. Never be taken as a value
   2. Never be the target of an apply invocation

  Functions which are reduced become tagged with ^:single and
  ^:static metadata.

  Options
  -----------
    This pass takes no options"
  [{:keys [arity-reduction-map] :as whole-ast} options]
  (let [munged-fns (atom arity-reduction-map)]
    (-> whole-ast
        (require-pass lift-lambdas {})
        (update-forms rewrite-fn-decls   munged-fns)
        (update-forms rewrite-fn-invokes munged-fns)
        (assoc :arity-reduction-map @munged-fns)
        (record-pass reduce-fn-arities))))

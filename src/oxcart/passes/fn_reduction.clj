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
            [clojure.tools.analyzer.passes.jvm.emit-form :refer [emit-form]]))


(defn update-through-meta
  "(λ {A → B} → ((λ B → args*) → C) → args*) → {A → C}

  A wrapper around oxcart.util/update which will reach through
  {:op :with-meta} nodes as if they don't exist. This allows AST
  transforms to do updates preserving with-meta operations without
  explicitly handling the possibility of metadata."
  [{:keys [op] :as ast} f & args]
  (if (= op :with-meta)
    (apply update ast :expr f args)
    (apply f ast args)))


(defn munge-symbol
  "Stub subject to change based on what TANAL permits, the issue is
  going to be symbol vs. var type in the AST. Munging vars is gonna be
  a bitch, munging symbols could happen."
  [sym arity vardic]
  (symbol
   (str sym "$arity$" arity
        (when vardic
          "+"))))


(defn munge-fn-name
  [wrapping-def
   {:keys [params] :as method}]
  (munge-symbol
   (pattern/def->symbol wrapping-def)
   (count params)
   (some :vardic params)))


(defn munge-callsite
  "λ Invoke-AST → Invoke-AST

  This function rewrites invoke AST nodes, returning a new invoke node
  which targets an arity munged function."
  [ast]
  )


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
  (let [new-name (munge-symbol
                  (pattern/def->symbol wrapping-def)
                  (count (first raw-method))
                  (some (partial = '&) (first raw-method)))]
    (ast `(def ~new-name
            (fn* ~raw-method))
         env)))


(defn write-body
  [wrapping-def
   raw-method
   {:keys [env] :as promoted-method}]
  (let [[params & forms] raw-method
        new-params       (for [p params
                               :when (not (= p '&))]
                           (gensym "OX__P"))

       vardic?          (some (partial = '&) params)

        new-form         (if vardic?
                           (list (vec (concat (butlast new-params) ['&] [(last new-params)]))
                                 (concat (list 'apply)
                                         (list (pattern/def->symbol promoted-method))
                                         new-params))
                           (list (vec new-params)
                                 (concat (list (pattern/def->symbol promoted-method))
                                         new-params)))]

    new-form))


(defn rewrite-fn
  [{:keys [methods env] :as fn-ast} wrapping-def prefix-forms-atom]
  (if (not (or (pattern/fn? fn-ast)
               (fn-is-multiple-arity? fn-ast)))
    ast
    (let [[_fn & methods] (emit-form fn-ast)
          [name methods]  (take-when symbol? methods)
          name            (pattern/def->symbol fn-ast)

          fdecl    (when name
                     (ast `(def ~name ~(pattern/def->symbol wrapping-def))
                          env))

          promotes (mapv (fn [m]
                           (promote-method wrapping-def m env))
                         methods)

          new-fn   (if name
                     `(fn* ~name
                           ~@(map
                              (partial write-body wrapping-def)
                              methods
                              promotes))
                     `(fn* ~@(map
                              (partial write-body wrapping-def)
                              methods
                              promotes)))]

      (reset! prefix-forms-atom
              (if name
                (cons fdecl promotes)
                promotes))

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
    (let [top-level-forms (atom [])
          new-fn-ast      (rewrite-fn (get ast :init)
                                      ast top-level-forms)]

      (-> @top-level-forms
          vec
          (conj (assoc ast :init new-fn-ast))))))


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
  [whole-ast options]
  (let [munged-fns (atom #{})]
    (-> whole-ast
        (require-pass lift-lambdas)
        (update-forms rewrite-fn-decls   munged-fns)
        (update-forms rewrite-fn-invokes munged-fns)
        ;; FIXME
        ;;   Is this something I need to do? Are there cases in which
        ;;   this analysis is itself desructive? all existing fns,
        ;;   defs and vars should be preserved by this.
        (clobber-passes)
        (record-pass reduce-fn-arities))))

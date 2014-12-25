(ns oxcart.passes.discard
  {:doc "This namespace implements a number of form discarding passes
        over the Oxcart module system."
   :author "Reid McKenzie"
   :added  "0.0.9"}
  (:require [oxcart.util    :as util]
            [oxcart.passes  :as passes]
            [oxcart.pattern :as pattern]))

(defn def->macro?
  "λ AST → (U True False)

  Predicate which indicates whether the argument AST is a macro. Only
  returns true if the AST is both a def form and the def'd var has
  the :meta tag."
  [form]
  {:pre [(map? form)]}
  (-> form
      (pattern/def->var)
      (meta)
      :macro
      (or false)))

(defn discard-forms
  "Helper function to discard-macros, composes a number of predicates
  in order to determine what forms should be discarded from the
  argument whole-ast."
  [whole-ast & preds]
  {:pre [(map? whole-ast)
         (:modules whole-ast)
         (every? fn? preds)]}
  (passes/update-forms
   (fn [form]
     (when (->> (map #(%1 form) preds)
                (some identity))
       form))))

(defn discard-macros
  "λ Whole-AST → Options → Whole-AST

  This pass walks the input whole-ast discarding definitions of vars
  marked as macros.

  Options
  -----------
    This pass takes no options."
  [whole-ast options]
  (-> whole-ast
      (passes/update-modules discard-forms def->macro?)
      (passes/clobber-passes)
      (passes/record-pass discard-macros)))

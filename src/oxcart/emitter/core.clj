;;   Copyright (c) Reid McKenzie, Rich Hickey & contributors. The use
;;   and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0
;;   (http://opensource.org/licenses/eclipse-1.0.php) which can be
;;   found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be
;;   bound by the terms of this license.  You must not remove this
;;   notice, or any other, from this software.

(ns oxcart.emitter.core
  {:doc "Oxcart will wind up with more than one emitter, but we'll
        start with this for now."
   :author "Reid McKenzie"
   :added "0.0.12"}
  (:require [oxcart
             [util :refer [var-name var->ns var->name]]
             [passes :as passes :refer [require-pass whole-ast->forms]]
             [pattern :as pattern]]
            [oxcart.passes.tree-shake
             :refer [analyze-var-uses tree-shake analyze-var-dependencies]]
            [oxcart.passes.fn-reduction 
             :refer [reduce-fn-arities]]
            [oxcart.passes.defs
             :refer [locate-var-as-value]]
            [clojure.tools.emitter.jvm.transform
             :refer [-compile]]))

(defn var->class [x]
  {:pre  [(var? x)]
   :post [(string? %)]}
  (str (var->ns x) "." (var->name x)))

(defn write-class
  [classname bytecode])

(defn dispatch-fn
  []
  )

(defmulti -emit
  "(λ AST) → OpSeq

  Recursively translates the given AST to TEJVM bytecode, returning a
  sequence of opcodes.

  OpSeq is (Seq (Σ Op Opseq))"
  dispatch-fn)

(defn emit-class
  "(λ Def-AST) → Class-AST

  Transforms a def to a class, returning a TEJVM AST describing the
  resulting class."
  [ast]
  {:pre [(pattern/def? ast)]}
  )

(defn emit
  "(λ Whole-AST → Options) → nil

  Emits a whole program with reference to a single emtry point as
  named by the argument var, writing class files for side-effects.

  Options
  -----------
  `:entry'
    a Var, being the var with reference to which the entire program
    will be emitted."
  [whole-program {:keys [entry] :as options}]
  {:pre  [(var? entry)]
   :post [(nil? %)]}
  (let [whole-program (-> whole-program
                          (require-pass tree-shake          options)
                          (require-pass reduce-fn-arities   options)
                          (require-pass tree-shake          options)
                          (require-pass analyze-var-uses    options)
                          (require-pass locate-var-as-value options))
        reach         (-> whole-program (get :var-reached) (get entry))
        reach-defs    (->> whole-program
                           whole-ast->forms
                           (keep #(and (pattern/def? %)
                                       (reach (pattern/def->var %)))))]
    ;; compile & write the set of used classes
    (doseq [ast reach-defs]
      (let [{:keys [class-name] :as class-ast} (-> ast emit-class)
            class-bc                           (-> class-ast -compile)]
        (write-class class-name class-bc)))

    (let [class-name (var->ns entry)
          class-ast  {}
          class-bc   (-> class-ast -compile)]
      (write-class class-name class-bc)))
  nil)

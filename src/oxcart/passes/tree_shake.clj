;;   Copyright (c) Reid McKenzie, Rich Hickey & contributors. The use
;;   and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0
;;   (http://opensource.org/licenses/eclipse-1.0.php) which can be
;;   found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be
;;   bound by the terms of this license.  You must not remove this
;;   notice, or any other, from this software.

(ns oxcart.passes.tree-shake
  {:doc "Implements a tree shaking pass intended to be run after def analysis."
   :author "Reid McKenzie"
   :added "0.0.5"}
  (:require [oxcart.util :as util]
            [oxcart.pattern :as pattern]
            [oxcart.passes :refer [require-pass record-pass clobber-passes]]
            [oxcart.passes.defs :as defs]
            [clojure.set :as set]
            [clojure.tools.analyzer.ast :as ast]
            [taoensso.timbre :refer [info warn]]))


(defn reach-set
  "λ AST → #{Var}

  Computes the var reach set of an AST"
  [source]
  (->> source
       ast/nodes
       (keep :var)
       (into #{})))


(defn -step-reach-set
  "λ {T → #{T}} → {T → #{T}}

  Implements a single update step of the context insensitive closure
  of closures dataflow operation."
  [mapping]
  (->> (for [[var deps] mapping]
         [var (->> deps
                   (mapv mapping)
                   (reduce set/union deps))])
       (into {})))


(defn global-reach-set
  "λ {T → #{T}} → {T → #{T}}

  Computes the reach set for an entire program"
  [symbol-dep-tree]
  (util/fix -step-reach-set symbol-dep-tree))


(defn trim-with-emit-set
  "λ Whole-AST → #{Var} → Whole-AST

  Rewrites a Whole-AST to eliminate definitions of vars which are not
  used. Note that this operation _preserves_ non def top level forms
  rather than discarding them."
  [{:keys [modules] :as whole-program-ast} reach-set]
  {:pre [(every? (partial contains? whole-program-ast) modules)
         (every? #(get-in whole-program-ast [%1 :forms]) modules)
         (every? var? reach-set)]}
  (let [new-ast (atom {:modules modules})]

    (doseq [m modules]
      (assert (:forms (get whole-program-ast m)))

      (doseq [ast (:forms (get whole-program-ast m))]
        (if (pattern/def? ast)
          (if (contains? reach-set (:var ast))
            (swap! new-ast update-in [m :forms] conj ast)

            (info "Discarding unused def form,"
                  (util/format-line-info ast)))

          (swap! new-ast update-in [m :forms] conj ast)))

      (swap! new-ast update-in [m :forms] vec))

    @new-ast))


(defn analyze-var-dependencies
  "λ Whole-AST → options → Whole-AST

  Implements an analysis pass which creates the following annotations
  in the supplied whole program AST.

    :dependency-map {Var → #{Var}}, represents the vars directly
    depended on by any given var for which source information exists
    in the whole program AST.

    :reach-map {Var → #{Var}}, represents the reach sets of every var
    in the program AST for which source information exists.

  options:
    This function takes no options."
  [{:keys [modules] :as whole-program-ast} options]
  {:pre [(every? symbol? modules)
         (every? (partial contains? whole-program-ast) modules)]}
  (let [dep-maps  (->>  (for [m     modules
                              form  (:forms (get whole-program-ast m))
                              :when (pattern/def? form)]
                          [(:var form) (reach-set form)])
                        (into {}))
        reach-map (global-reach-set dep-maps)]
    (-> whole-program-ast
        (assoc :dependency-map dep-maps
               :reach-map      reach-map)
        (record-pass analyze-var-dependencies))))


(defn tree-shake
  "λ Whole-AST → options → Whole-AST

  Implements def elimination on the basis of prior reachability
  analysis. Uses the reachability analysis as proof that a given def
  is used or not used and drops the ASTs constituting unused defs from
  the program.

  options:
    :entry is a symbol, presumably a namespace qualified -main, which
    is the entry point of the prorgam. It is with respect to this
    function that all other defs in all loaded namespaces will be
    considered for elimination."
  [{:keys [modules] :as ast} {:keys [entry] :as options}]
  {:pre [(every? symbol? modules)
         (symbol? entry)
         (every? (partial contains? ast) modules)]}
  (let [ast      (require-pass analyze-var-dependencies options)
        emit-set (get (:reach-map ast) (resolve entry))]
    (-> ast
        (trim-with-emit-set emit-set)
        (clobber-passes))))


(defn analyze-var-uses
  "λ Whole-AST → Options → Whole-AST

  Walks the argument AST, accumulating call site information about
  vars. Returns an updated Whole-AST which has the `:var-uses' key
  set, being a map {Var → #{Var}} where each var maps to the set of
  vars it is used by.

  Options
  -----------
    This pass takes no options"
  [whole-ast options]
  (let [{:keys [dependency-map] :as whole-ast}
        (-> whole-ast
            (require-pass analyze-var-dependencies {}))
        acc  (atom {})
        sconj #(conj (or %1 #{}) %2)]

    ;; FIXME
    ;;   There's probably a way to write this comprehension which is
    ;;   more efficient, but this works for now.
    (doseq [[var deps] dependency-map
            dep  deps]
      (swap! acc update-in [dep] sconj var))

    (-> whole-ast
        (assoc :var-uses @acc)
        (record-pass analyze-var-uses))))

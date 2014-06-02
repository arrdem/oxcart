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
            [oxcart.passes.defs :as defs]
            [clojure.set :as set]
            [clojure.tools.analyzer.ast :as ast]
            [taoensso.timbre :refer [info warn]]))


(defn reach-set
  "λ AST → #{Symbol}

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
      (debug "Pondering module:" m)

      (assert (:forms (get whole-program-ast m)))

      (doseq [ast (:forms (get whole-program-ast m))]
        (debug "Pondering line:" (util/format-line-info ast) (:op ast))

        (if (pattern/def? ast)
          (if (contains? reach-set (:var ast))
            (swap! new-ast update-in [m :forms] conj ast)

            (info "Discarding unused def form,"
                  (util/format-line-info ast)))

          (warn "Discarding non-def top level form,"
                (util/format-line-info ast))))

      (swap! new-ast update-in [m :forms] vec))

    @new-ast))


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
  (let [;; Compute the {Var → #{Var}} form of the whole program
        symbol-sets (->>  (for [m     modules
                                form  (:forms (get ast m))
                                :when (pattern/def? form)]
                            [(:var form) (reach-set form)])
                          (into {})
                          global-reach-set)

        ;; The emit set is now trivially
        emit-set (get symbol-sets (resolve entry))]
    (trim-with-emit-set ast emit-set)))

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
            [oxcart.passes.defs :as defs]
            [clojure.set :as set]
            [clojure.tools.analyzer.ast :as ast]))


(defn reach-set
  "λ AST → #{Symbol}

  Computes the var reach set of an AST"
  [source]
  (->> source
       ast/nodes
       (keep :var)
       (into #{})))


(defn -step-reach-set
  [ast]
  (util/map-vals
   (fn [v]
     (->> v
          (map (partial get ast))
          (reduce set/union v)))))


(defn global-reach-set
  "λ {Symbol → AST} → {Symbol → #{Symbol}}

  Computes the reach set for an entire program"
  [whole-program-ast]
  (util/fix -step-reach-set
            (-> whole-program-ast
                (util/map-vals reach-set))))


(defn tree-shake-module
  [module options]
  )

(defn tree-shake
  "λ Whole-AST → options → Whole-AST

  Implements def elimination on the basis of prior reachability
  analysis. Uses the reachability analysis as proof that a given def
  is used or not used and drops the ASTs constituting unused defs from
  the program.

  options:

    :entry is a symbol, presumably a namespace qualified -main var,
    which is the entry point of the prorgam. It is with respect to this
    function that all other defs will be considered for elimination."
  [ast {:keys [entry] :as options}]
  (-> ast
      (defs/locate-defs options)
      (util/map-vals (partial tree-shake-module options))))

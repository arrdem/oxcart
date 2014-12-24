(ns oxcart.emitter.clj
  {:doc "Implements an ast to source return emitter designed to be run
         after other transformations for verification of output."
   :added "0.0.4"
   :author "Reid McKenzie"}
  (:require [clojure.tools.analyzer.passes.jvm.emit-form :as emit]
            [oxcart.passes :as passes]))

(defn emit
  "(λ Whole-AST → options) → (Seq Form)

  Uses tools.analyzer.jvm to emit Clojure code from a whole program
  AST. Inteded for use in debugging and verifying output, as well as
  for rebuilding and re-analyzing programs in between syntax level
  passes.

  options:
    At present there are no options accepted by this pass."
  [{:keys [modules] :as whole-ast} options]
  (->> whole-ast
       passes/whole-ast->forms
       (map #(emit/emit-form %1 #{:qualified-vars}))
       (cons 'do)))

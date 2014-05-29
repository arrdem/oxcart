;;   Copyright (c) Reid McKenzie, Rich Hickey & contributors. The use
;;   and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0
;;   (http://opensource.org/licenses/eclipse-1.0.php) which can be
;;   found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be
;;   bound by the terms of this license.  You must not remove this
;;   notice, or any other, from this software.

(ns oxcart.passes.emit-clj
  {:doc "Implements an ast to source return emitter designed to be run
         after other transformations for verification of output."
   :added "0.0.4"
   :author "Reid McKenzie"}
  (:require [clojure.tools.analyzer.passes.jvm.emit-form :as emit]
            [oxcart.passes.util :as util]))


(defn emit-clojure
  "λ Whole-AST → options → (Seq Form)

  Uses tools.analyzer.jvm to emit Clojure code from a whole program
  AST. Inteded for use in debugging and verifying output, as well as
  for rebuilding and re-analyzing programs in between syntax level
  passes.

  options:
    At present there are no options accepted by this pass."
  [{:keys [modules] :as whole-ast} options]
  (->> whole-ast
       util/whole-ast->forms
       (map emit/emit-form)
       (cons 'do)))

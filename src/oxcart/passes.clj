;;   Copyright (c) Reid McKenzie, Rich Hickey & contributors. The use
;;   and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0
;;   (http://opensource.org/licenses/eclipse-1.0.php) which can be
;;   found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be
;;   bound by the terms of this license.  You must not remove this
;;   notice, or any other, from this software.

(ns oxcart.passes
  {:doc "Implements a naive pass manager and dependency system."
   :author "Reid McKenzie"
   :added  "0.0.6"})


;; Whole-ASTs are maps of this structure:
;;   {:modules #{symbol}
;;    :passes  (Option #{Var})
;;    ∀ m ∈ Modules m → Module
;;   }
;;
;; Where a Module is at least
;;   {:forms (Vec AST)}
;;
;; The rationale for this structure is that loading is done in the
;; refernce JVM Clojure implementation on a per-module basis where
;; modules are defined to be single files but compilation and
;; evaluation occur sequentially over forms in order of
;; occurrance. Grouping forms from the same namespace together into a
;; module is pretty obvious choice, as is storing forms in file/load
;; order.


;; Passes are then functions from Whole-ASTs to Whole-ASTs. For user
;; "friendliness" each pass shall also take an options argument which
;; may change the behavior of the pass by enabling or disabling a
;; given transformation or logging output.
;;
;; As it is considered good practice for passes to be composed to
;; achieve some result rather than being monolithic, passes are
;; expected to depend on other passes, especially when a given
;; transformation requires previous enabling analysis. To assist this
;; pattern, when a non-transforming pass completes, it is expected to
;; conj it's identifier into the :passes set. Other passes may then
;; elect not to re-run analyses on which they depend if they have
;; already been run.


(defn record-pass
  [whole-ast pass]
  (update-in whole-ast [:passes] conj pass))

(defn require-pass
  [whole-ast pass options]
  (if (contains? (:passes whole-ast) pass)
    whole-ast
    (pass whole-ast options)))

(defn do-passes
  [ast options & passes]
  (reduce #(require-pass %1 %2 options)
          ast passes))


;; To enable this pattern however, transforming passes are required to
;; clobber the :passes key replacing it with #{}. This indicates to
;; subsequent passes that while information may exist in the program
;; AST it is likely stale and should (must) be re-analyzed before use.


(defn clobber-passes
  [ast]
  (assoc ast :passes #{}))

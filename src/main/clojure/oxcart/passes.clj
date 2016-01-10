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


(defn whole-ast->modules
  "λ Whole-AST → (Seq Module)

  Returns the modules of the ASt as a sequence."
  [{:keys [modules] :as whole-ast}]
  (map whole-ast modules))


(defn whole-ast->forms
  "λ Whole-AST → (Seq AST)

  Returns a sequence of all the individual top level form ASTs in the
  given Whole-AST."
  [whole-ast]
  (->> whole-ast
       whole-ast->modules
       (mapcat :forms)))


(defn update-modules
  "λ Whole-AST → (λ Module → args * → Module) → args *

  Updates every module in the AST, replacing it with (apply f module
  args). Intended to eliminate repetitive Whole-AST comprehensions in
  pass implementations."
  [{:keys [modules] :as whole-ast} f & args]
  (->> (for [m modules]
         [m (apply f (get whole-ast m) args)])
       (into {})
       (merge whole-ast)))


(defn merge-vecs
  [forms]
  (reduce
   (fn [acc e]
     (concat acc
             (if (vector? e)
               e [e])))
   nil forms))


(defn update-forms
  "λ Whole-AST → (λ Form → args * → Form) → args *

  Updates every form in the given Whole-ast, replacing it with (apply
  f form args). Intended to eliminate repetitive Whole-AST
  comprehensions in pass implementations.

  Note that if a form is nil, it will be discarded.
  Note that if a form is a vector (presumably of forms) it will be
  concatinated into the forms sequence."
  [whole-ast f & args]
  (-> whole-ast
      (update-modules
       (fn [module]
         (update module :forms 
                 (fn [forms]
                   (->> forms
                        (map #(apply f %1 args))
                        (keep identity)
                        (merge-vecs)
                        (vec))))))))


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
  (update-in whole-ast [:passes] (fn [x y] (conj (or x #{}) y)) pass))


(defn require-pass
  [whole-ast pass options]
  (if (contains? (:passes whole-ast) pass)
    whole-ast
    (pass whole-ast options)))


(defn do-passes
  [whole-ast options & passes]
  (reduce #(require-pass %1 %2 options)
          whole-ast passes))


;; To enable this pattern however, transforming passes are required to
;; clobber the :passes key replacing it with #{}. This indicates to
;; subsequent passes that while information may exist in the program
;; AST it is likely stale and should (must) be re-analyzed before use.


(defn clobber-passes
  [whole-ast]
  (assoc whole-ast :passes #{}))


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

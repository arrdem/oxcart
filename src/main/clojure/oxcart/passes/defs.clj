(ns oxcart.passes.defs
  {:doc "Implements a var->def form location analysis pass for the Oxcart compiler."
   :added "0.0.2"
   :author "Reid McKenzie"}
  (:require [oxcart.pattern :as pattern]
            [oxcart.passes :refer :all]
            [clojure.tools.analyzer.ast :as ast]
            [clojure.set :refer [union]]))

(defn- locate-defs-in-module
  "λ Module → options → Module

  Helper function which implements definition location within the
  context of a single module."
  [module options]
  (->> (for [form (:forms module)
             :when (pattern/def? form)]
         [(pattern/def->symbol form) form])
       (into {})
       (assoc module :symbols)))

(defn- locate-publics-in-module
  "λ Module → options → Module

  Helper function which finds public symbols in the Module and creates
  the appropriate :public key in the module."
  [module options]
  (let [defs (:symbols module)]
    (->> (for [[symbol form] defs
               :when (pattern/public? form)]
           symbol)
         (into #{})
         (assoc module :public))))

(defn- locate-privates-in-module
  "λ Module → options → Module

  Helper function which finds private symbols in the Module and
  creates the appropriate :private key in the module."
  [module options]
  (let [defs (:symbols module)]
    (->> (for [[symbol form] defs
               :when (pattern/private? form)]
           symbol)
         (into #{})
         (assoc module :private))))

(defn- locate-consts-in-module
  "λ Module → options → Module

  Helper function which finds private symbols in the Module and
  creates the appropriate :cost key in the module."
  [module options]
  (let [defs (:symbols module)]
    (->> (for [[symbol form] defs
               :when (pattern/const? form)]
           symbol)
         (into #{})
         (assoc module :const))))

(defn- locate-dynamics-in-module
  "λ Module → options → Module

  Helper function which finds private symbols in the Module and
  creates the appropriate :dynamic key in the module."
  [module options]
  (let [defs (:symbols module)]
    (->> (for [[symbol form] defs
               :when (pattern/dynamic? form)]
           symbol)
         (into #{})
         (assoc module :dynamic))))

;; FIXME
;;   This pass is T(n) = 5*n, which is quite likely going to be a
;;   problem if iterated def location becomes an analysis
;;   priority. Squashing all the locate-*-9n-module operations into a
;;   single efficient pass over a single module would be slick.

(defn locate-defs
  "λ AST → options → AST

  This pass generates and deletes no code, it simply goes through each
  module and creates a :symbols map, which maps from symbols defined
  in the module's forms list to the defining forms.

    :symbols is the set of all defined symbols
    :public  is the set of symbols which are not annotated as private.
    :private is the complement set of :publics.
    :const   is the set of symbols which are marked constant.
    :dynamic is the set of symbols marked dynamic."
  [{:keys [modules] :as ast} options]
  (-> ast
      (update-modules 
       (fn [module]
         (-> module
             (locate-defs-in-module     options)
             (locate-publics-in-module  options)
             (locate-privates-in-module options)
             (locate-consts-in-module   options)
             (locate-dynamics-in-module options))))
      (record-pass locate-defs)))

(defn write-context
  [node]
  (-> node
      (ast/prewalk
       (fn [node]
         (cond (= :invoke (:op node))
               (assoc-in node [:fn :env ::context] :invoke)
               true node)))))

(defn locate-var-as-value
  "λ Whole-AST → Options → Whole-AST

  Walks the argument AST, computing taken as value data for each
  var. Creates a mapping `:usage' at the whole program level being
  {Var → (U :value :target)}.

  A Var maps to :value if it is _ever_ taken as a value, and has
  the :target value if and only if it occurs only as a target or not
  at all."
  [whole-ast options]
  (let [whole-ast (-> whole-ast
                      (require-pass locate-defs {})
                      (update-forms write-context))
        m         (transient {})]
    (doseq [form                      (whole-ast->forms whole-ast)
            {:keys [op var] :as node} (ast/nodes form)]
      (when var
        (assoc! m var
                (if (and (= :var op)
                         (not (= :value (get m node)))
                         (= (-> node :env ::context) :invoke))
                  :target
                  :value))))
    (-> whole-ast
        (assoc :usage (persistent! m))
        (record-pass locate-var-as-value))))

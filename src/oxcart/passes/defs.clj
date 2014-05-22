;;   Copyright (c) Reid McKenzie, Rich Hickey & contributors. The use
;;   and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0
;;   (http://opensource.org/licenses/eclipse-1.0.php) which can be
;;   found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be
;;   bound by the terms of this license.  You must not remove this
;;   notice, or any other, from this software.

(ns oxcart.passes.defs
  {:doc "Implements a var->def form location analysis pass for the Oxcart compiler."
   :added "0.0.2"
   :author "Reid McKenzie"}
  (:require [oxcart.pattern :as pattern]
            [clojure.set :refer [union]]))


(defn- locate-defs-in-module
  "λ Module → options → Module

  Helper function which implements definition location within the
  context of a single module."
  [module options]
  (reduce (fn [module form]
            (if (pattern/def? form)
              (assoc-in module 
                        [:symbols (pattern/def->symbol form)] form)
              form))
          module (:forms module)))


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
  (reduce
   (fn [ast module]
     (update-in ast [module]
                (-> module
                    (locate-defs-in-module     options)
                    (locate-publics-in-module  options)
                    (locate-privates-in-module options)
                    (locate-consts-in-module   options)
                    (locate-dynamics-in-module options))))
   ast modules))

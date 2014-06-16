;;   Copyright (c) Reid McKenzie, Rich Hickey & contributors. The use
;;   and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0
;;   (http://opensource.org/licenses/eclipse-1.0.php) which can be
;;   found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be
;;   bound by the terms of this license.  You must not remove this
;;   notice, or any other, from this software.

(ns oxcart.passes.rewrite-namespaces
  {:doc "This namespace provides utilities required to rewrite
        namespace forms into module data from top level forms."
   :author "Reid McKenzie"
   :added "0.0.9"}
  (:require [oxcart.util :refer [update ->>ensure ->ensure]]
            [oxcart.passes :refer [record-pass update-modules]]))


(defn import?
  [form]
  (-> form
      :raw-forms   ; (->ensure #(every? list? %1) "Didn't get a forms list!")
      last         ; (->ensure list? "Didn't get a form!")
      first        ; (->ensure symbol? "Didn't get a symbol!")
      (= 'import)))


(defn import->form [form]
  (when (import? form)
    (-> form
        :raw-forms ; (->ensure #(every? list? %1) "Didn't get a forms list!")
        last       ; (->ensure list? "Didn't get a form!")
        rest
        vec)))


(defn require?
  [form]
  (-> form
      :form        ; (->ensure list? "Didn't get a form!")
      first        ; (->ensure symbol? "Didn't get a symbol!")
      (= 'require)))


(defn require->form [form]
  (when (require? form)
    (-> form
        :form      ; (->ensure list? "Didn't get a form!")
        rest
        vec)))


(defn use?
  [form]
  (-> form
      :form        ; (->ensure list? "Didn't get a form!")
      first        ; (->ensure symbol? "Didn't get a symbol!")
      (= 'use)))


(defn use->form [form]
  (when (use? form)
    (-> form
        :form      ; (->ensure list? "Didn't get a form!")
        rest
        vec)))


(defn in-ns?
  [form]
  (-> form
      :form        ; (->ensure list? "Didn't get a form!")
      first        ; (->ensure symbol? "Didn't get a symbol!")
      (#(or (= %1 'in-ns)
            (= %1 'clojure.core/in-ns)))))


(defn rewrite-module-deps
  "λ Module → Module

  This helper function analyzes a module, deleting its ns form and
  creating the keys `:requires' `:uses' and `:imports' in the
  module. Returns an updated module with no top level import, use or
  require forms.

  `:requires'
    is a set of symbols, being a vector of require forms in order of
    occurance in the namespace.

  `:uses'
    As with `:requires' a vector of use forms in order of occurance.

  `:imports'
    As with `:uses' a vector of import forms in order of occurance."
  [{:keys [forms] :as module}]
  (let [data (atom {:requires []
                    :uses     []
                    :imports  []})
        new-forms (atom [])]

    (doseq [form forms]
      (cond (import? form)
            (swap! data update :imports  conj (import->form form))
            
            (require? form)
            (swap! data update :requires conj (require->form form))

            (use? form)
            (swap! data update :uses     conj (use->form form))
            
            true
            (swap! new-forms conj form)))

    (-> module
        (assoc :forms @new-forms)
        (merge @data))))


(defn drop-in-ns
  "λ Module → Module

  Rewrites the argument module discarding the leading in-ns form if
  present, otherwise making no changes. Returns the updated Module."
  [module]
  (letfn [(f [forms]
              (if (in-ns? (first forms))
                (vec (rest forms))
                forms))]
    (update module :forms f)))


(defn rewrite-namespaces
  "λ Whole-AST → Options → Whole-AST

  This pass analyzes the namespace form of each module in the whole
  program, generating module metadata describing imports, uses and
  requires. This pass then deletes the ns form and relies on emitters
  to use the module requirement data.

  Options
  -----------
    This pass takes no options."
  [whole-ast options]
  (-> whole-ast
      (update-modules (comp rewrite-module-deps drop-in-ns))
      (record-pass rewrite-namespaces)))

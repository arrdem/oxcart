;;   Copyright (c) Reid McKenzie, Rich Hickey & contributors. The use
;;   and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0
;;   (http://opensource.org/licenses/eclipse-1.0.php) which can be
;;   found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be
;;   bound by the terms of this license.  You must not remove this
;;   notice, or any other, from this software.

(ns oxcart.emitter.core
  {:doc "Oxcart will wind up with more than one emitter, but we'll
        start with this for now."
   :author "Reid McKenzie"
   :added "0.0.12"}
  (:require [oxcart
             [util :refer [var-name]]
             [passes :as passes :refer [require-pass]]
             [pattern :as pattern]]
            [oxcart.passes.tree-shake
             :refer [analyze-var-uses tree-shake analyze-var-dependencies]]
            [oxcart.passes.fn-reduction 
             :refer [reduce-fn-arities]]))

(defn make-class
  [name]
  {:name       name
   :attr       0
   :super      0
   :fields     []
   :methods    {}
   :interfaces #{}})

(defn add-fn-field
  [class child-class]
  )

(defn cinit-last-fn-field
  [class child-class]
  )

(defn add-method
  "Adds a new method with the given name to the given class, returning
  the updated class."
  [class name {:keys [op] :as method}]
  {:pre [(= :method op)
         (string? name)]}
  )

(defn class->tejvm-class
  "Rewrites a class into the TEJVM format for a class, returning a
  valid TEJVM class presumably for final bytecode emission."
  [class]
  )

;;--------------------------------------------------------------------

(defn -emit-as-method
  "(λ Form → This) → (This, nil)"
  [{:keys [op init var] :as form} this]
  {:pre [(= op :def)
         (= :fn (:op init))
         (var? var)]}
  (let [sym-name (var-name var)]
    [(add-method this (name sym-name) (first init)) nil]))


(defn -emit-as-class
  "(λ Form → This) → (This, (Seq Class))

  the `this' argument is ignored and passed through unmodified. The
  first (and only) class in the returned sequence of classes is the
  result of emitting the argument def form as a standalone class
  with (potentially) multiple implementations of the invoke method."
  [{:keys [init var] :as form} this]
  (let [sym-name (var-name var)
        gen-class (-> (str (namespace sym-name) "." (name sym-name))
                      make-class)
        gen-class (reduce #(add-method %1 "invoke" %2) gen-class (:methods init))]
    [this [gen-class]]))


(defn -emit-as-field
  "(λ Form → This) → (This, (Seq Class))

  Emits the argument def as a class, writing a new static public field
  into the `this` class being an instance of this new class."
  [{:keys [name op] :as form} this]
  {:pre [(= op :def)
         (not (nil? name))
         (symbol? name)]}
  (let [[this₁ [child :as helpers]] (-emit-as-class form this)]
    [(-> this₁
         (add-fn-field child)
         (cinit-last-fn-field name child))
     helpers]))


(defn -emit-form
  "(λ Form → This) → (This, (Seq Class))

  FIXME:
    Note that this will totally explode for anything that isn't a
    fn. Defs of constants aren't supported yet."
  [{:keys [op] :as form} this]
  (cond (and (= :def op)
             (= :fn  (-> form :init :op)))
        (if (-> form :meta :static)
          (-emit-as-method form this)
          (-emit-as-field  form this))

        true
        (assert false "lolwut not yet supported")))


(defn emit-form
  "(λ (This . (Seq Class)) → Form) → (This . (Seq Class))

  Helper function extracted from emit-aot

  Emits a single form, returning a new partial definition of \"this\",
  and a sequence of auxiliary classes. This partial definition of
  \"this\" contains any fields, methods and initializer code required
  for the above."
  [[this classes] form]
  (let [[this' helper-classes] (-emit-form form this)]
    [this' (concat classes helper-classes)]))


(defn emit-namespace
  "(λ Namespace) → (This . (Seq Class))

  Helper function extracted from emit-aot

  Emits a single namespace by walking all forms (which can only be
  defs), emitting each form, and accumulating a \"this\" being the
  namespace class itself, and a sequence of helper classes. Returns a
  sequence of classes, including the final \"self\" class and all
  helpers."
  [{:keys [name forms] :as namespace}]
  (let [this           (make-class name)
        classes        []
        [this classes] (reduce emit-form [this classes] forms)
        this           (select-keys this [:name :attr :super :fields :methods :interfaces])]
    (conj classes this)))


(defn compile-class
  "(λ Class) → (Name . Bytecode"
  []
  nil)


(defn write-class
  "(λ (Class . Bytecodes)) → Nil

  Helper function which takes a Class and spits out a bytecode file
  for side-effects. Returns nil."
  [[classname bytecode]]
  
  nil)


(defn emit-aot
  "(λ Whole-AST → Options) → nil

  Emits a whole program with reference to a single emtry point as
  named by the argument var, writing class files for side-effects.

  Options
  -----------
  `:entry'
    a Var, being the var with reference to which the entire program
    will be emitted."
  [whole-program {:keys [entry] :as options}]
  {:pre  [(var? entry)]
   :post [(nil? %)]}
  (let [whole-program (-> whole-program
                          (require-pass tree-shake        {})
                          (require-pass reduce-fn-arities {})
                          (require-pass tree-shake        {})
                          (require-pass analyze-var-uses  {}))
        reach         (get whole-program :var-reached)
        load          (->> entry reach (map #(. %1 ns)) (into #{}))
        classes       (mapcat emit-namespace load)]
    (doseq [c classes]
      (-> c compile-class write-class)))
  nil)

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
             [passes :as passes]
             [pattern :as pattern]]))


(defn -emit-as-class
  "This is an emitter case for handling forms (functions taken as
  values) which must be implemented as stand alone IFn classes."
  []
  )


(defn -emit-as-method
  "This is an emitter case for handling forms (functions of a single
  arity never taken as values) which may be implemented as static
  methods on the wrapping namespace class."
  []
  )


(defn -emit-as-field
  "(λ Named-ASTs → Named-Classes → Expression) → Named-Classes

  This is an emitter case which works together with -emit-as-class to
  emit constants (functions of multiple arities taken as values,
  inline constants) which must be emitted as public static fields in
  the wrapping namespace class."
  [named-asts named-classes expr]
  )


(defn emit-classes
  "(λ Named-ASTs → Entry-Point → Options) → {String → Class}

  This is the entry point for the AOT emitter. This is a function of a
  pair ({Symbol → AST}, EntryPoint :- Symbol). The emitter will be
  recursively invoked on the entry point symbol. Returns a map from
  Strings, being the names of classes, to ASTs representing class
  bytecode.

  Options
  -----------
    This emitter takes no options."
  [named-asts entry-point options]
  )

(defn emit-form
  [{:keys [init] :as form}]
  {:pre [(pattern/def? form)]}
  )

(defn merge-classes
  "Helper function for emit-form

  Takes two class definitions presumed to contain no name conflicts
  besides <init> and <cinit>, returning a new class having all methods
  and fields from both class definitions and <init> and <cinit>
  defined by concatinating the inits of r to those of r."
  [l r]  
  )


(def emit-form
  "Helper function extracted from emit-aot

  Emits a single form, returning a new partial definition of \"this\",
  and a sequence of auxiliary classes. This partial definition of
  \"this\" contains any fields, methods and initializer code required
  for the above."
  [[this classes] form]
  (let [[this' helper-classes] (emit-form form)]
    [(merge-classes this this')
     (concat classes
             helper-classes)]))


(def emit-namespace
  "Helper function extracted from emit-aot

  Emits a single namespace by walking all forms (which can only be
  defs), emitting each form, and accumulating a \"this\" being the
  namespace class itself, and a sequence of helper classes. Returns a
  sequence of classes, including the final \"self\" class and all
  helpers."
  [classes namespace]
  (let [[this classes]
        (reduce emit-form [this classes] (forms ns))]
    (conj classes this)))


(defn emit-aot
  "Emits a whole program with reference to a single emtry point."
  [whole-program entry-name]
  (let [load (->> entry reach (map .ns) (into #{}))]
    (reduce emit-namespace [] load)))


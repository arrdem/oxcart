(ns oxcart.vars
  (:refer-clojure :exclude [*loaded-libs*]))


(def ^:dynamic *loaded-libs*
  "A set, being those namespaces which have been loaded in compilation
  context. Note that this set has no relationship with
  clojure.core/*loaded-libs*, meaning that mod aliasing concerns
  compilation occurs in a different loading context than compiler
  execution."
  (atom #{}))

;;   Copyright (c) Reid McKenzie, Rich Hickey & contributors. The use
;;   and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0
;;   (http://opensource.org/licenses/eclipse-1.0.php) which can be
;;   found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be
;;   bound by the terms of this license.  You must not remove this
;;   notice, or any other, from this software.

(ns oxcart.vars
  (:refer-clojure :exclude [*loaded-libs*]))


(def ^:dynamic *loaded-libs*
  "A set, being those namespaces which have been loaded in compilation
  context. Note that this set has no relationship with
  clojure.core/*loaded-libs*, meaning that mod aliasing concerns
  compilation occurs in a different loading context than compiler
  execution."
  (atom #{}))

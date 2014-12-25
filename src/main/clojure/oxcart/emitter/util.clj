(ns oxcart.emitter.util
  (:refer-clojure :exclude [flatten]))

(defn instr?
  [x]
  (if (or (vector? x) (seq? x))
    (let [[target & more] x]
      (keyword? target))
    false))

(defn flatten
  [x]
  (if (instr? x) [x]
      (mapcat flatten x)))

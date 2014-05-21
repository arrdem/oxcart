(ns oxcart.pattern)


(defn def?
  "λ AST -> Boolean

  Indicates whether the top level form of the argument AST is a def form."
  [ast]
  (-> ast
      :op
      (= :def)))


(defn def->symbol
  "λ AST -> (Option Symbol)

  If the argument form was a def, returns the defined
  symbol. Otherwise the return value is garbage."
  [ast]
  (when (def? ast)
    (:name ast)))


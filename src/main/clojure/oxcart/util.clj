(ns oxcart.util
  {:doc "Implements various AST utilities common to different parts of
        Oxcart."
   :added "0.0.4"
   :author "Reid McKenzie"}
  (:refer-clojure :exclude [macroexpand-1 macroexpand])
  (:require [clojure.tools.analyzer.jvm :as ana.jvm]
            [clojure.tools.analyzer
             :refer [macroexpand-1
                     macroexpand]
             :as ana]
            [clojure.tools.analyzer.ast :refer [prewalk postwalk]]
            [clojure.tools.analyzer.passes.elide-meta
             :refer [elides]]))

(defn ast
  "λ Form → AST
   λ Form → Env → AST

  Wraps the clojure.tools.analyzer.jvm analysis system to provide
  reading and analysis of JVM Clojure forms in a user concise fashion."
  ([form]
     (ast form (ana.jvm/empty-env)))

  ([form env]
     (binding [ana/macroexpand-1 ana.jvm/macroexpand-1
               ana/create-var    ana.jvm/create-var
               ana/parse         ana.jvm/parse
               ana/var?          var?]
       (-> (binding [macroexpand-1 ana.jvm/macroexpand-1]
             (macroexpand form env))
           (ana.jvm/analyze env)))))


(defn clear-env [ast]
  (postwalk ast #(dissoc %1 :env)))

(defn minimize [ast]
  (postwalk ast
            (fn [node]
              (let [keep (into #{:op :var :id} (:children node))]
                (reduce dissoc node (remove keep (keys node)))))))

(defn take-when
  "λ (λ T → Bool) → (Seq T) → [(Option T) (Seq T)]

  When pred is true of the head of seq, return [head tail]. Otherwise
  [nil seq]. Used as a helper for parsing optional typed elements out
  of sequences. Say docstrings out of argument seqs."
  [pred seq]
  (if (pred (first seq))
    ((juxt first rest) seq)
    [nil seq]))


(defn map-vals
  "λ {A → B} → (λ B → more* → C) → more* → {A → C}

  Computes a new map from m preserving the keys of m, but mapping the
  keys of m to (apply f (get m k) args)."
  [m f & args]
  (->> (for [[k v] m]
         [k (apply f v args)])
       (into {})))


(defn fix
  "λ (fn T → T) → T → T

  Eagerly computes the fixed point combinator of the input function
  and value. As this computation is eager, it will terminate only when
  a fixed point is reached which may be never."
  [f dat]
  (let [dat' (f dat)]
    (if (= dat dat')
      dat
      (recur f dat'))))


(defn update
  "λ {A → B} → A → (λ B → args* → C) → args* → {A → C}

  Updates a key in the map by applying f to the value at that key more
  arguments, returning the resulting map."
  [map key f & args]
  (assoc map key
         (apply f (get map key) args)))


(defn format-line-info
  "λ AST → (Option String)

  Emits a string with the file, line and column information from the
  argument AST. Returns nil if the argument AST lacks the appropriate
  metadata."
  [ast]
  (let [{:keys [file line column]} (-> ast :meta :env)]
    (when (and file line column)
      (format "%s:%s:%s" file line column))))


(defn ->ensure
  ([v pred]
     (->ensure v pred "->assert failed!"))

  ([v pred error]
     (if (pred v) v
         (assert false error))))


(defn ->>ensure
  ([pred v]
     (->>ensure pred "->>assert failed!" v))

  ([pred error v]
     (if (pred v) v
         (assert false error))))


(defn var->sym
  [v]
  (symbol
   (-> v .ns ns-name str)
   (-> v .sym str)))


(defn var->name [v]
  {:pre  [(var? v)]
   :post [(symbol? %)]}
  (-> v .sym))


(defn var->ns [v]
  {:pre  [(var? v)]
   :post [(symbol? %)]}
  (-> v .ns ns-name))


(defn eval-in
  "(λ Sexpr → Env) → Any

  Helper for evaluating a given sexpr in another namespace as specified by a
  TANAL env record/map. Returns the result of evaluating the sexpr in the other
  environment."
  [form {other-ns :ns :as env}]
  (let [this (.name *ns*)]
    (in-ns other-ns)
    (let [res (clojure.core/eval form)]
      (in-ns this)
      res)))

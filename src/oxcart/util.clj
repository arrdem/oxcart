;;   Copyright (c) Reid McKenzie, Rich Hickey & contributors. The use
;;   and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0
;;   (http://opensource.org/licenses/eclipse-1.0.php) which can be
;;   found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be
;;   bound by the terms of this license.  You must not remove this
;;   notice, or any other, from this software.

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
             :as ana]))

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


(defn take-when
  "When pred is true of the head of seq, return [head tail]. Otherwise
  [nil seq]. Used as a helper for parsing optinal typed elements out
  of sequences. Say docstrings out of argument seqs."
  [pred seq]
  (if (pred (first seq))
    ((juxt first rest) seq)
    [nil seq]))


(defn map-vals
  [m f & args]
  (->> (for [[k v] m]
         [k (apply f v args)])
       (into {})))


(defn fix [f dat]
  (let [dat' (f dat)]
    (if (= dat dat')
      dat
      (recur f dat'))))


(defn update [map key f & args]
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

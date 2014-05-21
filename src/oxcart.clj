;;   Copyright (c) Reid McKenzie, Rich Hickey & contributors.  The use
;;   and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0
;;   (http://opensource.org/licenses/eclipse-1.0.php) which can be
;;   found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be
;;   bound by the terms of this license.  You must not remove this
;;   notice, or any other, from this software.

(ns oxcart
  (:refer-clojure :exclude [eval macroexpand-1 macroexpand load])
  (:require [clojure.tools.analyzer.jvm :as ana.jvm]
            [clojure.tools.analyzer 
             :refer [macroexpand-1 
                     macroexpand]
             :as ana]
            [clojure.tools.emitter.jvm.emit :as e]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.tools.reader :as r]
            [clojure.tools.reader.reader-types :as readers]
            [oxcart.pattern :as patern])
  (:import clojure.lang.IFn))


(def root-directory 
  @#'clojure.core/root-directory)

(def ^:dynamic *load-configuration*
  nil)

(defn atom? [x]
  (instance? clojure.lang.Atom x))

(defn eval
  ([form] 
     (eval form {:debug? false}))

  ([form {:keys [debug? ast env classloader] :as options}]
     (let [defs-ast  (:defs ast)
           forms-ast (:forms ast)
           mform     (binding [macroexpand-1 ana.jvm/macroexpand-1]
                       (macroexpand form (or env
                                             (ana.jvm/empty-env))))]

       (if (and (seq? mform)
                (= 'do (first mform)))

         ;; DO form handling
         ;;---------------------
         (let [[statements ret]
               (loop [statements  []
                      [e & exprs] (rest mform)]
                 (if-not (seq exprs)
                   [statements e]
                   (recur (conj statements e) exprs)))]

           (doseq [expr statements]
             (eval expr options))
           
           (eval ret options))

         ;; bare expression handling
         ;;-----------------------------
         (let [ast (binding [ana/macroexpand-1 ana.jvm/macroexpand-1
                             ana/create-var    ana.jvm/create-var
                             ana/parse         ana.jvm/parse
                             ana/var?          var?]
                     (ana.jvm/analyze mform (or env (ana.jvm/empty-env))))
               r   (e/emit (ana.jvm/analyze `(^:once fn* [] ~mform)
                                            (or env (ana.jvm/empty-env)))
                           {:debug?       debug?
                            :class-loader (or classloader
                                              (clojure.lang.RT/makeClassLoader))})
               class (-> r meta :class)]
           
           ;; the accumulator for the whole read program
           (when (and forms-ast
                      (atom? forms-ast))
             (swap! forms-ast conj ast))
           
           ;; the accumulator for defs to structure
           (when (and defs-ast
                      (atom? defs-ast)
                      (patern/def? ast))
             (swap! defs-ast assoc 
                    (patern/def->symbol ast)
                    ast))

           ;; and run the code for side-effects
           (.invoke ^IFn (.newInstance ^Class class)))))))


(defn load
  ([res]
     (load res
           (or *load-configuration*
               {:debug? false})))

  ([res {:keys [debug?] :as options}]
     (let [p      (str (apply str (replace {\. \/ \- \_} res)) ".clj")
           eof    (Object.)
           p      (if (.startsWith p "/")
                    (subs p 1)
                    (-> *ns* ns-name root-directory (str "/" p) (subs 1)))
           file   (-> p io/resource io/reader slurp)
           reader (readers/indexing-push-back-reader file 1 p)]
       (binding [*ns*                 *ns*
                 *file*               p
                 *load-configuration* options]
         (loop []
           (let [form (r/read reader false eof)]
             (when (not= eof form)
               (when debug?
                 (println *ns* "|" form))
               (eval form options)
               (recur))))))))

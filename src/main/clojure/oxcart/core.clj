;;   Copyright (c) Reid McKenzie, Rich Hickey & contributors. The use
;;   and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0
;;   (http://opensource.org/licenses/eclipse-1.0.php) which can be
;;   found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be
;;   bound by the terms of this license.  You must not remove this
;;   notice, or any other, from this software.

(ns oxcart.core
  {:doc "Implementation of the Oxcart compiler & API."
   :added "0.0.1"
   :author "Reid McKenzie"}
  (:refer-clojure :exclude [eval macroexpand-1 macroexpand load compile gensym])
  (:require [clojure.tools.analyzer.jvm :as ana.jvm]
            [clojure.tools.analyzer
             :refer [macroexpand-1
                     macroexpand]
             :as ana]
            [clojure.tools.emitter.jvm.emit :as e]
            [clojure.tools.emitter.jvm :as em.jvm]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.tools.reader :as r]
            [clojure.tools.reader.reader-types :as readers]
            [oxcart.vars :refer :all]
            [oxcart.util :as util]
            [oxcart.core-redefs]
            [oxcart.emitter.jvm :refer [emit]])
  (:import clojure.lang.IFn)
  (:gen-class))


(def root-directory
  @#'clojure.core/root-directory)


(def clojure-gensym
  @#'clojure.core/gensym)


(def ^:dynamic *load-configuration*
  "Dynamic var that oxcart/load uses to stash its configuration.

  Because load invocations may be recursive thanks to use, require and
  literal load uses in order for a load to capture all the compilation
  requested into a configuration containing AST atoms the AST atoms
  must be made visible to subsequent invocations of oxcart/load. This
  var combined with the single arity case of oxcart/load provides the
  required configuration persistance.

  nil by default, may be a valid configuration map for oxcart/load."
  nil)


(defn atom? [x]
  (instance? clojure.lang.Atom x))


(defn gensym
  "(λ) → Symbol
   (λ → Prefix) → Symbol

  Wrapper around clojure.core/gensym which adds metadata annotating
  generated symbols and permitting distinction between generated and
  user specified symbols.

  If Prefix is provided, it must be a string. Otherwise a prefix of
  \"OG__\" is used by default."
  ([]
     (gensym "OG__"))

  ([x]
     (with-meta
       (clojure-gensym x)
       {:gensym true})))


(defn eval
  "(λ form) → value
  (λ form → config-map) → value

  Form is any Clojure sexpr, config map is a load configuration and
  value is an arbitrary Clojure value or class.

  Analyzes the form argument via tools.analyzer.jvm, emits the
  appropriate bytecode and returns a computed result side effecting
  the invoking Clojure runtime."

  ([form]
     (eval form
           (or *load-configuration*
               {:debug? false})))

  ([form {:keys [debug? eval? exec? forms classloader env]
          :or   {debug? false
                 eval?  true
                 env    (ana.jvm/empty-env)}
          :as   options}]
     (let [mform (binding [macroexpand-1 ana.jvm/macroexpand-1]
                   (macroexpand form env))]

       (cond (and (seq? mform)
                  (= 'do (first mform)))

             ;; DO form handling
             ;;---------------------
             (let [[statements ret]
                   (loop [statements  []
                          [e & exprs] (rest mform)]
                     (if-not (seq exprs)
                       [statements e]
                       (recur (conj statements e) exprs)))

                   ;; If we are in the do of an (ns), discard all forms.
                   options (if (#{'clojure.core/ns 'ns} (first form))
                             (dissoc options :forms)
                             options)]
               
               (doseq [expr statements]
                 (eval expr options))
               
               (eval ret options))

             (nil? mform)
             ;; Top level nil handling
             ;;---------------------------
             nil


             true
             ;; Bare expression handling
             ;; ----------------------------
             (do ;; Run the code for side-effects.
                 (let [res (when (and eval?
                                      (not (= 'clojure.core (.name *ns*))))
                             (em.jvm/eval mform))]
                   
                   (let [ast (-> mform
                                 (util/ast))]
                     
                     ;; Add to the accumulator for the whole read
                     ;; program. Note that the following forms are
                     ;; discarded:
                     ;; 
                     ;; - clojure.core/require
                     ;; - clojure.core/use
                     ;; - clojure.core/refer
                     ;; - clojure.core/import
                     (when (and forms
                                (atom? forms)
                                (not (#{'clojure.core/require 'require
                                        'clojure.core/use     'use
                                        'clojure.core/refer   'refer
                                        'clojure.core/import  'import
                                        'clojure.core/import* 'import*}
                                      (first form))))
                       (swap! forms
                              #(-> %1
                                   (update-in [(.name *ns*) :forms]
                                              (comp vec concat) [ast])
                                   (update-in [:modules]
                                              (fn [x]
                                                (conj (or x #{})
                                                      (.name *ns*))))))))
                   res))))))


(defmacro with-macro-redefs
  "Bindings is a sequence of pairs of symbols, where the left hand
  side name macros and the right hand side names a new function which
  the macro fn shall be replaced with."
  [[l r & bindings] & forms]
  (if (and l r)
    `(let [rootv# (deref ~l)]
       (try
         (alter-var-root ~l (constantly ~r))
         (with-macro-redefs [~@bindings] ~@forms)
         (finally 
           (alter-var-root ~l (constantly rootv#)))))
    `(do ~@forms)))


(defn load
  "(λ String) → nil
   (λ String → config-map) → nil

  Loads a resource on the classpath identified by a string path as
  clojure code via eval for side-effects against the invoking Clojure
  runtime.

  config-map:
    If config-map is not provided, it will default to the value of
    *load-configuration* or {:debug? false}.

    If config contains a :debug? key, then printing of the generated
    class bytecode and other development information is enabled.

    If config contains an atom at :forms the ASTs of all read forms
    will be conj'd to it in reading & evaluation order."

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
         (with-redefs [clojure.core/load   oxcart.core/load
                       clojure.core/eval   oxcart.core/eval
                       clojure.core/gensym oxcart.core/gensym]
           (with-macro-redefs [#'clojure.core/defmulti        @#'oxcart.core-redefs/defmulti
                               #'clojure.core/defmethod       @#'oxcart.core-redefs/defmethod
                               #'clojure.core/deftype         @#'oxcart.core-redefs/deftype
                               #'clojure.core/defprotocol     @#'oxcart.core-redefs/defprotocol
                               #'clojure.core/proxy           @#'oxcart.core-redefs/proxy
                               #'clojure.core/extend-type     @#'oxcart.core-redefs/extend-type
                               #'clojure.core/extend-protocol @#'oxcart.core-redefs/extend-protocol]
             (loop []
               (let [form (r/read reader false eof)]
                 (when (not= eof form)
                   (eval form options)
                   (recur))))))))
  nil))


(defn load-ast
  "(  Resource)   Whole-AST
  (  Resource   Options)   Whole-AST

  Wrapper around the main load function which provides helper
  mechanics for the common case of wanting to load a resource and get
  a whole program AST as a return value."
  ([res]
     (load-ast res {}))

  ([res opts]
     (let [forms (atom {})]
       (load res (merge opts {:forms forms}))
       @forms)))


(defn compile
  "(λ String) → nil
   (λ String → config-map) → nil

  Loads a resource via load, applies any compilation transformations
  specified in the config-map, emitting and loading the resulting
  class files.

  config-map:
   `:passes` is a sequence of functions constituting an optimization
    configuration. These functions must be
    (λ ast → settings → ast)
    and pure. It is expected but not enforced that passes may not
    emit nodes which an emitter does not support. If passes is empty
    no program transformations will be done and the emitter will be
    invoked directly.

    `:emitter` is a single function which is λ ast → settings → nil
    which is expected to emit the appropriate classfiles as side
    effects.

    `:settings` may be a map containing `:passes` and `:emitter`,
    these being option maps which will be applied respectively at the
    invocation of each pass and the emitter.

    `[:settings :entry]` is expected to be a fully qualified symbol,
    being a -main method or other program entry point."

  ([res]
     ;; FIXME:
     ;;  Saner defaults with respect to the default emitter and
     ;;  default passes would probably be a good thing in future.
     (compile res nil))

  ([res {:keys [passes emitter settings] :as config}]
     (let [forms  (atom {})
           config {:debug? false
                   :exec?  true
                   :forms forms}]
       ;; Loads and macroexpands all the code using the built config
       ;; to generate the forms and defs structures.
       ;;
       ;; By definition of a well formed Clojure program, loading the
       ;; "root" resource must load all depended resources, otherwise
       ;; there will be a symbol resolution error in the compiling
       ;; Clojure runtime.
       (load res config)

       (let [settings (:passes settings)]
         (doseq [pass passes]
           (swap! forms pass settings)))

       ;; Having taken an O(Nᵏ) compile operation to the face we now
       ;; run the emitter and call it quits.
       (let [settings (:emitter settings)
             settings (merge settings
                             (select-keys settings [:entry]))]
         (emitter @forms settings)))
     nil))

(defn -main [namespace]
  (let [the-ns (symbol namespace)
        entry  (symbol namespace "-main")]
    (compile (s/replace namespace #"\." "/")
             {:emitter emit
              :settings {:emitter {:entry entry}}}))
  (shutdown-agents)
  nil)

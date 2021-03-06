;;   Copyright (c) Reid McKenzie, Rich Hickey & contributors. The use
;;   and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0
;;   (http://opensource.org/licenses/eclipse-1.0.php) which can be
;;   found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be
;;   bound by the terms of this license.  You must not remove this
;;   notice, or any other, from this software.

(ns oxcart.emitter.jvm
  {:doc "Oxcart will wind up with more than one emitter, but we'll
        start with this for now."
   :author "Reid McKenzie"
   :added "0.0.12"}
  (:refer-clojure :exclude [flatten])
  (:require [clojure.string :as s]
            [clojure.pprint :as pp]
            [clojure.java.io :as io]
            [oxcart.pattern :as pattern]
            [oxcart.emitter.jvm.emitter :as e]
            [oxcart.util                         :refer [var->sym var->ns var->name]]
            [oxcart.emitter.util                 :refer :all]
            [oxcart.passes :as passes            :refer [do-passes whole-ast->forms]]
            [oxcart.passes.tree-shake            :refer [analyze-var-uses tree-shake analyze-var-dependencies]]
            [oxcart.passes.fn-reduction          :refer [reduce-fn-arities]]
            [oxcart.passes.defs                  :refer [locate-var-as-value]]
            [clojure.tools.emitter.jvm.transform :refer [-compile]]
            [taoensso.timbre :as timbre          :refer [debug info warn]]))

(defn write-class
  "(λ ClassName → Bytecode) → Nil

  Writes the given bytecode to a file named by the ClassName and
  *compile-path*. Requires that *compile-path* be set. Returns Nil."
  [name bytecode]
  {:pre [(bound? #'clojure.core/*compile-path*)]}
  (let [path (str *compile-path* "/" name ".class")
        file (io/file path)]
    (.mkdirs (io/file (.getParent file)))
    (with-open [w (java.io.FileOutputStream. path)]
      (.write w bytecode)
      (info "Wrote class" name)))
  nil)


(defn preprocess-whole-ast
  [whole-program options]
  (do-passes whole-program options
             tree-shake reduce-fn-arities
             tree-shake analyze-var-uses))

(defn emit-classes
  "(λ Whole-AST → Options) → (Seq Class-AST)

  Emits a whole program with reference to a single emtry point as
  named by the argument var, returning dependency ordered class ASTs
  representing the emitted program.

  Options
  -----------
  `:entry'
    a Symbol, being the var with reference to which the entire program
    will be emitted."
  [whole-program
   {:keys [entry] :as options}]
  {:pre  [(symbol? entry)]}
  (let [whole-program (preprocess-whole-ast whole-program options)
        reach         (-> whole-program (get :reach-map) (get (resolve entry)))
        reach-defs    (->> whole-program
                           whole-ast->forms
                           (filter #(and (pattern/def? %)
                                         (reach (pattern/def->var %)))))]
    (conj
     (mapv #(e/emit %1 {}) reach-defs)
     (let [class-name (namespace entry)]
       {:op          :class
        :attr        #{:public :super :final}
        :class-name  class-name
        :super       "java/lang/Object"
        :name        (s/replace class-name \. \/)
        :methods     [{:op     :method
                       :attr   #{:public :static}
                       :method [[:<clinit>] :void]
                       :code   [[:start-method]
                                [:return-value]
                                [:end-method]]}
                      {:op     :method
                       :attr   #{:public}
                       :method [[:<init>] :void]
                       :code   [[:start-method]
                                [:load-this]
                                [:invoke-constructor [:java.lang.Object/<init>] :void]
                                [:return-value]
                                [:end-method]]}
                      {:op     :method
                       :attr   #{:public :static}
                       :method `[[:main "java.lang.String[]"] :void]
                       :code   (flatten
                                [[:start-method]

                                 (for [v (map pattern/def->var reach-defs)
                                       :when (not= v (resolve entry))]
                                   [[:push (str (var->ns v))]
                                    [:push (str (var->name v))]
                                    [:invoke-static [:clojure.lang.RT/var :java.lang.String :java.lang.String] :clojure.lang.Var]
                                    [:new-instance (e/var->class v)]
                                    [:dup]
                                    [:invoke-constructor [(keyword (e/var->class v) "<init>")] :void]
                                    [:invoke-virtual [:clojure.lang.Var/bindRoot :java.lang.Object] :void]])

                                 (let [v (resolve entry)]
                                   [[:new-instance (e/var->class v)]
                                    [:dup]
                                    [:push (str (var->ns v))]
                                    [:push (str (var->name v))]
                                    [:invoke-static [:clojure.lang.RT/var :java.lang.String :java.lang.String] :clojure.lang.Var]
                                    [:swap]
                                    [:dup]
                                    [:invoke-constructor [(keyword (e/var->class v) "<init>")] :void]
                                    [:invoke-virtual [:clojure.lang.Var/bindRoot :java.lang.Object] :void]
                                    [:aload 0]
                                    [:invoke-static [:clojure.lang.RT/seq :java.lang.Object] :clojure.lang.ISeq]
                                    [:invoke-interface [:clojure.lang.IFn/applyTo :clojure.lang.ISeq] :java.lang.Object]
                                    [:pop]
                                    [:return-value]
                                    [:end-method]])])}]}))))

(defn emit
  "(λ Whole-AST → Options) → Nil

  Emits a whole program with reference to a single emtry point as
  named by the argument var, writing class files for side-effects.

  Options
  -----------
  `:entry'
    a Symbol, being the var with reference to which the entire program
    will be emitted."
  [whole-program options]
  (doseq [{:keys [name] :as c}
          (emit-classes whole-program options)]
    (->> c -compile (write-class name)))
  nil)

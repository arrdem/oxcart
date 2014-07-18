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
  (:refer-clojure :exclude [flatten])
  (:require [clojure.string :as s]
            [clojure.pprint :as pp]
            [clojure.java.io :as io]
            [oxcart
             [util :refer [var-name var->ns var->name]]
             [passes :as passes :refer [require-pass whole-ast->forms]]
             [pattern :as pattern]]
            [oxcart.passes.tree-shake
             :refer [analyze-var-uses tree-shake analyze-var-dependencies]]
            [oxcart.passes.fn-reduction
             :refer [reduce-fn-arities]]
            [oxcart.passes.defs
             :refer [locate-var-as-value]]
            [clojure.tools.emitter.jvm.transform
             :refer [-compile]]
            [clojure.tools.analyzer.jvm.utils
             :refer [primitive? prim-or-obj box numeric?] :as j.u]
            [clojure.tools.emitter.jvm.emit
             :refer [emit-constants]]
            [clojure.tools.emitter.jvm.intrinsics
             :refer [intrinsic intrinsic-predicate]]))

(declare -emit emit emit-classes emit-program) ;; public API crap

(defn dispatch-fn
  [{:keys [op] :as node} _frame]
  (get supported-ops op :unsupported))

(defmulti -emit
  "(λ AST → Frame) → OpSeq

  Recursively translates the given AST to TEJVM bytecode, returning a
  sequence of opcodes.

  OpSeq is (Seq (Σ Op Opseq))"
  dispatch-fn)

(defn var->class
  [x]
  {:pre  [(var? x)]
   :post [(string? %)]}
  (munge (str (var->ns x) "." (var->name x))))

(defn write-class
  "(λ ClassName → Bytecode) → Nil

  Writes the given bytecode to a file named by the ClassName and
  *compile-path*. Requires that *compile-path* be set. Returns Nil."
  [classname bytecode]
  {:pre [(bound? #'clojure.core/*compile-path*)]}
  (let [rel   (s/split classname #"\.")
        class (last rel)
        rel   (butlast rel)]

    (doseq [i (range (count rel))]
      (let [f (io/file
               (str *compile-path* "/"
                    (s/join "/" (take (inc i) rel))))]

        (when-not (.exists f)
          (println "Making dir" f)
          (.mkdir f))))

    (with-open [w (java.io.FileOutputStream.
                   (str *compile-path* "/"
                        (s/join "/" rel)
                        "/" class ".class"))]
      (.write w bytecode)))
  nil)

(defn instr? [x]
  (if (or (vector? x) (seq? x))
    (let [[target & more] x]
      (keyword? target))
    false))

(defn flatten [x]
  (if (instr? x) [x]
      (mapcat flatten x)))

(def supported-ops
  #{:invoke                             ; ✓
    :instance-call                      ; ✓
    :static-call                        ; ✓
    :do                                 ; ✓
    :const                              ; ✓
    :static-field                       ; ✓
    :fn-method                          ; ✓
    :def                                ; ✓
    :fn                                 ; ✓
    })

(defn emit-pop [tag]
  (if (#{Double/TYPE Long/TYPE} tag)
    [:pop2]
    [:pop]))

(defn label
  []
  (keyword (gensym "label__")))

(defn emit-as-array
  [seq frame]
  (list
   [:push (int (count seq))]
   [:new-array :java.lang.Object]
   (mapcat (fn [i item]
             `[[:dup]
               [:push ~(int i)]
               ~@(emit item frame)
               [:array-store :java.lang.Object]])
           (range) seq)))

(defn emit-intrinsic
  [{:keys [args method ^Class class false-label]}]
  (let [m (str (.getMethod class (name method) (into-array Class (mapv :tag args))))]
    (if false-label
      (when-let [ops (intrinsic-predicate m)]
        (with-meta (conj (mapv (fn [op] [:insn op]) (butlast ops))
                         [:jump-insn (last ops) false-label])
          {:intrinsic-predicate true}))
      (when-let [ops (intrinsic m)]
        (mapv (fn [op] [:insn op]) ops)))))

(defn emit-box [tag box unchecked?]
  (if (and (primitive? tag)
           (not (primitive? box)))
    (cond
     (numeric? tag)
     [[:invoke-static [:clojure.lang.RT/box tag] :java.lang.Number]
      [:check-cast box]]

     (= Character/TYPE tag)
     [[:invoke-static [:clojure.lang.RT/box :char] :java.lang.Character]]

     (= Boolean/TYPE tag)
     [[:invoke-static [:clojure.lang.RT/box :boolean] :java.lang.Object]
      [:check-cast :java.lang.Boolean]])

    (when (primitive? box)
      (let [method (if (and (numeric? box) (or unchecked? *unchecked-math*))
                     (str "unchecked" (s/capitalize (.getName ^Class box)) "Cast")
                     (str (.getName ^Class box) "Cast"))
            tag (prim-or-obj tag)
            method-sig (str (.getMethod clojure.lang.RT method (into-array Class [tag])))]
        (if-let [ops (intrinsic method-sig)]
          (mapv (fn [op] [:insn op]) ops)
          [:invoke-static [(keyword "clojure.lang.RT" method) tag] box])))))

(defn emit-cast
  ([tag cast]
     (emit-cast tag cast false))

  ([tag cast unchecked?]
     (if (not (or (primitive? tag)
                (primitive? cast)))
       (when-not (#{Void Void/TYPE} cast)
         [:check-cast cast])
       (emit-box tag cast unchecked?))))

(defn emit
  "(λ AST) → Bytecode
  (λ AST → Options) → Bytecode

  AST is an analyzed, macroexpanded t.a.jvm AST. Options is a map, the
  following values of which are significant. Returns a (potentially
  empty) sequence of bytecodes. *classes* must be bound before calling
  emit.

  Options
  -----------
  :debug? :- (Option bool)
  Controls development debug level printing throughout code generation."

  ([ast]
     (emit ast {}))

  ([{:keys [env o-tag tag op type unchecked?] :as ast} frame]
     (let [bytecode   (-emit ast frame)
           statement? (= :ctx/statement (:context env))
           m          (meta bytecode)]
       (if statement?
         (when-not (:const m)
           (into bytecode
                 (when (and (not (:untyped m))
                            (not (:container m))
                            (not= Void/TYPE tag))
                   [(emit-pop tag)])))
         (into bytecode
               `[~(when (and (not (:container m))
                             (or (:untyped m)
                                 (= Void/TYPE tag)))
                    [:insn :ACONST_NULL])
                 ~@(when (and (not= tag o-tag)
                              (not= :const op))
                     (emit-cast o-tag tag unchecked?))])))))

(defn emit-fn-class
  [{:keys [class-name methods variadic? constants closed-overs env
           annotations super interfaces op fields class-id]
    :as ast}
   _frame]
  (let [constants          (->> constants
                                (remove #(let [{:keys [tag type]} (val %)]
                                           (or (primitive? tag)
                                               (#{:string :bool} type))))
                                (into {}))

        consts             (vals constants)

        constant-table     (zipmap (mapv :id consts) consts)

        consts             (mapv (fn [{:keys [id tag]}]
                                   {:op   :field
                                    :attr #{:public :final :static}
                                    :name (str "const__" id)
                                    :tag  tag})
                                 consts)

        ctor-types         (into (if meta [:clojure.lang.IPersistentMap] [])
                                 (mapv :tag closed-overs))

        class-ctors        [{:op     :method
                             :attr   #{:public :static}
                             :method [[:<clinit>] :void]
                             :code   `[[:start-method]
                                       [:return-value]
                                       [:end-method]]}
                            {:op     :method
                             :attr   #{:public}
                             :method `[[:<init> ~@ctor-types] :void]
                             :code   `[[:start-method]
                                       [:load-this]
                                       [:invoke-constructor [~(keyword (name super) "<init>")] :void]
                                       [:return-value]
                                       [:end-method]]}]

        variadic-method    (when variadic?
                             (let [required-arity (->> methods (filter :variadic?) first :fixed-arity)]
                               [{:op     :method
                                 :attr   #{:public}
                                 :method [[:getRequiredArity] :int]
                                 :code   [[:start-method]
                                          [:push (int required-arity)]
                                          [:return-value]
                                          [:end-method]]}]))]

    {:op          :class
     :attr        #{:public :super :final}
     :annotations annotations
     :class-name  class-name
     :name        (s/replace class-name \. \/)
     :super       (s/replace (name super) \. \/)
     :interfaces  interfaces
     :fields      consts
     :methods     (->> (concat class-ctors
                               variadic-method
                               (mapcat #(emit % _frame) methods))
                       (keep identity))}))

(defn emit-static-call
  [{:keys [fn args]} frame]
  {:pre [(= :var (:op fn))]}
  (let [v (:var fn)]
    `[~@(mapcat #(emit % frame) (take 19 args))
      ~@(when-let [args (seq (drop 19 args))]
          (emit-as-array args frame))
      [:invoke-static [~(keyword (var->class v) "invoke") ~@(repeat (min 20 (count args)) :java.lang.Object)] :java.lang.Object]]))

(defn emit-dynamic-call
  [{:keys [fn args]} frame]
  {:pre [(not (= :var (:op fn)))]}
  (list
   (emit fn frame)
   [:check-cast :clojure.lang.IFn]
   (mapcat #(emit % frame) (take 19 args))
   (when-let [args (seq (drop 19 args))]
     (emit-as-array args frame))
   [:invoke-interface [:clojure.lang.IFn/invoke ~@(repeat (min 20 (count args)) :java.lang.Object)] :java.lang.Object]))

;;--------------------------------------------------------------------

(defmethod -emit :invoke
  [{:keys [fn args] :as node} frame]
  (if (= :var (:op fn))
    (emit-static-call node frame)
    (emit-dynamic-call node frame)))

(defmethod -emit :instance-call
  [{:keys [env o-tag validated? args method ^Class class instance to-clear?]} frame]
  (if validated?
    (list
     (emit (assoc instance :tag class) frame)
     (mapcat #(emit % frame) args)
     (when to-clear?
       [[:insn :ACONST_NULL]
        [:var-insn :clojure.lang.Object/ISTORE 0]])
     [(if (.isInterface class)
        :invoke-interface
        :invoke-virtual)
      `[~(keyword (.getName class) (str method)) ~@(mapv :tag args)] o-tag])
     
     (list
      (emit instance frame)
      [:push ~(str method)]
      (emit-as-array args frame)
      (when to-clear?
        [[:insn :ACONST_NULL]
         [:var-insn :clojure.lang.Object/ISTORE 0]])
      [:invoke-static [:clojure.lang.Reflector/invokeInstanceMethod
                       :java.lang.Object :java.lang.String :objects]
       :java.lang.Object])))

(defmethod -emit :static-call
  [{:keys [env o-tag validated? args method ^Class class false-label to-clear?] :as ast} frame]
  (if validated?
    (let [intrinsic (emit-intrinsic ast)]
      (with-meta
        [(mapcat #(emit % frame) args)
         (or intrinsic
             [(when to-clear?
                [[:insn :ACONST_NULL]
                 [:var-insn :clojure.lang.Object/ISTORE 0]])
              [:invoke-static `[~(keyword (.getName class) (str method))
                                ~@(mapv :tag args)] o-tag]])]
        {:intrinsic-predicate (-> intrinsic meta :intrinsic-predicate)}))
    [[:push (.getName class)]
     [:invoke-static [:java.lang.Class/forName :java.lang.String] :java.lang.Class]
     [:push (str method)]
     (emit-as-array args frame)
     (when to-clear?
       [[:insn :ACONST_NULL]
        [:var-insn :clojure.lang.Object/ISTORE 0]])
     [:invoke-static [:clojure.lang.Reflector/invokeStaticMethod
                      :java.lang.Class :java.lang.String :objects]
      :java.lang.Object]]))

(defmethod -emit :do
  [{:keys [statements ret]} frame]
  (with-meta
    (vec (mapcat #(emit % frame) (conj statements ret)))
    {:container true}))

(defmethod -emit :const
  [{:keys [val id tag] :as ast} frame]
  (with-meta
    [(case val
       (true false)
       (if (primitive? tag)
         [:push val]
         [:get-static (if val :java.lang.Boolean/TRUE :java.lang.Boolean/FALSE)
          :java.lang.Boolean])
       
       nil
       [:insn :ACONST_NULL]
       
       (if (or (primitive? tag)
               (string? val))
         [:push (cast (or (box tag)
                          (class val))
                      val)]
         [:get-static (:class frame) (str "const__" id) tag]))]
    {:const true}))

(defmethod -emit :static-field
  [{:keys [field o-tag class env]} _frame]
  (with-meta
    [[:get-static class field o-tag]]
    {:const true}))

(defmethod -emit :fn-method
  [{:keys [params tag fixed-arity variadic? body env]}
   {:keys [class] :as frame}]
  (let [arg-tags               (mapv (comp prim-or-obj :tag) params)
        return-type            (prim-or-obj tag)
        method-name            :invoke
        [loop-label end-label] (repeatedly label)]

    ;; should emit typed only when there's an interface, otherwise it's useless

    [{:op     :method
      :attr   #{:public}
      :method [(into [method-name] arg-tags) return-type]
      :code   (flatten
               [[:start-method]
                [:local-variable :this :clojure.lang.AFunction nil loop-label end-label :this]
                (mapcat (fn [{:keys [name arg-id o-tag tag]}]
                          `[~[:local-variable name tag nil loop-label end-label name]
                            ~@(when-not (= tag o-tag)
                                [[:load-arg arg-id]
                                 [:check-cast tag]
                                 [:store-arg arg-id]])])
                        params)
                [:mark loop-label]
                (emit body
                      (assoc frame
                        :loop-label  loop-label
                        :loop-locals params))
                [:mark end-label]
                [:dup]
                [:check-cast return-type]
                [:return-value]
                [:end-method]])}]))

(defmethod -emit :fn
  [{:keys [form internal-name variadic?] :as ast}
   {:keys [class-name] :as frame}]
  (let [class-name (or class-name
                       (str (namespace-munge *ns*) "$" (munge internal-name)))
        super      (if variadic? :clojure.lang.RestFn :clojure.lang.AFunction)
        ast        (assoc ast
                     :class-name class-name
                     :super super)]
    (emit-fn-class ast frame)))

(defmethod -emit :def
  [{:keys [init var]} frame]
  (->> (var->class var)
       munge
       (assoc frame :class-name)
       (emit init)))

(defmethod -emit :unsupported
  [{:keys [op] :as node} frame]
  (assert false
          (str "Failed to emit op " op)))

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
  [whole-program {:keys [entry] :as options}]
  {:pre  [(symbol? entry)]}
  (let [whole-program (-> whole-program
                          (require-pass tree-shake          options)
                          (require-pass reduce-fn-arities   options)
                          (require-pass tree-shake          options)
                          (require-pass analyze-var-uses    options))
        reach         (-> whole-program (get :reach-map) (get (resolve entry)))
        reach-defs    (->> whole-program
                           whole-ast->forms
                           (filter #(and (pattern/def? %)
                                         (reach (pattern/def->var %)))))]

    (println (get whole-program :reach-map))

    (println reach)

    (conj
     (mapv #(emit %1 {}) reach-defs)
     (let [class-name (namespace entry)
           class-ast  {:op          :class
                       :attr        #{:public :super :final}
                       :class-name  class-name
                       :super       :java.lang.Object
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
                                      :code   `[[:start-method]
                                                [:invoke-static [:clojure.lang.RT/seq :java.lang.Object] :clojure.lang.Seq] ;; seq the array
                                                [:new-instance ~(var->class (resolve entry))]
                                                [:invoke-constructor [~(keyword (var->class (resolve entry)) "<init>")] :void]
                                                [:invoke-interface [:clojure.lang.IFn/applyTo :clojure/lang/ISeq] :java.lang.Object]
                                                [:pop]
                                                [:return-value] ;; stack must be empty I guess...
                                                [:end-method]]}]}]
       class-ast))))

(defn emit-program
  "(λ Whole-AST → Options) → Nil

  Emits a whole program with reference to a single emtry point as
  named by the argument var, writing class files for side-effects.

  Options
  -----------
  `:entry'
    a Symbol, being the var with reference to which the entire program
    will be emitted."
  [whole-program options]
  (doseq [{:keys [class-name] :as c}
          (emit-classes whole-program options)]
    (->> c -compile (write-class class-name)))
  nil)

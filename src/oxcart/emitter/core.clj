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
  (:require [clojure.string :as s]
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
             :refer [primitive? prim-or-obj box] :as j.u]
            [clojure.tools.emitter.jvm.emit
             :refer [emit-line-number
                     emit-constants]]
            [clojure.tools.emitter.jvm.intrinsics
             :refer [intrinsic intrinsic-predicate]]))

(defn var->class [x]
  {:pre  [(var? x)]
   :post [(string? %)]}
  (str (var->ns x) "." (var->name x)))

(defn write-class
  "(λ ClassName → Bytecode) → Nil

  Writes the given bytecode to a file named by the ClassName and
  *compile-path*. Requires that *compile-path* be set. Returns Nil."
  [classname bytecode]
  {:pre [(bound? *compile-path*)]}
  (let [rel   (s/split classname #"\.")
        class (last rel)
        rel   (butlast rel)]
    (-> (io/file (str *compile-path* "/"
                      (s/join "/" rel))
                 (str class ".class"))
        (spit bytecode)))
  nil)

(def supported-ops
  #{:instance-call                      ; ✓
    :static-call                        ; ✓
    :do                                 ; ✓
    :const                              ; ✓
    :static-field                       ; ✓
    :fn-method                          ; ✓
    :def                                ; ✓
    :fn                                 ; ✓
    })

(defn dispatch-fn
  [{:keys [op] :as node} _frame]
  (get supported-ops op :unsupported))

(defmulti -emit
  "(λ AST → Frame) → OpSeq

  Recursively translates the given AST to TEJVM bytecode, returning a
  sequence of opcodes.

  OpSeq is (Seq (Σ Op Opseq))"
  dispatch-fn)

(defn label []
  (keyword (gensym "label__")))

(defn emit-as-array [list frame]
  `[[:push ~(int (count list))]
    [:new-array :java.lang.Object]
    ~@(mapcat (fn [i item]
                `[[:dup]
                  [:push ~(int i)]
                  ~@(-emit item frame)
                  [:array-store :java.lang.Object]])
              (range) list)])

(defn emit-intrinsic [{:keys [args method ^Class class false-label]}]
  (let [m (str (.getMethod class (name method) (into-array Class (mapv :tag args))))]
    (if false-label
      (when-let [ops (intrinsic-predicate m)]
        (with-meta (conj (mapv (fn [op] [:insn op]) (butlast ops))
                         [:jump-insn (last ops) false-label])
          {:intrinsic-predicate true}))
      (when-let [ops (intrinsic m)]
        (mapv (fn [op] [:insn op]) ops)))))

;;--------------------------------------------------------------------

(defmethod -emit :instance-call
  [{:keys [env o-tag validated? args method ^Class class instance to-clear?]} frame]
  (if validated?
    `[~@(emit-line-number env)
      ~@(-emit (assoc instance :tag class) frame)
      ~@(mapcat #(-emit % frame) args)
      ~@(when to-clear?
           [[:insn :ACONST_NULL]
            [:var-insn :clojure.lang.Object/ISTORE 0]])
      [~(if (.isInterface class)
          :invoke-interface
          :invoke-virtual)
       [~(keyword (.getName class) (str method)) ~@(mapv :tag args)] ~o-tag]]
    `[~@(-emit instance frame)
      [:push ~(str method)]
      ~@(emit-as-array args frame)
      ~@(when to-clear?
          [[:insn :ACONST_NULL]
           [:var-insn :clojure.lang.Object/ISTORE 0]])
      [:invoke-static [:clojure.lang.Reflector/invokeInstanceMethod
                       :java.lang.Object :java.lang.String :objects]
       :java.lang.Object]]))

(defmethod -emit :static-call
  [{:keys [env o-tag validated? args method ^Class class false-label to-clear?] :as ast} frame]
  (if validated?
    (let [intrinsic (emit-intrinsic ast)]
      `^{:intrinsic-predicate ~(-> intrinsic meta :intrinsic-predicate)}
      [~@(emit-line-number env)
       ~@(mapcat #(-emit % frame) args)
       ~@(or intrinsic
             `[~@(when to-clear?
                   [[:insn :ACONST_NULL]
                    [:var-insn :clojure.lang.Object/ISTORE 0]])
               [:invoke-static [~(keyword (.getName class) (str method))
                                ~@(mapv :tag args)] ~o-tag]])])
    `[[:push ~(.getName class)]
      [:invoke-static [:java.lang.Class/forName :java.lang.String] :java.lang.Class]
      [:push ~(str method)]
      ~@(emit-as-array args frame)
      ~@(when to-clear?
          [[:insn :ACONST_NULL]
           [:var-insn :clojure.lang.Object/ISTORE 0]])
      [:invoke-static [:clojure.lang.Reflector/invokeStaticMethod
                       :java.lang.Class :java.lang.String :objects]
       :java.lang.Object]]))

(defmethod -emit :do
  [{:keys [statements ret]} frame]
  (with-meta
    (vec (mapcat #(-emit % frame) (conj statements ret)))
    {:container true}))

(defmethod -emit :const
  [{:keys [val id tag] :as ast} frame]
  ^:const
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
       [:push (cast (or (box tag) (class val)) val)]
       [:get-static (:class frame) (str "const__" id) tag]))])

(defmethod -emit :static-field
  [{:keys [field o-tag class env]} _frame]
  `^:const
  [~[:get-static class field o-tag]])

(defmethod -emit :fn-method
  [{:keys [params tag fixed-arity variadic? body env]}
   {:keys [class] :as frame}]
  (let [arg-tags               (mapv (comp prim-or-obj :tag) params)
        return-type            (prim-or-obj tag)
        tags                   (conj arg-tags return-type)
        prim-interface         (j.u/prim-interface tags)

        primitive?             (some primitive? tags)

        method-name            (cond
                                variadic? :doInvoke
                                primitive? :invokePrim
                                :else
                                :invoke)

        ;; arg-types
        [loop-label end-label] (repeatedly label)

        code
        `[[:start-method]
          [:local-variable :this :clojure.lang.AFunction nil ~loop-label ~end-label :this]
          ~@(mapcat (fn [{:keys [name arg-id o-tag tag]}]
                      `[~[:local-variable name tag nil loop-label end-label name]
                        ~@(when-not (= tag o-tag)
                            [[:load-arg arg-id]
                             [:check-cast tag]
                             [:store-arg arg-id]])])
                  params)
          [:mark ~loop-label]
          ~@(emit-line-number env loop-label)
          ~@(-emit body
                   (assoc frame
                     :loop-label  loop-label
                     :loop-locals params))
          [:mark ~end-label]
          [:return-value]
          [:end-method]]]

    ;; should emit typed only when there's an interface, otherwise it's useless

    `[{:op     :method
       :attr   #{:public}
       :method [(into [method-name] arg-tags) return-type]
       :code   code}
      (when primitive?
        {:op        :method
         :attr      #{:public}
         :interface prim-interface
         :method    [(into [:invoke] (repeat (count params) :java.lang.Object))
                     :java.lang.Object]
         :code      `[[:start-method]
                      [:load-this]
                      ~@(mapcat (fn [{:keys [tag]} id]
                                  `[~[:load-arg id]
                                    ~@(emit-cast Object tag)])
                                params (range))
                      ~[:invoke-virtual (into [(keyword class "invokePrim")] arg-tags) return-type]
                      ~@(emit-cast return-type Object)
                      [:return-value]
                      [:end-method]]})]))

(defmethod -emit :def
  [{:keys [init var]} frame]
  (->> (var->class var)
       (assoc frame :class-name)
       (-emit init)))

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
     :methods     (concat class-ctors
                          variadic-method
                          (mapcat #(-emit %) methods))}))

(defmethod -emit :fn
  [{:keys [form internal-name variadic?] :as ast}
   _frame]
  (let [class-name (str (namespace-munge *ns*) "$" (munge internal-name))
        super      (if variadic? :clojure.lang.RestFn :clojure.lang.AFunction)
        ast        (assoc ast
                     :class-name class-name
                     :super super)]
    (emit-fn-class ast)))

(defn emit-class
  "(λ Def-AST) → Class-AST

  Transforms a def to a class, returning a TEJVM AST describing the
  resulting class."
  [ast _frame]
  {:pre [(pattern/def? ast)]}
  )

(defn emit
  "(λ Whole-AST → Options) → nil

  Emits a whole program with reference to a single emtry point as
  named by the argument var, writing class files for side-effects.

  Options
  -----------
  `:entry'
    a Var, being the var with reference to which the entire program
    will be emitted."
  [whole-program {:keys [entry] :as options}]
  {:pre  [(var? entry)]
   :post [(nil? %)]}
  (let [whole-program (-> whole-program
                          (require-pass tree-shake          options)
                          (require-pass reduce-fn-arities   options)
                          (require-pass tree-shake          options)
                          (require-pass analyze-var-uses    options)
                          (require-pass locate-var-as-value options))
        reach         (-> whole-program (get :var-reached) (get entry))
        reach-defs    (->> whole-program
                           whole-ast->forms
                           (keep #(and (pattern/def? %)
                                       (reach (pattern/def->var %)))))]

    ;; compile & write the set of used classes
    (doseq [ast reach-defs]
      (let [{:keys [class-name] :as class-ast} (emit-class ast emit)
            class-bc                           (-compile class-ast)]
        (write-class class-name class-bc)))

    ;; compile & write the bootstrap class
    (let [class-name (var->ns entry)
          class-ast  {} ;; FIXME
                        ;;   This is something that I'm gonna need to
                        ;;   build by hand methinks.
          class-bc   (-> class-ast -compile)]
      (assert (not (= class-ast {}))
              "FIXME: not implemented yet!")
      (write-class class-name class-bc)))
  nil)

;;   Copyright (c) Reid McKenzie, Rich Hickey & contributors. The use
;;   and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0
;;   (http://opensource.org/licenses/eclipse-1.0.php) which can be
;;   found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be
;;   bound by the terms of this license.  You must not remove this
;;   notice, or any other, from this software.

(ns oxcart.passes.warnings
  {:doc "Implements static error detection passes for Oxcart."
   :author "Reid McKenzie"
   :added "0.0.7"}
  (:require [clojure.set :as set]
            [clojure.set :refer [intersection]]
            [clojure.tools.analyzer.ast :as ast]
            [oxcart
             [passes :refer :all]
             [util :as util]
             [pattern :as pattern]]
            [oxcart.passes.tree-shake :refer [analyze-var-dependencies]]
            [taoensso.timbre :refer [warn error]]))


(def banned-vars
  "Vars which Oxcart does not support at all.

  The use of these vars is a compile time failure, however all
  failures should be reported before compilation is aborted."

  #{#'clojure.core/eval
    #'clojure.core/read-string
    #'clojure.core/alter-var-root})


(def dangerous-vars
  "Vars which Oxcart does not currently support.

  The use of these vars is a compile time failure, however all
  failures should be reported before compilation is aborted."

  ;; FIXME:
  ;;   Anything that I can compile I which don't want to support yet?
  ;;   I think the answer is no, most things which I can't compile are
  ;;   just banned but this is here in case I think of something
  ;;   later.
  #{;; dynamic bindings stuff is evil
    #'clojure.core/push-thread-bindings
    #'clojure.core/pop-thread-bindings
    #'clojure.core/get-thread-bindings

    ;; namespace introspection stuff is pretty sketch too
    #'clojure.core/ns-resolve
    #'clojure.core/find-var})


(defn check-vars
  "λ Whole-AST → options → Whole-AST

  Emits the appropriate error messages when banned or unsupported vars
  are used by input program.

  options:
    :suppress-errors
    if true the compiler will emit warnings and errors normally but will
    continue compilation despite having encountered errors. Using this
    flag is a warning source, as resulting bytecode is untrustworthy at
    best.

    :warnings-as-errors
    if true rather than emitting a warning and continuing a pass
    emitting warnings will collect and emit all warnings and then abort
    compilation. Note that this flag _ignores_ :suppress-errors.

    :banned-set
    #{} by default, a set of vars which the user may blacklist
    arbitrarily. These vars are considered when checking for banned vars
    along with compiler banned vars. Note that the user may not
    whitelist vars which the compiler has blacklisted. This is a
    correctness feature.

    :warn-unsupported
    true by default, if true a check is made to see if the program
    accesses vars which are stated to be unsupported."
  [whole-ast
   {:keys [warnings-as-errors
           suppress-errors
           banned-set
           warn-unsupported]
    :or {warnings-as-errors false
         suppress-errors    false
         banned-set         #{}
         warn-unsupported   true}
    :as options}]
   (let [whole-ast (-> whole-ast
                       (require-pass analyze-var-dependencies
                                     options))
         reach-map (:reach-map whole-ast)
         dep-map   (:dependency-map whole-ast)
         forms     (whole-ast->forms whole-ast)
         error?    (atom false)]

     ;; do a single pass over the forms
     (doseq [ast forms]
       (if-let [var (pattern/def->var ast)]
         (cond (contains? banned-vars var)
               ;; outright banned vars
               (do (when-not suppress-errors
                     (reset! error? true))
                   (error (str (util/format-line-info ast)
                               ", Program makes use of banned var:"
                               var)))

              (contains? banned-set var)
              ;; user banned vars
              (do (when-not suppress-errors
                    (reset! error? true))
                  (error (str (util/format-line-info ast)
                              ", Program makes use of user banned var:"
                              var)))

              (and (contains? dangerous-vars var)
                   warn-unsupported)
              ;; unsupported vars
              (do (when warnings-as-errors
                    (reset! error? true))
                  (warn (str (util/format-line-info ast)
                             ", Program makes use of flagged var:"
                             var))))))

     (assert (not @error?)
             "Errors were generated in checking.")
     (-> whole-ast
         (record-pass check-vars))))


(def banned-ops
  "AST operations which are banned due to their violation of Oxcart's
  static assumptions."
  #{})


(def dangerous-ops
  "Supported AST operations which are dangerous, but not always illegal."
  #{:set!})


(defn check-ops
  "λ Whole-AST → options → Whole-AST

  Traverses the argument AST on a form by form basis to make sure that
  no blacklisted operations are used.

  options:
    :suppress-errors
    if true the compiler will emit warnings and errors normally but will
    continue compilation despite having encountered errors. Using this
    flag is a warning source, as resulting bytecode is untrustworthy at
    best.

    :warnings-as-errors
    if true rather than emitting a warning and continuing a pass
    emitting warnings will collect and emit all warnings and then abort
    compilation. Note that this flag _ignores_ :suppress-errors."
  [whole-ast
   {:keys [suppress-errors
           warnings-as-errors]
    :or   {suppress-errors    false
           warnings-as-errors false}
    :as options}]
  (let [error?   (atom false)]
    (doseq [ast (whole-ast->forms whole-ast)]
      (let [ops (->> ast
                     ast/children
                     (map :op)
                     (into #{}))]
        (cond (not (empty? (intersection banned-ops ops)))
              (do (when-not suppress-errors
                    (reset! error? true))
                  (error (util/format-line-info ast)
                         ", Program makes use of unsupported op!"))

              (not (empty? (intersection dangerous-ops ops)))
              (do (when-not suppress-errors
                    (reset! error? true))
                  (warn (util/format-line-info ast)
                        ", Program makes use of dangerous op!")))))

    (assert (not @error?)
            "Errors were generated in checking."))
  (-> whole-ast
      (record-pass check-ops)))

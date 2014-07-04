;;   Copyright (c) Reid McKenzie, Rich Hickey & contributors. The use
;;   and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0
;;   (http://opensource.org/licenses/eclipse-1.0.php) which can be
;;   found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be
;;   bound by the terms of this license.  You must not remove this
;;   notice, or any other, from this software.

(ns oxcart.core-redefs
  {:doc "Provides alternative implementations of clojure.core
        functions and macros used by the Oxcart compiler to handle
        parts of the Clojure core which are otherwise difficult to
        analyze and emit such as 

        - clojure.core/ns,
        - clojure.core/defmulti
        - clojure.core/defmethod

        These alternative implementations are _not_ suitable for
        general use and are implementation details of the Oxcart
        compiler family. Nasal deamons abound here."
   :author "Reid McKenzie"
   :added "0.0.12"}
  (:require [oxcart.util :as util]
            [oxcart.vars :refer :all])
  (:refer-clojure :exclude [ns *loaded-libs* defmulti defmethod
                            deftype defprotocol extend-type
                            extend-protocol proxy]))


;; FIXME:
;;   This is currently dead code. Do I need a custom implementation of
;;   ns? I don't think I do but I may... at present rather than using
;;   a custom ns eval simply drops forms from ns. This may or may not
;;   be a problem, so the below is retained.

(defmacro oxcart.core-redefs/ns
  "Oxcart's alternative implementation of the ns macro. See
  clojure.core/ns for docs.

  Differences:
  ---------------
  - http://p.hagelb.org/egalitarian-ns.html
    This patch allows ns to use alternate implementations of require
    or other loading functions by providing a namespace qualified
    keyword.

  - Reverts fed833d8a117ea137c008bde10dd011d3d7c2b97,
    dbb85aafafc90a9b6021ef58c5cd4bd8a20600fc.
    These two paches changed the behavior of loading a new namespace
    so that loading would occur in a child classloader rather than in
    the Clojure core classloader. While this is arguably a good thing
    from a loading modularity standpoint, the loading context of the
    wrapper function defeats Oxcart's requirement that load operations
    occur at the top level.

  - As an implementation detail and for clarity leverages
    oxcart.util/take-when rather than having repeated let
    statements. This should have no user visible impact."

  {:arglists '([name docstring? attr-map?  references*])}
  [name & references]
  (let [process-reference      (fn [[kname & args]]
                                 (when-let [clause-ns (namespace kname)]
                                   (clojure.core/require (clojure.core/symbol clause-ns)))
                                 `(~(symbol (or (clojure.core/namespace kname) "clojure.core")
                                            (clojure.core/name kname))
                                   ~@(map #(list 'quote %) args)))
        [docstring references] (util/take-when string? references)
        name                   (if docstring
                                 (vary-meta name assoc :doc docstring)
                                 name)
        [metadata references]  (util/take-when map? references)
        name                   (if metadata
                                 (vary-meta name merge metadata)
                                 name)
        gen-class-clause       (first (filter #(= :gen-class (first %)) references))
        gen-class-call         (when gen-class-clause
                                 (list* `gen-class :name (.replace (str name) \- \_) :impl-ns name :main true (next gen-class-clause)))
        references             (remove #(= :gen-class (first %)) references)
        ]
    `(do
       (clojure.core/in-ns '~name)
       ~(when gen-class-call gen-class-call)
       ~(when (and (not= name 'clojure.core)
                   (not-any? #(= :refer-clojure (first %)) references))
          `(clojure.core/refer '~'clojure.core))
       ~@(map process-reference references)
       ~(when-not (.equals name 'clojure.core)
          `(do (clojure.core/dosync
                (clojure.core/commute
                 @#'clojure.core/*loaded-libs*
                 clojure.core/conj '~name)))))))


;; FIXME
;;
;;   Do I need to do crazy stuff to rewrite defrecord, defprotocol,
;;   defmulti and the related class generation operations? These
;;   functions currently dynamically generate classes which are
;;   injected into the hot runtime. It's possible that Oxcart could
;;   try to capture these classes and emit them AOT (which I think
;;   clojure.core/compile can do) but that's out of scope for the
;;   moment.


(defmacro oxcart.core-redefs/defmulti
  [name & _]
  (assert false "defmulti is not supported by Oxcart"))

(defmacro oxcart.core-redefs/defmethod
  [& _]
  (assert false "defmethod is not supported by Oxcart"))

(defmacro oxcart.core-redefs/deftype
  [& _]
  (assert false "deftype is not supported by Oxcart"))

(defmacro oxcart.core-redefs/defprotocol
  [& _]
  (assert false "defprotocol is not supported by Oxcart"))

(defmacro oxcart.core-redefs/proxy
  [& _]
  (assert false "proxy is not supported by Oxcart"))

(defmacro oxcart.core-redefs/extend-type
  [& _]
  (assert false "extend-type is not supported by Oxcart"))

(defmacro oxcart.core-redefs/extend-protocol
  [& _]
  (assert false "extend-protocol is not supported by Oxcart"))

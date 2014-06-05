;;   Copyright (c) Reid McKenzie, Rich Hickey & contributors. The use
;;   and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0
;;   (http://opensource.org/licenses/eclipse-1.0.php) which can be
;;   found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be
;;   bound by the terms of this license.  You must not remove this
;;   notice, or any other, from this software.

(ns oxcart.test-util
  (:refer-clojure :exclude [macroexpand-1 macroexpand])
  (:require [clojure.test :refer :all]
            [clojure.tools.analyzer.jvm :as ana.jvm]
            [clojure.tools.analyzer
             :refer [macroexpand-1
                     macroexpand]
             :as ana]))

(defmacro is-not [form]
  `(is (not ~form)))

(defmacro ast [mform]
  `(binding [ana/macroexpand-1 ana.jvm/macroexpand-1
             ana/create-var    ana.jvm/create-var
             ana/parse         ana.jvm/parse
             ana/var?          var?]
     (ana.jvm/analyze (quote ~mform)
                      (ana.jvm/empty-env))))

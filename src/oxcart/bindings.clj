(ns oxcart.bindings)


;; Bindings
;; ------------
;;
;; Bindings will be tracked using records, being of the form
;;
;; {:link     → Binding
;;  :bindings → {Symbol → form}
;; }


(defn bind
  "λ Binding → Symbol → Binding
   λ Binding → Symbol → AST → Binding

  Adds a sybol to the given binding frame, returning the updated
  binding frame. If no AST is provided then a nil tree will be used."
  ([binding symbol]
     (bind binding symbol nil))

  ([binding symbol val]
     (-> binding
         (update-in [:bindings] assoc symbol val))))


;; If link is not nil, then link points "up" the binding expression
;; stack to the previous binding environment. At the top level
;; obviously the parent is nil thus a trivial recursive search upwards
;; for the first preceeding binding environment is correct when
;; attempting to locate a symbol.


(defn depth
  "λ Binding → Int

  Computes the link tree depth of a given binding tree by performing
  the trivial linked list length measurement. Expensive but called for
  sometimes."
  [binding]
  (loop [{:keys [link] :as b} binding
         d 0]
    (if-not link d
      (recur link (inc d)))))


(defn get-level
  "λ Binding → Symbol → (Maybe Int)

  Searches back up a binding stack, returning the get-binding-depth of the
  frame which first binds a given symbol."
  [{:keys [link bindings] :as binding-stack} symbol]
  (if (contains? bindings symbol)
    (depth binding-stack)
    (when link
      (recur link symbol))))


(defn get-value
  "λ Binding → Symbol → (Maybe AST)"
  [{:keys [link bindings] :as binding-stack} symbol]
  (or (get bindings symbol)
      (when link
        (recur link symbol))))



(defn push-bindings [bindings]
  {:link bindings :bindings {}})


(defn pop-bindings [bindings]
  (:link bindings))

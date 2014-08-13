(ns oxcart.logging)

;; FATAL    : 5
;; ERROR    : 4
;; WARN     : 3
;; MESSAGE  : 2
;; INFO     : 1
;; DEBUG    : 0

(def ^:dynamic *log-level* -1)

(defn debug [& args]
  (when (>= 0 *log-level*)
    (println "[DEBUG   ]" (apply str args))))

(defn info [& args]
  (when (>= 1 *log-level*)
    (println "[INFO    ]" (apply str args))))

(defn message [args]
  (when (>= 2 *log-level*)
    (println "[MESSAGE ]" (apply str args))))

(defn warn [args]
  (when (>= 3 *log-level*)
    (println "[WARN    ]" (apply str args))))

(defn error [args]
  (when (>= 4 *log-level*)
    (println "[ERROR   ]" (apply str args))))

(defn fatal [args]
  (when (>= 5 *log-level*)
    (println "[FATAL   ]" (apply str args))))

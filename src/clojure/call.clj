(ns call
  (:gen-class))

(defn vcall ^long [^long i]
  (inc i))

(defn bench [^long reps]
  (loop [i 0
         accum 0]
    (when (< i reps)
      (recur (inc i) (vcall accum)))))

(defn -main []
  (dotimes [_ 10]
    (time (bench 10000000))))

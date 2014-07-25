(ns hello)

(defn -main []
   (. (. System out)
      (println "Hello, World!"))
   (. System (exit (int 0))))

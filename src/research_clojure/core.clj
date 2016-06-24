(ns research-clojure.core)
(require ['clojure.string :as 'str])

;Used to read in the sentence file
;Each line is an element
(defn read-file [file]
  (str/split-lines (slurp file)))


(defn -main
  "I don't do a whole lot."
  []
  (println "Hello, World!")
  ;(def x (read-file "tests.txt"))
  (def x (str/split "543  dfsfsdfds" #" "))
  (println x)
  (println (get x 1))
  (println (get x 0))
  (println (get x 2)))


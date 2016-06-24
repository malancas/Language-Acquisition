(ns research-clojure.core)
(require ['clojure.string :as 'str])
(require '[clojure.data.csv :as csv])

;Used to read in the sentence file
;Creates a vector of vectors
(defn readFile
  [file]
   (with-open [rdr (clojure.java.io/reader file)] (vec (csv/read-csv rdr :separator \tab))))

(defn isSentenceNeeded
  [x gID]
  (println (get x 0))
  (= gID (get x 0)))

(defn chooseSentences
  [file gID]
  (loop [remainingSentences file chosenSentences []]
    (if (empty? remainingSentences)
      chosenSentences
      (let [[sentence & remaining] remainingSentences]
        (recur remaining
               (into chosenSentences
                     (if (isSentenceNeeded sentence gID)
                       (set [sentence]))))))))



(defn -main
  "I don't do a whole lot."
  []
  (println "Load file from memory")
  (def allSentences (readFile "tests.txt"))
  ;(println (get (get allSentences 0) 0))
  (def selectedSentences (chooseSentences allSentences "611"))
  (println (count selectedSentences))
  (println (get selectedSentences 0))
  )



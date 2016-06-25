(ns research-clojure.core)
(require ['clojure.string :as 'str])
(require '[clojure.data.csv :as csv])

;Used to read in the sentence file
;Creates a vector of vectors
(defn readFile
  [file]
   (with-open [rdr (clojure.java.io/reader file)] (vec (csv/read-csv rdr :separator \tab))))

;Returns whether the sentences grammar ID
;matches gID
(defn isSentenceNeeded
  [x gID]
  (println (get x 0))
  (= gID (get x 0)))

;Iterates through the file containing all sentences
;and adds sentences with grammar IDs matching gID
;to a new vector
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


(defn func2
  [x]
  (assoc x 1 "2"))



(defn -main
  "Loads the sentence file contents int to
  memory and creates a file of selected sentences
  based on a chosen grammar ID"
  []
  (def x ["2" "1" "0"])
  (println x)
  (def y (func2 x))
  (println y)

  (println "Load file from memory")
  (def allSentences (readFile "tests.txt"))
  (println (get (get allSentences 0) 0))
  (def selectedSentences (chooseSentences allSentences "611"))
  (println (count selectedSentences))
  (println (get selectedSentences 0)))



(ns research-clojure.core)
(require 'research-clojure.eChild.runSimulation)
(refer 'research-clojure.eChild.runSimulation)
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
  (= gID (get x 0)))


(defn chooseSentences
  "Iterates through sentences, containing all sentences,
  and adds sentences with grammar IDs matching gID
  to a new vector"
  [sentences gID]
  (loop [remainingSentences sentences chosenSentences []]
    (if (empty? remainingSentences)
      chosenSentences
      (let [[sentence & remaining] remainingSentences]
        (recur remaining
               (into chosenSentences
                     (if (isSentenceNeeded sentence gID)
                       (set [sentence]))))))))


(defn -main
  "Loads the sentence file contents int to
  memory and creates a file of selected sentences
  based on a chosen grammar ID"
  []

  (println "Load file from memory")
  (def allSentences (readFile "EngFrJapGerm.txt"))
  ;Sentences with the grammar ID 611 are copied into
  ;the selectedSentences vector
  (def selectedSentences (chooseSentences allSentences "611"))
  (runSimulation selectedSentences 100 1000))




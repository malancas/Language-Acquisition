(ns research-clojure.core)
(require ['clojure.string :as 'str])
(require '[clojure.data.csv :as csv])

;Used to read in the sentence file
;Creates a vector of vectors
(defn readFile
  [file]
   (with-open [rdr (clojure.java.io/reader file)] (vec (csv/read-csv rdr :separator \tab))))

(defn chooseSentence
  [x gID]
  (if (= gID (get x 0))
    ))

(defn chooseSelectSentences
  [file gID]
  (map (chooseSentence gID) file))

(defn -main
  "I don't do a whole lot."
  []
  (println "Load file from memory")
  (def allSentences (readFile "tests.txt"))
  (def selectedSentences (chooseSelectSentences allSentences "611")))



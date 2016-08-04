(ns research-clojure.core)
(require 'research-clojure.eChild.runSimulation)
(refer 'research-clojure.eChild.runSimulation)
(require '[clojure.data.csv :as csv])


(defn readFile
  "Used to read in the sentence file. Creates a vector of vectors"
  [file]
  (with-open [rdr (clojure.java.io/reader file)] (vec (csv/read-csv rdr :separator \tab))))


(defn chooseSentences
  "Iterates through sentences, containing all sentences,
  and adds sentences with grammar IDs matching gID
  to a new vector.
  French=584, English=611, German=2253, Japanese=3856"
  [sentences gID]
  (loop [remainingSentences sentences chosenSentences []]
    (if (empty? remainingSentences)
      chosenSentences
      (let [[sentence & remaining] remainingSentences]
        (recur remaining
               (into chosenSentences
                    ;If the grammar ID in sentence is
                    ;equal to gID, then sentence is added
                    ;to chosenSentences
                    (if (= (get sentence 0) gID) 
                      (set [sentence]))))))))


(defn -main
  "Loads the sentence file contents int to
  memory and creates a file of selected sentences
  based on a chosen grammar ID"
  [& args]

  (let [arguments (into [] (map #(Integer/parseInt %) args))] 
    (if (not= 3 (count arguments))
      (do (println "Not enough arguments entered at command line")
          (System/exit 0))
      (if (not (and (>= (get arguments 0) 0) (>= (get arguments 1) 0) (>= (get arguments 2) 0)))
        (println "All arguments must be integers equal or greater than zero")
        (do (println "Load file from memory")

            (let [allSentences (readFile "EngFrJapGerm.txt")]

              ;Sentences with the grammar ID 611 are copied into
              ;the selectedSentences vector
              (let [selectedSentences (chooseSentences allSentences (str (get arguments 2)))]
                (if (empty? selectedSentences)
                  (println "There are no sentences with the selected grammarID")
                  (runSimulation selectedSentences (get arguments 0) (get arguments 1))))))))))
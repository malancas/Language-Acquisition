(ns research-clojure.eChild.runSimulation)
(require 'research-clojure.eChild.Child)
(refer 'research-clojure.eChild.Child)
(require '[clojure.string :as str]
         '[clojure.data.csv :as csv]
         '[clojure.java.io :as io]
         '[clojure.set :refer [union]])


(defn writeResults
  "Writes the results of doesChildLearnGrammar? to output file"
  [lines]
  (with-open [out-file (io/writer "out.csv" :append true)]
            (csv/write-csv out-file [[(get lines 0) (get lines 1)]])))


(defn doesChildLearnGrammar?
  "Runs sentence consuming functions until the 
  correct grammar is learned or the maximum number
  of sentences are processed. Needs infoList, grammar, expected grammar"
  [sentences max_num]

  (let [grammarLearned (atom false)
        sentenceCount (atom 0)
        grammar (atom [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1])]

        (while (and (not @grammarLearned) (< @sentenceCount max_num))
          (let [infoList1 (consumeSentence (rand-nth sentences))]
            (reset! grammar (setParameters infoList1 grammar))
            (reset! grammarLearned (isGrammarLearned? grammar infoList1)))
          (swap! sentenceCount inc))

        (println "Final grammar: " @grammar)
        (writeResults [@grammar @grammarLearned])))


(defn runSimulation
  "max_eChildren number of eChildren will process 
  max_sentences number of sentences."
  [sentences max_eChildren max_sentences]
  (io/delete-file "out.csv" :silently true)

  (let [counter (atom 0)]
    (while (< @counter max_eChildren)
      (println "eChild #" @counter)
      (doesChildLearnGrammar? sentences max_sentences)
      (swap! counter inc))))


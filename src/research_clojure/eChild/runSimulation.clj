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
  (with-open [out-file (io/writer "results.csv")]
  (csv/write-csv out-file
                 lines)))


(defn doesChildLearnGrammar?
  "Runs sentence consuming functions until the 
  correct grammar is learned or the maximum number
  of sentences are processed. Needs infoList, grammar, expected grammar"
  [sentences max_num]
  (def grammarLearned (atom false))
  (def sentenceCount (atom 0))

  (def grammar [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1])
  (while (and (not @grammarLearned) (< @sentenceCount max_num))
    (def infoListAndSentence (consumeSentence (rand-nth sentences)))
    (def currGrammarAndState (setParameters (get infoListAndSentence 0) grammar))
    (swap! grammarLearned (isGrammarLearned? (get currGrammarAndState 1)))
    (swap! sentenceCount inc))
  (writeResults currGrammarAndState))


(defn runSimulation
  [sentences max_eChildren max_sentences]
  (def count (atom 0))
  (while (< @count max_eChildren)
    (doesChildLearnGrammar? sentences max_sentences)
    (swap! count inc)))


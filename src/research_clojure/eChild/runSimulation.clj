(ns research-clojure.eChild.runSimulation
  (:require [research-clojure.eChild.Child :refer :all]
            ['clojure.string :as 'str]))


(defn writeResults [lines]
 ;need to be rewritten
)


(defn doesChildLearnGrammar?
  "Runs sentence consuming functions until the 
  correct grammar is learned or the maximum number
  of sentences are processed. Needs infoList, grammar, expected grammar"
  [sentences max_num]
  (def grammarLearned (atom false))
  (def sentenceCount (atom 0))

  (while (and (not @grammarLearned) (< @sentenceCount max_num))
    (def infoListAndState (consumeSentence (rand-nth sentences)))
    (def eChildData (conj eChildData [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1]))
    (def currGrammarAndState (setParameters (get eChildData 0) (get eChildData 2)))
    (swap! grammarLearned (isGrammarLearned? (get currGrammarAndState 1)))
    (swap! sentenceCount inc)) 
  (writeResults currGrammarAndState))


(defn runSimulation
  [sentences max_eChildren max_sentences]
  (def count (atom 0))
  (while (< @count max_eChildren)
    (doesChildLearnGrammar? sentences max_sentences)
    (swap! count inc)))


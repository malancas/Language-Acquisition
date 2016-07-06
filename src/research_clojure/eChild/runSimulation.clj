(ns research-clojure.eChild.runSimulation)
(require 'research-clojure.eChild.Child)
(refer 'research-clojure.eChild.Child)
(require ['clojure.string :as 'str])


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

  (def eChildData [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1])
  (while (and (not @grammarLearned) (< @sentenceCount max_num))
    (conj eChildData (consumeSentence (rand-nth sentences)))
    (def currGrammarAndState (setParameters (get eChildData 1) (get eChildData 0)))
    (swap! grammarLearned (isGrammarLearned? (get currGrammarAndState 1)))
    (swap! sentenceCount inc))
  ;(writeResults currGrammarAndState)
  )

(defn runSimulation
  [sentences max_eChildren max_sentences]
  (def count (atom 0))
  (while (< @count max_eChildren)
    (doesChildLearnGrammar? sentences max_sentences)
    (swap! count inc)))


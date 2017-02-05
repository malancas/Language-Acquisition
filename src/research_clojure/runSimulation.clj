(load "Child")
(require '[clojure.string :as str]
         '[clojure.data.csv :as csv]
         '[clojure.java.io :as io]
         '[clojure.set :refer [union]])


(defn writeResults
  "Writes the results of each learner to an output file"
  [results]
  (println "Results: " results)
  (spit "out.csv" results))


(defn updateTimeCourseVector
  "Updates the sentence number of any of
  the 13 parameters (represented by oldGrammar)
  recorded in timeCourseVector if their coresponding
  value in grammar has changed after being
  processed by setParameters"
  [timeCourseVector currGrammar oldGrammar sentenceCount]
  (let [i (atom 0)
        countArr (atom [])]
    (while (< @i 13)
        (if (not= (get oldGrammar @i) (get currGrammar @i))
          (reset! countArr (conj @countArr sentenceCount))
          (reset! countArr (conj @countArr (get (get timeCourseVector 1) @i))))
        (swap! i inc))
    (assoc timeCourseVector 1 @countArr)))


(defn doesChildLearnGrammar
  "Runs sentence consuming functions until the
  correct grammar is learned or the maximum number
  of sentences are processed. Needs infoList, grammar, expected grammar"
  [sentences max_num]

  (let [grammarLearned (atom false)
        sentenceCount (atom 0)
        grammar (atom [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1])
        timeCourseVector (atom [[1 2 3 4 5 6 7 8 9 10 11 12 13] [-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1]])
        oldGrammar (atom [-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1])]

        (while (and (not @grammarLearned) (< @sentenceCount max_num))
          (let [infoList1 (consumeSentence (rand-nth sentences))]
            (reset! grammar (setParameters infoList1 grammar))
            (reset! timeCourseVector (updateTimeCourseVector @timeCourseVector @grammar @oldGrammar @sentenceCount))
            (reset! oldGrammar @grammar)
            (reset! grammarLearned (isGrammarLearned? grammar infoList1)))
          (swap! sentenceCount inc))

        (println "Finished")
        ;(writeResults @grammar)
        [@grammar @grammarLearned (get @timeCourseVector 0) (get @timeCourseVector 1)]))


(defn runSimulation
  "max_eChildren number of eChildren will process
  max_sentences number of sentences."
  [sentences max_eChildren max_sentences]
  (io/delete-file "out.csv" :silently true)

  (loop [i 0
         results []]
    (if (< i max_eChildren)
      (do 
        (recur (inc i) (conj results (future (doesChildLearnGrammar sentences max_sentences)))))
      (do
        (writeResults results) (println "Finished simulation")))))


(comment
working version
(defn runSimulation
  "max_eChildren number of eChildren will process
  max_sentences number of sentences."
  [sentences max_eChildren max_sentences]
  (io/delete-file "out.csv" :silently true)

  (loop [i 0]
    (when (< i max_eChildren)
      (future (doesChildLearnGrammar sentences max_sentences))
      (recur (inc i))))
  (println "Finished simulation"))

)

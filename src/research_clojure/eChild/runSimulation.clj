(ns research-clojure.eChild.runSimulation)
(require 'research-clojure.eChild.Child)
(refer 'research-clojure.eChild.Child)
(require '[clojure.string :as str]
         '[clojure.data.csv :as csv]
         '[clojure.java.io :as io]
         '[clojure.set :refer [union]]
         '[clojure.core.async :as a 
                              :refer [>! <! chan alts!! thread go]])


(defn writeResults
  "Writes the results of doesChildLearnGrammar to output file"
  [lines]
  (let [parameterList (get lines 2)
        convergedList (get lines 3)]

      (with-open [out-file (io/writer "out.csv" :append true)]
        (csv/write-csv out-file [[(get lines 0) (get lines 1) 
          (get parameterList 0) (get convergedList 0) (get parameterList 1) 
          (get convergedList 1) (get parameterList 2) (get convergedList 2) 
          (get parameterList 3) (get convergedList 3) (get parameterList 4) 
          (get convergedList 4) (get parameterList 5) (get convergedList 5) 
          (get parameterList 6) (get convergedList 6) (get parameterList 7) 
          (get convergedList 7) (get parameterList 8) (get convergedList 8) 
          (get parameterList 9) (get convergedList 9) (get parameterList 10) 
          (get convergedList 10) (get parameterList 11) (get convergedList 11) 
          (get parameterList 12) (get convergedList 12)]]))))


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

;The final grammar must be return from 
;this function and piped into writeResults
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

        (println "Final grammar: " @grammar)
        [@grammar @grammarLearned (first @timeCourseVector) (last @timeCourseVector)]))


(defn runSimulation
  "totalEChildren number of the function doesChildLearnGrammar
  will run, each on their own thread. The answers will
  then be fed into writeResults"
  [sentences totalEChildren max_sentences]
  (io/delete-file "out.csv" :silently true)

  (let [c (chan)]
    (go (while true (writeResults (<! c))))
    (dotimes [m totalEChildren] (go (>! c (doesChildLearnGrammar sentences max_sentences))))))


(defn runSimulation_serial
  "max_eChildren number of eChildren will process 
  max_sentences number of sentences. Doesn't use 
  any concurrency"
  [sentences totalEChildren max_sentences]
  (io/delete-file "out.csv" :silently true)

  (let [counter (atom 0)]
    (while (< @counter totalEChildren)
      (println "eChild #" (+ @counter 1))
      (doesChildLearnGrammar sentences max_sentences)
(swap! counter inc))))
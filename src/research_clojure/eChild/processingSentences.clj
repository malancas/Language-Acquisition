(ns research-clojure.eChild.processingSentences)
(require ['clojure.string :as 'str])


(defn in?
  "Returns true if e is an element of sentence"
  [sentence e]  
  (some #(= e %) sentence))


(defn isDeclarative
  [x]
  (= x "DEC"))


(defn Verb_Never
  [sen]
  (and (isDeclarative (get sen 1)) (= (.indexOf sen "Verb") (- (.indexOf sen "Never") 1)) ((= (.indexOf sen "Aux") -1))))
    

(defn hasKa
  [sen]
  (in? sen "ka"))


(defn Aux_Verb
  [sen]
  (and (isDeclarative (get sen 1)) (= (.indexOf sen "Aux") (- (.indexOf sen "Verb") 1)))) 
    

(defn Verb_Aux
  [sen]
  (and (isDeclarative (get sen 1)) (= (.indexOf sen "Verb") (- (.indexOf sen "Aux") 1))))
   

(defn Never_Verb
  [sen]
  (and (isDeclarative (get sen 1)) (= (.indexOf sen "Never") (.indexOf sen "Verb")) ((= (.indexOf sen "Aux") -1))))


(defn isImperative
  [x]
  (= x "IMP"))


(defn containsTopicalizable
  [sentence]
  (def i (.indexOf sentence "S"))
  (def j (.indexOf sentence "01"))
  (def k (.indexOf sentence "02"))
  (def l (.indexOf sentence "03"))
  (def m (.indexOf sentence "Adv"))
  
  (or (= i 0) (= j 0) (= k 0) (= l 0) (= m 0)))


(defn outOblique
  [sentence]
  (def i (.indexOf sentence "01"))
  (def j (.indexOf sentence "02"))
  (def k (.indexOf sentence "P"))
  (def l (.indexOf sentence "03"))
  
  (if (and (not= i -1) (not= j -1) (not= k -1) (< i j k) (or (and (< i j k) (= l (+ k 1))) (and (< l j i) (= k (+ l 1)))))
    false)

  (if (and (not= i -1) (not= j -1) (not= k -1) (not= l -1))
    true))


(defn isQuestion
  [x]
  (= x "Q"))

    
(defn Verb_tensed
  [sentence x]
  (or isDeclarative x (and isQuestion x (in? sentence "Aux"))))


(defn S_Aux
  [sen]
  (if (isDeclarative (get sen 1))
    (do (def i (.indexOf sen "S"))
        (and (> i 0) (= (+ i 1) (.indexOf sen "Aux")))))
  false)


(defn Aux_S
  [sen]
  (if (isDeclarative (get sen 1))
    (do (def i (.indexOf sen "Aux"))
        (and (> i 0) (= (+ i 1) (.indexOf sen "S")))))
  false)


;First parameter
(defn setSubjPos
  [sen grammar infoList]
  (if (and (in? (get infoList 2) "01") (in? (get infoList 2) "S"))
    ;;Check if 01 and S are in the sentence    
    (do (def first (.indexOf sen "01"))
        (if (and (> first 0) (< first (.indexOf sen "S")))
          (assoc grammar 0 "1")))))


;2nd parameter
(defn setHead
  [sen infoList grammar]
  (if (and (in? (get infoList 2) "03") (in? (get infoList 2) "P"))
    (do (def first (.indexOf sen "03"))
        ;03 followed by P
        (if (and (> first 0) (= (.indexOf sen "P") (+ first 1)))
          (assoc grammar 1 "1"))))
  ;If imperative, make sure Verb directly follow 01
  (if (and (isImperative (get infoList 1)) (in? (get infoList 2) "01") (in? (get infoList 2) "Verb"))
    (if (= (.indexOf sen "01") (- (.indexOf sen "Verb") 1))
      (assoc grammar 1 "1"))))


;4th parameter
(defn setObligTopic
  [sen infoList grammar]
  (if (isDeclarative (get infoList 1))
    (def infL2 (get infoList 2))
    (if (and (in? infL2 "02") (= (in? infL2) false))
      (do (assoc grammar 5 "1")
          (if (= (get grammar 3) "1")
            (assoc grammar 3 "0")))
      (if (containsTopicalizable sen)
        (assoc grammar 3 "1")))))


;5th parameter
;Only works for full, not necessarily with CHILDES distribution
(defn setNullSubj
  [sentence infoList grammar]
  (if (and (isDeclarative (get infoList 1)) (= (in? (get infoList 2) "S") false) (outOblique sentence))
    (do (assoc grammar 4 "1")
        (println (get grammar 4)))))


;6th parameter
;infL2 = infoList[2]
(defn setNullTopic
  [infL2 grammar]
  (if (and (in? infL2 "02") (= (in? infL2 "01") false))
    (assoc grammar 5 "1")))


;7th parameter
;infL2 = infoList[2]
(defn setWHMovement
  [sentence infL2 grammar]
  (if (and (> (.indexOf sentence "+WH") 0) (= (in? infL2 "03[+WH]") false))
    (assoc grammar 6 "0")))


;#8th parameter
;infL2 = infoList[2]
(defn setPrepStrand
  [sentence infL2 grammar]
  (if (and (in? infL2 "P") (in? infL2 "03"))
    (do
      (def i (.indexOf sentence "P")) ;Get index of P
      (def j (.indexOf sentence "03")) ;Ge index of 03
      (if (and (not= i -1) (not= j -1) (not= (Math/abs (- i j)) -1)) ;If they exist, make sure they aren't adjacent
        (assoc grammar 7 "1")))))


;9th parameter
;infL2 = infoList[2]
(defn setTopicMark
  [infL2 grammar]
  (if (in? infL2 "WA")
    (assoc grammar 8 "1")))


;10th parameter
;infL2 = infoList[2]
(defn vToI
  [sentence infL2 grammar]
  (if (and (in? infL2 "01") (in? infL2 "Verb"))
    (do (def i (.indexOf sentence "01"))
        (def j (.indexOf sentence "Verb"))
        (if (and (> i 0) (not= j -1) (not= (Math/abs (- i j)) 1))
          (assoc grammar 9 "1")))))


;11th parameter
(defn iToC
  [sentence grammar]
  (def g0 (get grammar 0))
  (def g1 (get grammar 1))
  (def g2 (get grammar 2))
  
  (if (and (= g0 "0") (= g1 "0") (= g2 "0") (S_Aux sentence))
    (assoc grammar 10 "0"))
  (if (and (= g0 "1") (= g1 "1") (= g2 "1") (Aux_S sentence))
    (assoc grammar 10 "0"))
  (if (and (= g0 "1") (= g1 "0") (= g2 "1") (Aux_Verb sentence))
    (assoc grammar 10 "0"))
  (if (and (= g0 "0") (= g1 "1") (= g2 "0") (Verb_Aux sentence))
    (assoc grammar 10 "0"))
  (if (and (= g0 "0") (= g1 "0") (= g2 "1") (S_Aux sentence))
    (assoc grammar 10 "0"))
  (if (and (= g0 "1") (= g1 "1") (= g2 "0") (Aux_S sentence))
    (assoc grammar 10 "0"))
  (if (and (= g0 "1") (= g1 "0") (= g2 "0") (or (Never_Verb sentence) (hasKa sentence)))
    (assoc grammar 10 "0"))
  (if (and (= g0 "0") (= g1 "1") (= g2 "1") (or (Verb_Never sentence) (hasKa sentence)))
    (assoc grammar 10 "0")))


;12th parameter                                                                                                             
(defn affixHop
  [sentence infoList  grammar]
  (def infL1 (get infoList 1))
  (def infL2 (get infoList 2))

  (if (and (Verb_tensed infL1) (in? infL2 "Never Verb 01"))
    (assoc grammar 11 "1"))
  (if (and (Verb_tensed infL1) (> (.indexOf sentence "01") 0) (in? infL2 "01 Verb Never"))
    (assoc grammar 11 "1")))

  
;13th parameter
;infL2 = infoList[2]
(defn questionInver
  [infL2 grammar]
  (if (in? infL2 "ka")
    (assoc grammar 12 "0")))


(defn parameter1
  [grammar infoList]
  (if (= (get grammar 0) "0")
    (def newGrammar (setSubjPos infoList grammar))))


(defn parameter2
  [grammar infoList]
  (if (= (get grammar 1) "0")
    (def newGrammar (setHead infoList grammar))))

(defn parameter3
  [grammar infoList]
  (if (= (get grammar 2) "0")
    (def newGrammar (setHead infoList grammar))))

(defn parameter4
  [grammar infoList]
  (if (and (not= (get grammar 3) "0") (= (get grammar 5) "1"))
    (def newGrammar (setObligTopic infoList grammar))))

(defn parameter5
  [grammar infoList]
  (if (= (get grammar 4) "0")
    (def newGrammar (setNullSubj infoList grammar))))

(defn parameter6
  [grammar infoList]
  (if (= (get grammar 5) "0")
    (def newGrammar (setNullTopic infoList grammar))))

(defn parameter7
  [grammar infoList]
  (if (= (get grammar 6) "1")
    (def newGrammar (setWHMovement infoList grammar))))


(defn parameter8
  [grammar infoList]
  (if (= (get grammar 7) "0")
    (def newGrammar (setPrepStrand infoList grammar))))


(defn parameter9
  [grammar infoList]
  (if (= (get grammar 8) "0")
    (def newGrammar (setTopicMark infoList grammar))))


(defn parameter10
  [grammar infoList]
  (if (= (get grammar 9) "0")
    (def newGrammar (vToI infoList grammar))))


(defn parameter11
  [grammar infoList]
  (if (= (get grammar 10) "1")
    (def newGrammar (iToC infoList grammar))))


(defn parameter12
  [grammar infoList]
  (if (= (get grammar 11) "0")
    (def newGrammar (affixHop infoList grammar))))


(defn parameter13
  [grammar infoList]
  (if (= (get grammar 12) "1")
    (def newGrammar (questionInver infoList grammar))))


(defn isGrammarLearned?
  [currentGrammar expectedGrammar]
  (= currentGrammar expectedGrammar))


(defn setParameters
  "Uses grammar, infolist"
  [infoList grammar expectedGrammar]
  
  (def currentGrammar (atom []))
  
  (swap! currentGrammar (parameter1 grammar infoList))
  (swap! currentGrammar (parameter2 currentGrammar infoList))
  (swap! currentGrammar (parameter3 currentGrammar infoList))
  (swap! currentGrammar (parameter4 currentGrammar infoList))
  (swap! currentGrammar (parameter5 currentGrammar infoList))
  (swap! currentGrammar (parameter6 currentGrammar infoList))
  (swap! currentGrammar (parameter7 currentGrammar infoList))
  (swap! currentGrammar (parameter8 currentGrammar infoList))
  (swap! currentGrammar (parameter9 currentGrammar infoList))
  (swap! currentGrammar (parameter10 currentGrammar infoList))
  (swap! currentGrammar (parameter11 currentGrammar infoList))
  (swap! currentGrammar (parameter12 currentGrammar infoList))
  (swap! currentGrammar (parameter13 currentGrammar infoList))

  (def results [currentGrammar, (isGrammarLearned? currentGrammar expectedGrammar)]))
;return newGrammar and grammarLearned in list?


(defn consumeSentence
  "Sets infoList, sentence, expectedGrammar, currentGrammar
  in old implementation"
  [sen]
  (def info (str/replace sen #"\n" ""))
  (def info (str/replace info #"\"" ""))
  (def results [(def infoList (str/split info #"\t")), (def sentence (str/split (get infoList 2) #" "))]))


(defn writeResults [lines]
 ;need to be rewritten
)


(defn doesChildLearnGrammar?
  "Runs sentence consuming functions until the 
  correct grammar is learned or the maximum number
  of sentences are processed"
  [sentences max_num]

  (def grammarLearned (atom false))
  (def sentenceCount (atom 0))

  (while (and (not @grammarLearned) (< @sentenceCount max_num))
    (def infoListAndSentence (consumeSentence (rand-nth sentences)))
    (def currGrammarAndState (setParameters))
    (swap! grammarLearned (isGrammarLearned? (get currGrammarAndState 1)))
    (swap! sentenceCount inc))
  
  (writeResults currGrammarAndState)
)


(defn runSimulation
  [sentences max_eChildren max_sentences]
  (def count (atom 0))
  (while (< count max_eChildren)
    (println count)
    (doesChildLearnGrammar? sentences max_sentences)
    (swap! count inc)))


(defn noSubjPos
  [sen grammar]
  (if (and (in? (get infoList 2) "01") (in? (get infoList 2) "S"))
    ;;Check if 01 and S are in the sentence
    (do (def first (.indexOf sen "S"))
        ;Make sure 01 is non-sentence-initial and before S
        (if (and (> first 0) (< first (.indexOf sen "01")))
          (assoc grammar 0 "0")))))
    

(defn noHead
  [sen infoList grammar]
  (if (and (in? (get infoList 2) "03") (in? (get infoList 2) "P"))
    (do (def first (.indexOf sen "P"))
        ;03 followed by P
        (if (and (> first 0) (= (.indexOf sen "03") (+ first 1)))
          (assoc grammar 1 "0"))))
  
  ;If imperative, make sure Verb directly follows 01
  (if (and (isImperative (get infoList 1)) (in? (get infoList 2) "01") (in? (get infoList 2) "Verb"))
    (if (= (.indexOf sen "Verb") (- (.indexOf sen "01") 1))
      (assoc grammar 1 "0"))))


;3rd parameer
;infL1 must be infoList[1]
(defn setHeadCP
  [sen infL1 grammar]
  (if (isQuestion infL1)
    (if (or (= (.indexOf sen "ka") (- (count sen) 1)) (and (= (in? sen "ka") false) (= (.indexOf sen "Aux") (- (count sen) 1))))
      (assoc grammar 2 "1"))))


;infL1 must be infoList[1]
(defn noHeadCP
  [sen infL1 grammar]
  (if (isQuestion infL1)
    (if (or (= (.indexOf sen "ka") 0) (and (= (in? sen "ka") false) (= (.indexOf sen "Aux") 0)))
      (assoc grammar 2 "0"))))


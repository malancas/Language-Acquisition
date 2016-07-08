(ns research-clojure.eChild.Child)
(require ['clojure.string :as 'str])


(defn in?
  "Returns true if e is an element of sentence"
  [sentence e]
  (println "sentence: " sentence)
  (println "e: " e)
  (> (.indexOf sentence e) -1))


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


;4th parameter
(defn setObligTopic
  [sen infoList grammar]
  (if (isDeclarative (get infoList 1))
    (do (def infL2 (get infoList 2))
        (if (and (in? infL2 "02") (= (in? infL2) false))
          (do (assoc grammar 5 "1")
              (if (= (get grammar 3) "1")
                (assoc grammar 3 "0")
                grammar))
          (if (containsTopicalizable sen)
            (assoc grammar 3 "1")
            grammar)))
    grammar))


;5th parameter
;Only works for full, not necessarily with CHILDES distribution
(defn setNullSubj
  [sentence infoList grammar]
  (if (and (isDeclarative (get infoList 1)) (= (in? (get infoList 2) "S") false) (outOblique sentence))
    (assoc grammar 4 "1")
    grammar))


;6th parameter
;infL2 = infoList[2]
(defn setNullTopic
  [infL2 grammar]
  (if (and (in? infL2 "02") (= (in? infL2 "01") false))
    (assoc grammar 5 "1")
    grammar))


;7th parameter
;infL2 = infoList[2]
(defn setWHMovement
  [sentence infL2 grammar]
  (if (and (> (.indexOf sentence "+WH") 0) (= (in? infL2 "03[+WH]") false))
    (assoc grammar 6 "0")
    grammar))


;#8th parameter
;infL2 = infoList[2]
(defn setPrepStrand
  [sentence infL2 grammar]
  (if (and (in? infL2 "P") (in? infL2 "03"))
    (do
      (def i (.indexOf sentence "P")) ;Get index of P
      (def j (.indexOf sentence "03")) ;Ge index of 03
      (if (and (not= i -1) (not= j -1) (not= (Math/abs (- i j)) -1)) ;If they exist, make sure they aren't adjacent
        (assoc grammar 7 "1")
        grammar))
    grammar))


;9th parameter
;infL2 = infoList[2]
(defn setTopicMark
  [infL2 grammar]
  (if (in? infL2 "WA")
    (assoc grammar 8 "1")
    grammar))


;10th parameter
;infL2 = infoList[2]
(defn vToI
  [sentence infL2 grammar]
  (if (and (in? infL2 "01") (in? infL2 "Verb"))
    (do (def i (.indexOf sentence "01"))
        (def j (.indexOf sentence "Verb"))
        (if (and (> i 0) (not= j -1) (not= (Math/abs (- i j)) 1))
          (assoc grammar 9 "1")
          grammar))
    grammar))


(defn iToC_aux1
  [g0 g1 g2 sentence grammar]
  (if (and (= g0 "0") (= g1 "0") (= g2 "0") (S_Aux sentence))
    (assoc grammar 10 "0")
    grammar))


(defn iToC_aux2
  [g0 g1 g2 sentence grammar]
  (if (and (= g0 "1") (= g1 "1") (= g2 "1") (Aux_S sentence))
    (assoc grammar 10 "0")
    grammar))


(defn iToC_aux3
  [g0 g1 g2 sentence grammar]
  (if (and (= g0 "1") (= g1 "0") (= g2 "1") (Aux_Verb sentence))
    (assoc grammar 10 "0")
    grammar))


(defn iToC_aux4
  [g0 g1 g2 sentence grammar]
  (if (and (= g0 "0") (= g1 "1") (= g2 "0") (Verb_Aux sentence))
    (assoc grammar 10 "0")
    grammar))


(defn iToC_aux5
  [g0 g1 g2 sentence grammar]
  (if (and (= g0 "0") (= g1 "0") (= g2 "1") (S_Aux sentence))
    (assoc grammar 10 "0")
    grammar))


(defn iToC_aux6
  [g0 g1 g2 sentence grammar]
  (if (and (= g0 "1") (= g1 "1") (= g2 "0") (Aux_S sentence))
    (assoc grammar 10 "0")
    grammar))


(defn iToC_aux7
  [g0 g1 g2 sentence grammar]
  (if (and (= g0 "1") (= g1 "0") (= g2 "0") (or (Never_Verb sentence) (hasKa sentence)))
    (assoc grammar 10 "0")
    grammar))


(defn iToC_aux8
  [g0 g1 g2 sentence grammar]
  (if (and (= g0 "0") (= g1 "1") (= g2 "1") (or (Verb_Never sentence) (hasKa sentence)))
    (assoc grammar 10 "0")
    grammar))


(defn iToC
  "11th parameter"
  [sentence grammar]
  (def g0 (get grammar 0))
  (def g1 (get grammar 1))
  (def g2 (get grammar 2))
  (def currentGrammar (atom grammar))

  (reset! currentGrammar (iToC_aux1 g0 g1 g2 sentence currentGrammar))
  (reset! currentGrammar (iToC_aux2 g0 g1 g2 sentence currentGrammar))
  (reset! currentGrammar (iToC_aux3 g0 g1 g2 sentence currentGrammar))  
  (reset! currentGrammar (iToC_aux4 g0 g1 g2 sentence currentGrammar)) 
  (reset! currentGrammar (iToC_aux5 g0 g1 g2 sentence currentGrammar)) 
  (reset! currentGrammar (iToC_aux6 g0 g1 g2 sentence currentGrammar)) 
  (reset! currentGrammar (iToC_aux7 g0 g1 g2 sentence currentGrammar))
  (reset! currentGrammar (iToC_aux8 g0 g1 g2 sentence currentGrammar)))


(defn affixHop_aux1
  [infL1 infL2 grammar]
  (if (and (Verb_tensed infL1) (in? infL2 "Never Verb 01"))
    (assoc grammar 11 "1")
    grammar))


(defn affixHop_aux2
  [infL1 infL2 sentence grammar]
  (if (and (Verb_tensed infL1) (> (.indexOf sentence "01") 0) (in? infL2 "01 Verb Never"))
    (assoc grammar 11 "1")
    grammar))

                                                  
(defn affixHop
  "12th parameter"
  [sentence infoList  initGrammar]
  (def infL1 (get infoList 1))
  (def infL2 (get infoList 2))
  (def currentGrammar (atom initGrammar))

  (reset! currentGrammar (affixHop_aux1 infL1 infL2 currentGrammar))
  (reset! currentGrammar (affixHop_aux2 infL1 infL2 sentence currentGrammar))
  currentGrammar)

  
;13th parameter
;infL2 = infoList[2]
(defn questionInver
  [infL2 grammar]
  (if (in? infL2 "ka")
    (assoc grammar 12 "0")
    grammar))


(defn setSubjPos
  [grammar sen]
  (if (and (in? sen "01") (in? sen "S"))
    ;Check if 01 and S are in the sentence
    (do (let [first (.indexOf sen "01")]
         (if (and (> first 0) (< first (.indexOf sen "S")))
           (assoc grammar 0 "1")
           grammar)))
    grammar))


(defn setHead
  [infoList grammar]
  (if (and (in? (get infoList 2) "03") (in? (get infoList 2) "P"))
        (do (def first (.indexOf (get infoList 2) "03"))
            ;03 followed by P
            (if (and (> first 0) (= (.indexOf (get infoList 2) "P") (+ first 1)))
              (assoc grammar 1 "1")
              grammar))
        grammar)
   ;If imperative, make sure Verb directly follow 01
   (if (and (isImperative (get infoList 1)) (in? (get infoList 2) "01") (in? (get infoList 2) "Verb"))
     (if (= (.indexOf (get infoList 2) "01") (- (.indexOf (get infoList 2) "Verb") 1))
       (assoc grammar 1 "1")
       grammar)
     grammar))


(defn parameter1
  [grammar infoList]
  (if (= (get grammar 0) "0")
    (setSubjPos grammar (get infoList 2))
    grammar))


(defn parameter2
  [grammar infoList]
  (if (= (get grammar 1) "0")
    (setHead infoList grammar)
    grammar))


(defn parameter3
  [grammar infoList]
  (if (= (get grammar 2) "0")
    (setHead infoList grammar)
    grammar))


(defn parameter4
  [grammar infoList]
  (if (and (not= (get grammar 3) "0") (= (get grammar 5) "1"))
    (setObligTopic infoList grammar)
    grammar))


(defn parameter5
  [grammar infoList]
  (if (= (get grammar 4) "0")
    (setNullSubj infoList grammar)
    grammar))


(defn parameter6
  [grammar infoList]
  (if (= (get grammar 5) "0")
    (setNullTopic infoList grammar)
    grammar))


(defn parameter7
  [initGrammar infoList]
  (if (= (get initGrammar 6) "1")
    (setWHMovement infoList initGrammar)
    initGrammar))


(defn parameter8
  [initGrammar infoList]
  (if (= (get initGrammar 7) "0")
    (setPrepStrand infoList initGrammar)
    initGrammar))


(defn parameter9
  [initGrammar infoList]
  (if (= (get initGrammar 8) "0")
    (setTopicMark infoList initGrammar)
    initGrammar))


(defn parameter10
  [initGrammar infoList]
  (if (= (get initGrammar 9) "0")
    (vToI infoList initGrammar)
    initGrammar))


(defn parameter11
  [initGrammar infoList]
  (if (= (get initGrammar 10) "1")
    (iToC infoList initGrammar)
    initGrammar))


(defn parameter12
  [initGrammar infoList]
  (if (= (get initGrammar 11) "0")
    (affixHop infoList initGrammar)
    initGrammar))


(defn parameter13
  [initGrammar infoList]
  (if (= (get initGrammar 12) "1")
    (questionInver infoList initGrammar)
    initGrammar))


(defn isGrammarLearned?
  "Converts the grammar ID to binary and compares it to
  currentGrammar"
  [currentGrammar infoList]
  (let [dec (int (read-string (get infoList 0)))]
    (= currentGrammar (Integer/toString dec 2))))


(comment 
  (defn setParameters2
    [infoList initGrammar funcList]
    (def currentGrammar (atom initGrammar))
    
    (let [remainingFuncs funcList]

      (loop [remainingFuncs funcList]
        (if (empty? remainingFuncs)
          [currentGrammar, (isGrammarLearned? currentGrammar infoList)]
          (let [[func & remaining] remainingFuncs]
            (recur (reset! currentGrammar (func infoList currentGrammar))))))))
)


(defn setParameters
  "Uses grammar, infolist"
  [infoList initGrammar]
  
  (def currentGrammar (atom []))

  (reset! currentGrammar (setSubjPos initGrammar infoList))
  (reset! currentGrammar (parameter2 currentGrammar infoList))
  (reset! currentGrammar (parameter3 currentGrammar infoList))
  (reset! currentGrammar (parameter4 currentGrammar infoList))
  (reset! currentGrammar (parameter5 currentGrammar infoList))
  (reset! currentGrammar (parameter6 currentGrammar infoList))
  (reset! currentGrammar (parameter7 currentGrammar infoList))
  (reset! currentGrammar (parameter8 currentGrammar infoList))
  (reset! currentGrammar (parameter9 currentGrammar infoList))
  (reset! currentGrammar (parameter10 currentGrammar infoList))
  (reset! currentGrammar (parameter11 currentGrammar infoList))
  (reset! currentGrammar (parameter12 currentGrammar infoList))
  (reset! currentGrammar (parameter13 currentGrammar infoList))

  [currentGrammar, (isGrammarLearned? currentGrammar infoList)])
;return newGrammar and grammarLearned in list?


(defn consumeSentence
  "Sets infoList, sentence, expectedGrammar, currentGrammar
  in old implementation"
  [sen]
  (def info (clojure.string/join " " sen))
  (def infoList (clojure.string/split info #" " 3))
  [infoList, (clojure.string/split (get infoList 2) #" ")])


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

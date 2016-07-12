(ns research-clojure.eChild.Child)
(require ['clojure.string :as 'str])


(defn in?
  "Returns true if e is an element of sentence"
  [sentence e]
  (= e (some #{e} sentence)))


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
    (do (let [i (.indexOf sen "S")]
          (and (> i 0) (= (+ i 1) (.indexOf sen "Aux")))))
    false))


(defn Aux_S
  [sen]
  (if (isDeclarative (get sen 1))
    (do (let [i (.indexOf sen "Aux")]
          (and (> i 0) (= (+ i 1) (.indexOf sen "S")))))
    false))


(defn setObligTopic
  "4th parameter"
  [infoList grammar]
  (if (isDeclarative (get infoList 1))
    (do (let [sentence (get infoList 2)]
          (if (and (in? sentence "02") (= (in? sentence) false))
            (do (assoc grammar 5 1)
                (if (= (get grammar 3) 1)
                  (assoc grammar 3 0)
                  grammar))
            (if (containsTopicalizable sentence)
              (assoc grammar 3 1)
              grammar))))
    grammar))


(defn setNullSubj
  "5th parameter,
  Only works for full, not 
  necessarily with CHILDES distribution"
  [infoList grammar]
  (if (and (isDeclarative (get infoList 1)) (= (in? (get infoList 2) "S") false) (outOblique (get infoList 2)))
    (assoc grammar 4 1)
    grammar))


(defn setNullTopic
  "6th parameter"
  [sentence grammar]
  (if (and (in? sentence "02") (= (in? sentence "01") false))
    (assoc grammar 5 1)
    grammar))


(defn setWHMovement
  "7th parameter"
  [sentence grammar]
  (if (and (> (.indexOf sentence "+WH") 0) (= (in? sentence "03[+WH]") false))
    (assoc grammar 6 0)
    grammar))


(defn setPrepStrand
  "8th parameter"
  [sentence grammar]
  (if (and (in? sentence "P") (in? sentence "03"))
    (do
      (def i (.indexOf sentence "P")) ;Get index of P
      (def j (.indexOf sentence "03")) ;Ge index of 03
      (if (and (not= i -1) (not= j -1) (not= (Math/abs (- i j)) -1)) ;If they exist, make sure they aren't adjacent
        (assoc grammar 7 1)
        grammar))
    grammar))


(defn setTopicMark
  "9ath parameter"
  [sentence grammar]
  (if (in? sentence "WA")
    (assoc grammar 8 1)
    grammar))


(defn vToI
  "10th parameter"
  [sentence grammar]
  (if (and (in? sentence "01") (in? sentence "Verb"))
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
  [senType sentence grammar]
  (if (and (Verb_tensed senType) (in? sentence "Never Verb 01"))
    (assoc grammar 11 1)
    grammar))


(defn affixHop_aux2
  [senType sentence grammar]
  (if (and (Verb_tensed senType) (> (.indexOf sentence "01") 0) (in? sentence "01 Verb Never"))
    (assoc grammar 11 1)
    grammar))

                                                  
(defn affixHop
  "12th parameter"
  [infoList initGrammar]
  (def senType (get infoList 1))
  (def sentence (get infoList 2))
  (def currentGrammar (atom initGrammar))

  (reset! currentGrammar (affixHop_aux1 senType sentence currentGrammar))
  (reset! currentGrammar (affixHop_aux2 senType sentence currentGrammar))
  currentGrammar)

  
(defn questionInver
  "13th parameter"
  [sentence grammar]
  (if (in? sentence "ka")
    (assoc grammar 12 "0")
    grammar))


(defn setSubjPos
  [grammar sen]
  (if (and (in? sen "01") (in? sen "S"))
    ;Check if 01 and S are in the sentence
    (do (let [first (.indexOf sen "01")]
         (if (and (> first 0) (< first (.indexOf sen "S")))
           (assoc grammar 0 1)
           grammar)))
    grammar))


(defn setHead_aux1
  "Checks if both 03 and P are present in the sentence
  and if they appear sequentially"
  [infoList initGrammar]
  (if (and (not= nil (in? (get infoList 2) "03")) (not= nil (in? (get infoList 2) "P")))
    (do (def first (.indexOf (get infoList 2) "03"))
        ;If 03 followed by P
            (if (and (> first 0) (= (.indexOf (get infoList 2) "P") (+ first 1)))
              (assoc initGrammar 1 1)
              initGrammar))
    initGrammar))


(defn setHead_aux2
  "If imperative, make sure Verb directly follow 01"
  [infoList initGrammar]
  (if (and (isImperative (get infoList 1)) (in? (get infoList 2) "01") (in? (get infoList 2) "Verb"))
     (if (= (.indexOf (get infoList 2) "01") (- (.indexOf (get infoList 2) "Verb") 1))
       (assoc initGrammar 1 1)
       initGrammar)
     initGrammar))


(defn setHead
  [infoList grammar]
  (let [tempGrammar (setHead_aux1 infoList grammar)]   
    (setHead_aux2 infoList tempGrammar)))


(defn parameter1
  [grammar infoList]
  (if (= (get grammar 0) 0)
    (setSubjPos grammar (get infoList 2))
    grammar))


(defn parameter2
  [grammar infoList]
  (if (= (get grammar 1) 0)
    (setHead infoList grammar)
    grammar))


(defn parameter3
  [grammar infoList]
  (if (= (get grammar 2) 0)
    (setHead infoList grammar)
    grammar))


(defn parameter4
  [grammar infoList]
  (if (and (not= (get grammar 3) 0) (= (get grammar 5) 1))
    (setObligTopic infoList grammar)
    grammar))


(defn parameter5
  [grammar infoList]
  (if (= (get grammar 4) 0)
    (setNullSubj infoList grammar)
    grammar))


(defn parameter6
  [grammar infoList]
  (if (= (get grammar 5) 0)
    (setNullTopic infoList grammar)
    grammar))


(defn parameter7
  [initGrammar infoList]
  (if (= (get initGrammar 6) 1)
    (setWHMovement infoList initGrammar)
    initGrammar))


(defn parameter8
  [initGrammar infoList]
  (if (= (get initGrammar 7) 0)
    (setPrepStrand infoList initGrammar)
    initGrammar))


(defn parameter9
  [initGrammar infoList]
  (if (= (get initGrammar 8) 0)
    (setTopicMark infoList initGrammar)
    initGrammar))


(defn parameter10
  [initGrammar infoList]
  (if (= (get initGrammar 9) 0)
    (vToI infoList initGrammar)
    initGrammar))


(defn parameter11
  [initGrammar infoList]
  (if (= (get initGrammar 10) 1)
    (iToC infoList initGrammar)
    initGrammar))


(defn parameter12
  [initGrammar infoList]
  (if (= (get initGrammar 11) 0)
    (affixHop infoList initGrammar)
    initGrammar))


(defn parameter13
  [initGrammar infoList]
  (if (= (get initGrammar 12) 1)
    (questionInver infoList initGrammar)
    initGrammar))


(defn isGrammarLearned?
  "Converts the grammar ID to binary and compares it to
  currentGrammar"
  [currentGrammar grammarID]
  (let [dec (int (read-string (get grammarID 0)))]
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

  (reset! currentGrammar (parameter1 initGrammar infoList))
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


(defn setHeadCP
  "3rd parameter"
  [sen senType grammar]
  (if (isQuestion senType)
    (if (or (= (.indexOf sen "ka") (- (count sen) 1)) (and (= (in? sen "ka") false) (= (.indexOf sen "Aux") (- (count sen) 1))))
      (assoc grammar 2 1))))


(defn noHeadCP
  [sen senType grammar]
  (if (isQuestion senType)
    (if (or (= (.indexOf sen "ka") 0) (and (= (in? sen "ka") false) (= (.indexOf sen "Aux") 0)))
      (assoc grammar 2 "0"))))

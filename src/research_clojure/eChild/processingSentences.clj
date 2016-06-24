(ns research-clojure.eChild.processingSentences)
(require ['clojure.string :as 'str])

(defn isQuestion
  [x]
  (= x "Q"))


(defn isImperative
  [x]
  (= x "IMP"))


(defn isDelarative
  [x]
  (= x "DEC"))


(defn in?
  "Returns true if lt is an element of sentence"
  [sentence lt]  
  (some #(= lt %) sentence))

    
(defn Verb_tensed
  [sentence]
  (or isDeclarative x (and isQuestion x (in? sentence "Aux"))))


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


(defn Aux_Verb
  [sen]
  (and (isDeclarative (get sen 1)) (= (.indexOf sen "Aux") (- (.indexOf sen "Verb") 1)))) 
    

(defn Verb_Aux
  [sen]
  (and (isDeclarative (get sen 1)) (= (.indexOf sen "Verb") (- (.indexOf sen "Aux") 1))))
   

(defn Never_Verb
  [sen]
  (and (isDeclarative (get sen 1)) (= (.indexOf sen "Never") (.indexOf sen "Verb")) ((= (.indexOf sen "Aux") -1))))


(defn Verb_Never
  [sen]
  (and (isDeclarative (get sen 1)) (= (.indexOf sen "Verb") (- (.indexOf sen "Never") 1)) ((= (.indexOf sen "Aux") -1))))
    

(defn hasKa
  [sen]
  (in? sen "ka"))


(defn setParameters
  [sen])


;First parameter
(defn setSubjPos
  [sen grammar]
  (if (and (in? (get infoList 2) "01") (in? (get infoList 2) "S"))
    ;;Check if 01 and S are in the sentence
    (do (def first (.indexOf sen "01"))
        ;Make sure 01 is non-sentence-initial and before S
        (if (and (> first 0) (< first (.indexOf sen "S")))
          (assoc grammar 0 '1')))))


(defn noSubjPos
  [sen grammar]
  (if (and (in? (get infoList 2) "01") (in? (get infoList 2) "S"))
    ;;Check if 01 and S are in the sentence
    (do (def first (.indexOf sen "S"))
        ;Make sure 01 is non-sentence-initial and before S
        (if (and (> first 0) (< first (.indexOf sen "01")))
          (assoc grammar 0 "0")))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn consumeSentence
  [sen]
  (def info (str.replace sen #"\n" ""))
  (def info (str.replace info #"\"" ""))
  (def infoList (str.split info #"\t"))
  (def sentence (str.split (get infoList 2) #" ")))

    
(defn doesChildLearnGrammar
  "Runs sentence consuming functions until the 
  correct grammar is learned or the maximum number
  of sentences are processed"
  [sentences max_num]
  (def grammarLearned false)
  (def count 0)

  (while (and (not grammarLearned) (< count max_num))
    (consumeSentence (rand-nth sentences))))


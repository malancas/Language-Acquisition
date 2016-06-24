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

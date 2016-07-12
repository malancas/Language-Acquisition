(ns research-clojure.child-test
  (:require [clojure.test :refer :all]
            [research-clojure.eChild.Child :refer :all]))


(deftest in?-tests
  (testing "Testing in?"
    (def x ["80", "Q", ["Adv", "03", "P", "S"]])
    (assert (in? (get x 2) "P"))
    (assert (in? (get x 2) "03"))
    (assert (not (in? (get x 2) "01")))))

(deftest parameter1-tests
  (testing "Testing parameter1"
    ;The grammar won't change since 01 and S don't appear
    ;in the sentence
    (let [currGrammar (parameter1 [0,0,0,1,0,1,1,0,0,0,0,0,0]  ["611", "DEC", ["Adv", "S", "Aux", "Never", "Verb"]])]
         (assert (= 13 (count currGrammar)))
         (assert (= nil (some #(< 1 %) currGrammar)))
         (assert (= currGrammar [0,0,0,1,0,1,1,0,0,0,0,0,0])))
    
    ;The grammar will change since 01 appears before S appears
    ;in the sentence and the index of 01 is greater than zero
    (let [currGrammar (parameter1 [0,0,0,1,0,1,1,0,0,0,0,0,0]  ["611", "DEC", ["Adv", "01", "P", "S", "Aux", "Never", "Verb"]])]
         (assert (= 13 (count currGrammar)))
         (assert (= nil (some #(< 1 %) currGrammar)))
         (assert (= currGrammar [1,0,0,1,0,1,1,0,0,0,0,0,0])))

    ;The grammar won't change since the index of 01 in the
    ;sentence is zero
    (let [currGrammar (parameter1 [0,0,0,1,0,1,1,0,0,0,0,0,0]  ["611", "DEC", ["01", "P", "S", "Aux", "Never", "Verb"]])]
         (assert (= 13 (count currGrammar)))
         (assert (= nil (some #(< 1 %) currGrammar)))
         (assert (= currGrammar [0,0,0,1,0,1,1,0,0,0,0,0,0])))))


(deftest parameter2-tests
  (testing "Testing parameter2"
    ;The grammar won't change since 03 and P don't appear
    ;in the sentence
    (let [currGrammar (parameter2 [0,0,0,1,0,1,1,0,0,0,0,0,0]  ["611", "DEC", ["Adv", "S", "Aux", "Never", "Verb"]])]
      (assert (= 13 (count currGrammar)))
      (assert (= nil (some #(< 1 %) currGrammar)))
      (assert (= [0,0,0,1,0,1,1,0,0,0,0,0,0] currGrammar)))

    ;The grammar will change since 03 and P appear in the sentence
    ;and the index of 03 is greater than zero
    (let [currGrammar (parameter2 [0,0,0,1,0,1,1,0,0,0,0,0,0] ["611", "DEC", ["Adv", "03", "P", "S"]])] 
      (assert (= 13 (count currGrammar)))
      (assert (= nil (some #(< 1 %) currGrammar)))
      (assert (= [0,1,0,1,0,1,1,0,0,0,0,0,0] currGrammar)))))

(deftest parameter3-tests
  (testing "Testing parameter3"
    ;Grammar won't change since 03 and P aren't in the sentence
    (let [currGrammar (parameter3 [0,0,0,1,0,1,1,0,0,0,0,0,0]  ["611", "DEC", ["Adv", "S", "Aux", "Never", "Verb"]])]
         (assert (= 13 (count currGrammar)))
         (assert (= nil (some #(< 1 %) currGrammar)))
         (assert (= currGrammar [0,0,0,1,0,1,1,0,0,0,0,0,0])))

    ;The grammar will change since 03 and P appear sequentially
    ;in the sentence
    (let [currGrammar (parameter3 [0,0,0,1,0,1,1,0,0,0,0,0,0]  ["611", "DEC", ["Adv", "03", "P", "Aux", "Never", "Verb"]])]
         (assert (= 13 (count currGrammar)))
         (assert (= nil (some #(< 1 %) currGrammar)))
         (assert (= currGrammar [0,1,0,1,0,1,1,0,0,0,0,0,0])))

    ;The grammar won't change since 03 and P don't appear
    ;sequentially in the sentence
    (let [currGrammar (parameter3 [0,0,0,1,0,1,1,0,0,0,0,0,0]  ["611", "DEC", ["Adv", "03", "Aux", "P", "Never", "Verb"]])]
         (assert (= 13 (count currGrammar)))
         (assert (= nil (some #(< 1 %) currGrammar)))
         (assert (= currGrammar [0,0,0,1,0,1,1,0,0,0,0,0,0])))

    ;The grammar won't change since 01 and Verb don't appear
    ;sequentially in the sentence
    (let [currGrammar (parameter3 [0,0,0,1,0,1,1,0,0,0,0,0,0]  ["611", "IMP", ["Adv", "01", "Aux", "P", "Never", "Verb"]])]
         (assert (= 13 (count currGrammar)))
         (assert (= nil (some #(< 1 %) currGrammar)))
         (assert (= currGrammar [0,0,0,1,0,1,1,0,0,0,0,0,0])))

    ;The grammar will change since 01 and Verb appear
    ;sequentially in the sentence
    (let [currGrammar (parameter3 [0,0,0,1,0,1,1,0,0,0,0,0,0]  ["611", "IMP", ["Adv", "01", "Verb", "Aux", "P", "Never"]])]
         (assert (= 13 (count currGrammar)))
         (assert (= nil (some #(< 1 %) currGrammar)))
         (assert (= currGrammar [0,1,0,1,0,1,1,0,0,0,0,0,0])))))


(deftest parameter4-tests
  (testing "Testing parameter4"
    ;The grammar won't change since 02 doesn't appear
    ;in the sentence
    (let [currGrammar (parameter4 [0,0,0,1,0,1,1,0,0,0,0,0,0] ["611", "DEC", ["Adv", "S", "Aux", "Never", "Verb"]])]
         (assert (= 13 (count currGrammar)))
         (assert (= nil (some #(< 1 %) currGrammar)))
         (assert (= currGrammar [0,0,0,1,0,1,1,0,0,0,0,0,0])))

    ;The sentence will become [0,0,0,0,0,1,1,0,0,0,0,0,0]
    ;since 02 does appear in the sentence and 01 doesn't
    ;and the 4th element in the initial grammar is 1
    (let [currGrammar (parameter4 [0,0,0,1,0,1,1,0,0,0,0,0,0] ["611", "DEC", ["Adv", "S", "Aux", "02", "Never", "Verb"]])]
         (assert (= 13 (count currGrammar)))
         (assert (= nil (some #(< 1 %) currGrammar)))
         (assert (= currGrammar [0,0,0,0,0,1,1,0,0,0,0,0,0])))))

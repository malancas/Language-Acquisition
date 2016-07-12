(ns research-clojure.child-test
  (:require [clojure.test :refer :all]
            [research-clojure.eChild.Child :refer :all]))


(deftest in?-tests
  (testing "Testing in?"
    (def x ["80", "Q", ["Adv", "03", "P", "S"]])
    (assert (not= nil (in? (get x 2) "P")))
    (assert (not= nil (in? (get x 2) "03")))))

(deftest parameter1-tests
  (testing "Testing parameter1"
    ;Test sentence

    (let [currGrammar (parameter1 [0,0,0,1,0,1,1,0,0,0,0,0,0]  ["611", "DEC", ["Adv", "S", "Aux", "Never", "Verb"]])]
         (assert (= 13 (count currGrammar)))
         (assert (= nil (some #(< 1 %) currGrammar)))
         (assert (= currGrammar [0,0,0,1,0,1,1,0,0,0,0,0,0])))
    (let [currGrammar (parameter1 [0,0,0,1,0,1,1,0,0,0,0,0,0]  ["611", "DEC", ["Adv", "01", "P", "S", "Aux", "Never", "Verb"]])]
         (assert (= 13 (count currGrammar)))
         (assert (= nil (some #(< 1 %) currGrammar)))
         (assert (= currGrammar [1,0,0,1,0,1,1,0,0,0,0,0,0])))
    (let [currGrammar (parameter1 [0,0,0,1,0,1,1,0,0,0,0,0,0]  ["611", "DEC", ["01", "P", "S", "Aux", "Never", "Verb"]])]
         (assert (= 13 (count currGrammar)))
         (assert (= nil (some #(< 1 %) currGrammar)))
         (assert (= currGrammar [0,0,0,1,0,1,1,0,0,0,0,0,0])))))


(deftest parameter2-tests
  (testing "Testing parameter2"
  
    (let [currGrammar (parameter2 [0,0,0,1,0,1,1,0,0,0,0,0,0]  ["611", "DEC", ["Adv", "S", "Aux", "Never", "Verb"]])]
      (assert (= 13 (count currGrammar)))
      (assert (= nil (some #(< 1 %) currGrammar)))
      (assert (= [0,0,0,1,0,1,1,0,0,0,0,0,0] currGrammar)))
    (let [currGrammar (parameter2 [0,0,0,1,0,1,1,0,0,0,0,0,0] ["611", "DEC", ["Adv", "03", "P", "S"]])] 
      (assert (= 13 (count currGrammar)))
      (assert (= nil (some #(< 1 %) currGrammar)))
      (assert (= [0,1,0,1,0,1,1,0,0,0,0,0,0] currGrammar)))))

(deftest parameter3-tests
  (testing "Testing parameter3"
  
    (let [currGrammar (parameter3 [0,0,0,1,0,1,1,0,0,0,0,0,0]  ["611", "DEC", ["Adv", "S", "Aux", "Never", "Verb"]])]
         (assert (= 13 (count currGrammar)))
         (assert (= nil (some #(< 1 %) currGrammar)))
         (assert (= currGrammar [0,0,0,1,0,1,1,0,0,0,0,0,0])))))


(deftest parameter4-tests
  (testing "Testing parameter4"
  
    (let [currGrammar (parameter4 [0,0,0,1,0,1,1,0,0,0,0,0,0]  ["611", "DEC", ["Adv", "S", "Aux", "Never", "Verb"]])]
         (assert (= 13 (count currGrammar)))
         (assert (= nil (some #(< 1 %) currGrammar))))))

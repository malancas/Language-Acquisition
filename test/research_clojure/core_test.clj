(ns research-clojure.core-test
  (:require [clojure.test :refer :all]
            [research-clojure.core :refer :all]))

(deftest chooseSentences-test
  (testing "Testing chooseSentences function"
    ;Reads in the sentences
    (def sentences (readFile "EngFrJapGerm.txt"))
    ;Shouldn't return any sentences
    (assert (empty? (chooseSentences sentences "612")))
    ;Testing the French grammar ID 584
    (assert (= 756 (count (chooseSentences sentences "584"))))
    ;Testing the English grammar ID 611
    (assert (= 540 (count (chooseSentences sentences "611"))))
    ;Testing the German grammar ID 2253
    (assert (= 1134 (count (chooseSentences sentences "2253"))))
    ;Testing the Japanese grammar ID 3856
    (assert (= 1092 (count (chooseSentences sentences "3856"))))))

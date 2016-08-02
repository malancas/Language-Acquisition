(ns research-clojure.runSimulation-tests
(:require [clojure.test :refer :all]
          [research-clojure.eChild.runSimulation :refer :all]))

(defn equalTo14
  [x]
  (= x 14))

(deftest updateTimeCourseVector-test1
  (testing "timeCourseVectore value consistency
in updateTimeCourseVector. Every element in the second
element of timeCourseVector will become 14"
    (let [timeCourseVector (atom [[1 2 3 4 5 6 7 8 9 10 11 12 13] [-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1]])
          oldGrammar [1 1 1 1 1 1 0 1 1 1 0 1 0]
          grammar [0 0 0 0 0 0 1 0 0 0 1 0 1]]

      (reset! timeCourseVector (updateTimeCourseVector @timeCourseVector grammar oldGrammar 14))
      (map (assert equalTo14) (get timeCourseVector 1)))))


(deftest updateTimeCourseVector-test2
  (testing "4 of the elements of timeCourseVector[1] will become 14"
    (let [timeCourseVector (atom [[1 2 3 4 5 6 7 8 9 10 11 12 13] [-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1]])
          oldGrammar [0 0 0 1 1 1 1 1 1 1 0 1 0]
          grammar [0 0 0 0 0 0 1 0 0 0 1 0 1]]
      
      (reset! timeCourseVector (updateTimeCourseVector @timeCourseVector grammar oldGrammar 14))

      (let [countArr (get @timeCourseVector 1)]
        (assert (not (equalTo14 (get countArr 0))))
        (assert (not (equalTo14 (get countArr 1))))
        (assert (not (equalTo14 (get countArr 2))))
        (assert (not (equalTo14 (get countArr 6))))
        (assert (equalTo14 (get countArr 3)))
        (assert (equalTo14 (get countArr 4)))
        (assert (equalTo14 (get countArr 5)))
        (assert (equalTo14 (get countArr 7)))
        (assert (equalTo14 (get countArr 8)))
        (assert (equalTo14 (get countArr 9)))
        (assert (equalTo14 (get countArr 10)))
        (assert (equalTo14 (get countArr 11)))
        (assert (equalTo14 (get countArr 12)))))))

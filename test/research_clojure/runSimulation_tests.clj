(ns research-clojure.runSimulation-tests
(:require [clojure.test :refer :all]
          [research-clojure.eChild.runSimulation :refer :all]))

(deftest updateTimeCourseVector-tests
  (testing "timeCourseVectore value consistency
in updateTimeCourseVector"
    (let [timeCourseVector (atom [[0 1 2 3 4 5 6 7 8 9 10 11 12] [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1] [-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1]])]
      (for [x timeCourseVector] (assert (or (= (get x 0) 0) (= (get x 0) 1)))))))

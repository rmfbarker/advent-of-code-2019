(ns advent-of-code-2019.day9-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2019.intcode :as core]))

(deftest examples
  (is (= [109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99]
         (core/compute [109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99] nil)))

  (is (= 16 (count (str (first (core/compute [1102, 34915192, 34915192, 7, 4, 7, 99, 0] nil))))))

  (let [prog [104, 1125899906842624, 99]]
    (is (= (second prog)
           (first (core/compute prog nil))))))

(deftest solution

  (is (= [2789104029] (core/compute (core/read-program "input-day9") [1])))
  (is (= [32869] (core/compute (core/read-program "input-day9") [2]))))
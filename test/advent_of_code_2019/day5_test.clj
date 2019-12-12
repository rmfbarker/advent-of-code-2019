(ns advent-of-code-2019.day5-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2019.core :as core]))

;; Day 5
(deftest day-5
  (testing
    "Using position mode consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not)"

    (is (= 1 (first (core/compute
                      [3 9 8 9 10 9 4 9 99 -1 8]
                      [8]))))

    (is (= 0 (first (core/compute
                      [3 9 8 9 10 9 4 9 99 -1 8]
                      [7]))))

    (is (= 1 (first (core/compute
                      [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9]
                      [1]))))

    (is (= 0 (first (core/compute
                      [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9]
                      [0]))))

    (is (= 1 (first (core/compute
                      [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9]
                      [1]))))

    (is (= 0 (first (core/compute
                      [3 3 1105 -1 9 1101 0 0 12 4 12 99 1]
                      [0]))))

    (is (= 1 (first (core/compute
                      [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9]
                      [1]))))

    (is (= 999 (first (core/compute
                        [3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                         1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                         999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99]
                        [7]))))

    (is (= 1000 (first (core/compute
                         [3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                          1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                          999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99]
                         [8]))))

    (is (= 1001 (first (core/compute
                         [3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                          1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                          999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99]
                         [9]))))))

(deftest solution
  (let [intcodes (core/read-program "input-day5")]
    (is (= 15314507 (last (core/compute intcodes [1]))))
    (is (= 652726 (last (core/compute intcodes [5]))))))

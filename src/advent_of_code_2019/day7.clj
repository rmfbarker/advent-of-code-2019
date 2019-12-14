(ns advent-of-code-2019.day7
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [advent-of-code-2019.intcode :as core]))

(defn solve [phase-sequence program]
  (reduce
    #(first (core/compute
       program
       [%2 %1]))
    0
    phase-sequence))

(defn permutations [s]
  (lazy-seq
    (if (seq (rest s))
      (apply concat (for [x s]
                      (map #(cons x %) (permutations (remove #{x} s)))))
      [s])))

(deftest day7-examples

  (is (= 43210 (solve [4 3 2 1 0] [3 15 3 16 1002 16 10 16 1 16 15 15 4 15 99 0 0])))
  (is (= 54321 (solve [0,1,2,3,4] [3,23,3,24,1002,24,10,24,1002,23,-1,23,
                                   101,5,23,23,1,24,23,23,4,23,99,0,0])))

  (is (= 65210 (solve [1,0,4,3,2] [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
                                   1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]))))

(deftest day7-part1
  (is (= 17440 (let [program (core/read-program "input-day7")
                     all-phases (permutations [0 1 2 3 4])]
                 (first (reverse (sort (map #(solve % program) all-phases))))))))

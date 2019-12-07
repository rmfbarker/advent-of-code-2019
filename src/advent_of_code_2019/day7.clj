(ns advent-of-code-2019.day7
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]))

(defn parse-opcode [x]
  (let [x (format "%02d" x)]
    [(subs x (- (count x) 2))
     (concat (map #(Integer/parseInt (str %)) (drop 2 (reverse x)))
             (repeat 0))]))

(defn evolve [program pointer input output]
  (let [[opcode & parameters] (drop pointer program)
        [instruction modes] (parse-opcode opcode)
        get-value (fn [pos]
                    (let [param (nth parameters pos)
                          mode  (nth modes pos)]
                      (if (= 0 mode) (nth program param)
                                     param)))]
    (condp = instruction
      ;; ADD
      "01" [(assoc program (nth parameters 2) (+
                                                (get-value 0)
                                                (get-value 1)))
            (+ 4 pointer)
            input
            output]

      ;; MULTIPLY
      "02" [(assoc program (nth parameters 2) (*
                                                (get-value 0)
                                                (get-value 1)))
            (+ 4 pointer)
            input
            output]

      ;; STORE
      "03" [(assoc program (nth parameters 0) (first input))
            (+ 2 pointer)
            (rest input)
            output]

      ;; OUTPUT
      "04" [program
            (+ 2 pointer)
            input
            (get-value 0)]

      ;; JUMP if true
      "05" [program
            (if (not= 0 (get-value 0))
              (get-value 1)
              (+ 3 pointer))
            input
            output]

      ;; JUMP if false
      "06" [program
            (if (= 0 (get-value 0))
              (get-value 1)
              (+ 3 pointer))
            input
            output]

      ;; LESS THAN
      "07" [(assoc program (nth parameters 2)
                           (if (< (get-value 0) (get-value 1))
                             1 0))
            (+ 4 pointer)
            input
            output]

      ;; Equals
      "08" [(assoc program (nth parameters 2)
                           (if (= (get-value 0) (get-value 1))
                             1 0))
            (+ 4 pointer)
            input
            output])))

(defn compute [program input]
  (loop [program program
         pointer 0
         input   input
         output  nil]
    (if (= (nth program pointer) 99)
      output
      (let [[program pointer input output] (evolve program pointer input output)]
        (recur program pointer input output)))))

(deftest day-5
  (testing
    "Using position mode consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not)"

    (is (= 1 (compute
               [3 9 8 9 10 9 4 9 99 -1 8]
               [8])))

    (is (= 0 (compute
               [3 9 8 9 10 9 4 9 99 -1 8]
               [7])))

    (is (= 1 (compute
               [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9]
               [1])))

    (is (= 0 (compute
               [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9]
               [0])))

    (is (= 1 (compute
               [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9]
               [1])))

    (is (= 0 (compute
               [3 3 1105 -1 9 1101 0 0 12 4 12 99 1]
               [0])))

    (is (= 1 (compute
               [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9]
               [1])))

    (is (= 999 (compute
                 [3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                  1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                  999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99]
                 [7])))

    (is (= 1000 (compute
                  [3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                   1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                   999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99]
                  [8])))

    (is (= 1001 (compute
                  [3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                   1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                   999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99]
                  [9])))))

(defn read-program [filename] (mapv read-string (clojure.string/split
                                  (clojure.string/trim
                                    (slurp
                                      (io/resource filename)))
                                  #",")))

(deftest solution
  (let [intcodes (read-program "input-day5")]
    (is (= 15314507 (compute intcodes [1])))
    (is (= 652726 (compute intcodes [5])))))

(defn solve [phase-sequence program]
  (reduce
    #(compute
       program
       [%2 %1])
    0
    phase-sequence
    ))

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
  (is (= 17440 (let [program (read-program "input-day7")
                     all-phases (permutations [0 1 2 3 4])]
                 (first (reverse (sort (map #(solve % program) all-phases))))))))

(comment

  (let [program (read-program "input-day7")
        all-phases (permutations [0 1 2 3 4])]
    (first (reverse (sort (map #(solve % program) all-phases))))
    )
  )
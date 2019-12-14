(ns advent-of-code-2019.day7-par2
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [advent-of-code-2019.intcode :as core]))

(defn permutations [s]
  (lazy-seq
    (if (seq (rest s))
      (apply concat (for [x s]
                      (map #(cons x %) (permutations (remove #{x} s)))))
      [s])))

(defn evolve [program pointer input]
  (let [[opcode & parameters] (drop pointer program)
        [instruction modes] (core/parse-opcode opcode)
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
            nil]

      ;; MULTIPLY
      "02" [(assoc program (nth parameters 2) (*
                                                (get-value 0)
                                                (get-value 1)))
            (+ 4 pointer)
            input
            nil]

      ;; INPUT value, pause if nothing in buffer
      "03" [(assoc program (nth parameters 0) (first input))
            (+ 2 pointer)
            (rest input)
            nil]

      ;; OUTPUT value, stop
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
            nil]

      ;; JUMP if false
      "06" [program
            (if (= 0 (get-value 0))
              (get-value 1)
              (+ 3 pointer))
            input
            nil]

      ;; LESS THAN
      "07" [(assoc program (nth parameters 2)
                           (if (< (get-value 0) (get-value 1))
                             1 0))
            (+ 4 pointer)
            input
            nil]

      ;; Equals
      "08" [(assoc program (nth parameters 2)
                           (if (= (get-value 0) (get-value 1))
                             1 0))
            (+ 4 pointer)
            input
            nil])))

(defn solve [phase-sequence program]
  (reduce
    (fn [output phase]
      (first (core/compute
         program
         [phase output])))
    0
    phase-sequence))

(deftest day7-examples

  (is (= 43210 (solve [4 3 2 1 0] [3 15 3 16 1002 16 10 16 1 16 15 15 4 15 99 0 0])))
  (is (= 54321 (solve [0, 1, 2, 3, 4] [3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23, 101, 5, 23, 23, 1, 24, 23, 23, 4, 23, 99, 0, 0])))

  (is (= 65210 (solve [1, 0, 4, 3, 2] [3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33, 1002, 33, 7, 33, 1, 33, 31, 31, 1, 32, 31, 31, 4, 31, 99, 0, 0, 0]))))

(deftest day7-part1
  (is (= 17440 (let [program    (core/read-program "input-day7")
                     all-phases (permutations [0 1 2 3 4])]
                 (first (reverse (sort (map #(solve % program) all-phases))))))))

(defn init-computer [program phase]
  [program 0 [phase] nil])

(defn run-computer [computer amp-input]
  (let [[program pointer input-buffer] computer]
    (loop [program program
           pointer pointer
           input   (conj input-buffer amp-input)
           output  nil]

      (cond
        (= (nth program pointer) 99)
        [program pointer input (or output amp-input) true]

        output
        [program pointer input output false]

        :else
        (let [[program pointer input output] (evolve program pointer input)]
                (recur program pointer input output))))))

(defn init-computers [program phase-sequence]
  (map #(init-computer program %) phase-sequence))

(defn calculate-signal [program phase-sequence]
  (loop [[computer & computers] (init-computers program phase-sequence)
         input-signal 0]
    (if-not computer
      input-signal
      (let [[program pointer input output halted] (run-computer computer input-signal)]
        (recur (if halted computers
                          (conj (into [] computers)
                                [program pointer input]))
               output)))))

(defn solve-part2 []
  (apply max (let [program    (core/read-program "input-day7")
                   all-phases (permutations [5 6 7 8 9])]
               (map
                 (fn [phase-seq]
                   (calculate-signal program phase-seq))
                 all-phases))))

(deftest part2

  (is (= 139629729
         (calculate-signal [3 26 1001 26 -4 26 3 27 1002 27 2 27 1 27 26
                            27 4 27 1001 28 -1 28 1005 28 6 99 0 0 5]
                           [9 8 7 6 5])))

  (is (= 18216
         (calculate-signal [3 52 1001 52 -5 52 3 53 1 52 56 54 1007 54 5 55 1005 55 26 1001 54
                            -5 54 1105 1 12 1 53 54 53 1008 54 0 55 1001 55 1 55 2 53 55 53 4
                            53 1001 56 -1 56 1005 56 6 99 0 0 0 0 10]
                           [9 7 8 5 6])
         ))

  (is (= 27561242 (calculate-signal
                    (core/read-program "input-day7")
                    [7 9 5 8 6])))

  (is (= 27561242 (solve-part2))))
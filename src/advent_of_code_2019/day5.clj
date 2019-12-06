(ns advent-of-code-2019.day5
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]))

;; Day 5

; given an opcode, return a tuple of the instruction and parameter modes for the instruction params
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
            output]

      ;; MULTIPLY
      "02" [(assoc program (nth parameters 2) (*
                                                (get-value 0)
                                                (get-value 1)))
            (+ 4 pointer)
            output]

      ;; STORE
      "03" [(assoc program (nth parameters 0) input)
            (+ 2 pointer)
            output]

      ;; OUTPUT
      "04" [program
            (+ 2 pointer)
            (get-value 0)]

      ;; JUMP if true
      "05" [program
            (if (not= 0 (get-value 0))
              (get-value 1)
              (+ 3 pointer))
            output]

      ;; JUMP if false
      "06" [program
            (if (= 0 (get-value 0))
              (get-value 1)
              (+ 3 pointer))
            output]

      ;; LESS THAN
      "07" [(assoc program (nth parameters 2)
                           (if (< (get-value 0) (get-value 1))
                             1 0))
            (+ 4 pointer)
            output]

      ;; Equals
      "08" [(assoc program (nth parameters 2)
                           (if (= (get-value 0) (get-value 1))
                             1 0))
            (+ 4 pointer)
            output])))

(defn compute [program input]
  (loop [program program
         pointer 0
         output  nil]
    (if (= (nth program pointer) 99)
      output
      (let [[program pointer output] (evolve program pointer input output)]
        (recur program pointer output)))))

(deftest day-5
  (testing
    "Using position mode consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not)"

    (is (= 1 (compute
               [3 9 8 9 10 9 4 9 99 -1 8]
               8)))

    (is (= 0 (compute
               [3 9 8 9 10 9 4 9 99 -1 8]
               7)))

    (is (= 1 (compute
               [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9]
               1)))

    (is (= 0 (compute
               [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9]
               0)))

    (is (= 1 (compute
               [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9]
               1)))

    (is (= 0 (compute
               [3 3 1105 -1 9 1101 0 0 12 4 12 99 1]
               0)))

    (is (= 1 (compute
               [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9]
               1)))

    (is (= 999 (compute
                 [3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                  1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                  999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99]
                 7)))

    (is (= 1000 (compute
                  [3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                   1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                   999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99]
                  8)))

    (is (= 1001 (compute
                  [3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                   1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                   999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99]
                  9)))))

(def intcodes (mapv read-string (clojure.string/split
                                  (clojure.string/trim
                                    (slurp
                                      (io/resource "input-day5")))
                                  #",")))

(deftest solution
  (is (= 15314507 (compute intcodes 1)))
  (is (= 652726 (compute intcodes 5))))

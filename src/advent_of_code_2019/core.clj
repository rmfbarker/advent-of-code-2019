(ns advent-of-code-2019.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]))

;; Day 1

(defn fuel-required [m]
  (- (quot m 3) 2))

(defn solve-day-1 []
  (reduce (fn [sum mass]
            (let [fuel (take-while pos? (drop 1 (iterate fuel-required mass)))]
              (apply + sum fuel)))
          0
          (map #(Integer/parseInt %)
               (clojure.string/split-lines
                 (slurp (io/resource "input-day1"))))))

;; Day 2
(defn day-2 []
  (let [file-input (-> (slurp (io/resource "day-2-input"))
                       clojure.string/trim
                       (clojure.string/split #","))
        program    (mapv #(Integer/parseInt %) file-input)
        evolve     (fn [instructions pos]
                     (let [[opcode arg1 arg2 arg3] (drop pos instructions)
                           cmd    (if (= opcode 1) + *)
                           result (cmd (get instructions arg1)
                                       (get instructions arg2))]
                       (assoc instructions arg3 result)))
        compute    (fn [noun verb]
                     (reduce (fn [instructions pos]
                               (if (= 99 (get instructions pos))
                                 (reduced (first instructions))
                                 (evolve instructions pos)))
                             (assoc program 1 noun 2 verb)
                             (range 0 (count program) 4)))]
    (first
      (for [noun (range 99)
            verb (range 99)
            :when (= (compute noun verb)
                     19690720)]
        (+ verb (* 100 noun))))))


;; day 3

(defn move [current-position cmd]
  (let [[x y] current-position
        dir   (subs cmd 0 1)
        steps (range 1 (inc (Integer/parseInt (subs cmd 1))))]
    (for [step steps]
      (condp = dir
        "R" [(+ x step) y]
        "L" [(- x step) y]
        "U" [x (+ y step)]
        "D" [x (- y step)]))))

(defn path [cmds]
  (reduce
    (fn [path cmd]
      (concat path (move (last path) cmd)))
    [[0 0]]
    (clojure.string/split cmds #",")))

(defn distance [wire-1 wire-2]
  (let [path-1 (path wire-1)
        path-2 (path wire-2)]
    (first
      (sort
        (map
          (fn [co-ord] (+ (.indexOf path-1 co-ord)
                          (.indexOf path-2 co-ord)))
          (remove
            #{[0 0]}
            (clojure.set/intersection
              (set path-1)
              (set path-2))))))))

(defn solve []
  (apply distance (clojure.string/split-lines (slurp (io/resource "input-day3")))))


;; Day 4

(defn valid-password? [pwd]
  (and
    (= (count (str pwd)) 6)
    (< 402328 pwd 864247)
    (some #(= 2 (count %)) (partition-by identity (seq (str pwd))))
    (apply <= (map (comp #(Integer/parseInt %) str) (seq (str pwd))))))

(comment
  (count (filter valid-password? (range 402328 (inc 864247)))))

;; Day 5

; given an opcode, return a tuple of the instruction and parameter modes for the instruction params
(defn parse-opcode [x]
  (let [x (format "%02d" x)]
    [(subs x (- (count x) 2))
     (concat (map #(Integer/parseInt (str %)) (drop 2 (reverse x)))
             (repeat 0))]))

(defn evolve [program pos input]
  (let [[opcode & parameters] (drop pos program)
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
            (+ 4 pos)
            input]

      ;; MULTIPLY
      "02" [(assoc program (nth parameters 2) (*
                                                (get-value 0)
                                                (get-value 1)))
            (+ 4 pos)
            input]

      ;; STORE
      "03" [(assoc program (nth parameters 0) input)
            (+ 2 pos)
            input]

      ;; OUTPUT
      "04" [program
            (+ 2 pos)
            (get-value 0)]

      ;; JUMP if true
      "05" (if (not= 0 (get-value 0))
             [program
              (get-value 1)
              input]
             [program
              (+ 3 pos)
              input])

      ;; JUMP if false
      "06" (if (= 0 (get-value 0))
             [program
              (get-value 1)
              input]
             [program
              (+ 3 pos)
              input])

      ;; LESS THAN
      "07" [(assoc program (nth parameters 2)
                           (if (< (get-value 0) (get-value 1))
                             1 0))
            (+ 4 pos)
            input]

      ;; Equals
      "08" [(assoc program (nth parameters 2)
                           (if (= (get-value 0) (get-value 1))
                             1 0))
            (+ 4 pos)
            input])))

(defn compute [program input]
  (loop [program program
         pos     0
         input   input]
    (if (= (nth program pos) 99)
      input
      (let [[program pos input] (evolve program pos input)]
        (recur program pos input)))))

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

(comment
  ;; output solutions for day 5
  (let [intcodes (mapv read-string (clojure.string/split
                                     (clojure.string/trim
                                       (slurp
                                         (io/resource "input-day5")))
                                     #","))]
    (compute intcodes 1)
    (compute intcodes 5))

  )
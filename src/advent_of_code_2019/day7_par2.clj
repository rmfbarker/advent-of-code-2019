(ns advent-of-code-2019.day7-par2
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

      ;; INPUT value, pause
      "03" [(assoc program (nth parameters 0) (first input))
            (+ 2 pointer)
            (into [] (rest input))
            output]

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
    (fn [output phase]
      (compute
        program
        [phase output]))
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
  (is (= 54321 (solve [0, 1, 2, 3, 4] [3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23, 101, 5, 23, 23, 1, 24, 23, 23, 4, 23, 99, 0, 0])))

  (is (= 65210 (solve [1, 0, 4, 3, 2] [3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33, 1002, 33, 7, 33, 1, 33, 31, 31, 1, 32, 31, 31, 4, 31, 99, 0, 0, 0]))))

(deftest day7-part1
  (is (= 17440 (let [program    (read-program "input-day7")
                     all-phases (permutations [0 1 2 3 4])]
                 (first (reverse (sort (map #(solve % program) all-phases))))))))

(comment

  (let [program    (read-program "input-day7")
        all-phases (permutations [0 1 2 3 4])]
    (first (reverse (sort (map #(solve % program) all-phases))))
    )

  (let [program    (read-program "input-day7")
        all-phases (permutations [5 6 7 8 9])]
    (first (reverse (sort (map #(solve % program) all-phases))))
    )


  (is (= 139629729
         (solve-day-2 [3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26, 27, 4, 27, 1001, 28, -1, 28, 1005, 28, 6, 99, 0, 0, 5]
                      [9, 8, 7, 6, 5])))

  ;; run computer till output
  (loop [program example-program-1
         pointer 0
         input   [9 0]
         output  nil]
    (cond
      (= (nth program pointer) 99) [program pointer input output true] ;; terminate
      output [program pointer input output halted]          ;; pass output to next amplifier/computer, capture current state
      :else (let [[program pointer input output] (evolve program pointer input output)]
              (recur program pointer input output halted))))

  )


(defn init-computer [program phase computer-name]
  [program 0 [phase] computer-name nil])

(defn run-computer [computer amp-input]
  (let [[program pointer input-buffer computer-name] computer]
    (loop [program program
           pointer pointer
           input   (conj (into [] input-buffer) amp-input)
           output  nil]
      (cond
        (= (nth program pointer) 99) [program pointer input output true computer-name] ;; terminate
        output [program pointer input output false computer-name] ;; pass output to next amplifier/computer, capture current state
        :else (let [[program pointer input output] (evolve program pointer input nil)]
                (recur program pointer input output))))))

(defn init-computers [program phase-sequence]
  (mapv #(init-computer program
                        (first %1) (second %1))
        (map vector
             phase-sequence
             ["A" "B" "C" "D" "E"])))

(defn calculate-signal [program phase-sequence]
  (loop [[computer & computers] (init-computers program phase-sequence)
         input-signal 0]
    (if-not computer
      input-signal
      (let [computer-state (run-computer computer input-signal)
            [program pointer input output halted computer-name] computer-state]
        (recur (if halted
                 computers
                 (conj (into [] computers) [program pointer input computer-name output]))
               output)))))

(deftest part2

  (is (= 139629729
         (calculate-signal [3 26 1001 26 -4 26 3 27 1002 27 2 27 1 27 26
                            27 4 27 1001 28 -1 28 1005 28 6 99 0 0 5]
                           [9 8 7 6 5])
         ))

  (is (= 18216
         (calculate-signal [3 52 1001 52 -5 52 3 53 1 52 56 54 1007 54 5 55 1005 55 26 1001 54
                            -5 54 1105 1 12 1 53 54 53 1008 54 0 55 1001 55 1 55 2 53 55 53 4
                            53 1001 56 -1 56 1005 56 6 99 0 0 0 0 10]
                           [9 7 8 5 6])
         )))

(defn solve-part2 []
  (apply max (let [program    (read-program "input-day7")
                   all-phases (permutations [5 6 7 8 9])]
               (map
                 (fn [phase-seq]
                   (calculate-signal program phase-seq))
                 all-phases
                 )))
  )

(deftest part2
  (is (= 27561242 (solve-part2))))
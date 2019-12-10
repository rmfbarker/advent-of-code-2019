(ns advent-of-code-2019.day9
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]))

(defn read-program [filename]
  (mapv read-string
        (-> filename
            io/resource
            slurp
            clojure.string/trim
            (clojure.string/split #","))))

(defn parse-opcode [x]
  (let [x (format "%02d" x)]
    [(subs x (- (count x) 2))
     (concat (map #(Integer/parseInt (str %)) (drop 2 (reverse x)))
             (repeat 0))]))

(defn evolve [program pointer relative-base input]
  (let [opcode        (get program pointer)
        [instruction modes] (parse-opcode opcode)
        get-parameter (fn [pos]
                        (let [mode  (nth modes pos)
                              param (get program (+ (inc pos) pointer))
                              value (condp = mode
                                      0 param
                                      ;1 param
                                      2 (+ relative-base param))]
                          value)
                        )
        get-value     (fn [pos]
                        (let [mode  (nth modes pos)
                              param (get program (+ (inc pos) pointer))
                              value (condp = mode
                                      0 (get program param 0)
                                      1 param
                                      2 (get program (+ relative-base param) 0))]
                          value))]
    (condp = instruction
      ;; ADD
      "01" (do
             [(assoc program (get-parameter 2) (+
                                                 (get-value 0)
                                                 (get-value 1)))
              (+ 4 pointer)
              relative-base
              input
              nil])

      ;; MULTIPLY
      "02" [(assoc program (get-parameter 2) (*
                                               (get-value 0)
                                               (get-value 1)))
            (+ 4 pointer)
            relative-base
            input
            nil]

      ;; INPUT value, pause if nothing in buffer
      "03" [(assoc program (get-parameter 0) (first input))
            (+ 2 pointer)
            relative-base
            (rest input)
            nil]

      ;; OUTPUT value, stop; should we be conj'ing to the output?
      "04" (do
             [program
              (+ 2 pointer)
              relative-base
              input
              (get-value 0)])

      ;; JUMP if true
      "05" [program
            (if (not= 0 (get-value 0))
              (get-value 1)
              (+ 3 pointer))
            relative-base
            input
            nil]

      ;; JUMP if false
      "06" [program
            (if (= 0 (get-value 0))
              (get-value 1)
              (+ 3 pointer))
            relative-base
            input
            nil]

      ;; LESS THAN
      "07" [(assoc program (get-parameter 2)
                           (if (< (get-value 0) (get-value 1))
                             1 0))
            (+ 4 pointer)
            relative-base
            input
            nil]

      ;; Equals
      "08" [(assoc program (get-parameter 2)
                           (if (= (get-value 0) (get-value 1))
                             1 0))
            (+ 4 pointer)
            relative-base
            input
            nil]

      ;; adjust relative base
      "09" (do
             [program
              (+ 2 pointer)
              (+ relative-base (get-value 0))
              input
              nil])
      )))

(defn compute [program input]
  (loop [program       (into {} (map-indexed vector program))
         pointer       0
         relative-base 0
         input         input
         outputs       []]
    (if (= (get program pointer) 99)
      outputs
      (let [[program pointer relative-base input output] (evolve program pointer relative-base input)]
        (recur program pointer relative-base input (if output (conj outputs output) outputs))))))

(deftest examples
  (is (= [109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99]
         (compute [109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99] nil)))

  (is (= 16 (count (str (first (compute [1102, 34915192, 34915192, 7, 4, 7, 99, 0] nil))))))

  (let [prog [104, 1125899906842624, 99]]
    (is (= (second prog)
           (first (compute prog nil)))))
  )

(deftest solution

  (is (= [2789104029] (compute (read-program "input-day9") [1])))
  (is (= [32869] (compute (read-program "input-day9") [2]))))
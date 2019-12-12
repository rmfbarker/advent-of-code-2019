(ns advent-of-code-2019.core
  (:require [clojure.java.io :as io]))

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

        get-parameter (fn [pos] (get program (+ (inc pos) pointer)))
        mode          (fn [pos] (nth modes pos))

        write-memory  (fn [pos value]
                        (let [param (get-parameter pos)]
                          (assoc program (condp = (mode pos)
                                           0 param
                                           ;1 param
                                           2 (+ relative-base param)) value)))

        read-memory   (fn [pos]
                        (let [param (get-parameter pos)]
                          (condp = (mode pos)
                            0 (get program param 0)
                            1 param
                            2 (get program (+ relative-base param) 0))))]

    (condp = instruction
      ;; ADD
      "01" [(write-memory 2 (+ (read-memory 0)
                               (read-memory 1)))
            (+ 4 pointer)
            relative-base
            input
            nil]

      ;; MULTIPLY
      "02" [(write-memory 2 (* (read-memory 0)
                               (read-memory 1)))
            (+ 4 pointer)
            relative-base
            input
            nil]

      ;; INPUT value, pause if nothing in buffer
      "03" [(write-memory 0 (first input))
            (+ 2 pointer)
            relative-base
            (rest input)
            nil]

      ;; OUTPUT value, stop; should we be conj'ing to the output?
      "04" [program
            (+ 2 pointer)
            relative-base
            input
            (read-memory 0)]

      ;; JUMP if true
      "05" [program
            (if (not= 0 (read-memory 0))
              (read-memory 1)
              (+ 3 pointer))
            relative-base
            input
            nil]

      ;; JUMP if false
      "06" [program
            (if (= 0 (read-memory 0))
              (read-memory 1)
              (+ 3 pointer))
            relative-base
            input
            nil]

      ;; LESS THAN
      "07" [(write-memory 2
                          (if (< (read-memory 0) (read-memory 1))
                            1 0))
            (+ 4 pointer)
            relative-base
            input
            nil]

      ;; Equals
      "08" [(write-memory 2
                          (if (= (read-memory 0) (read-memory 1))
                            1 0))
            (+ 4 pointer)
            relative-base
            input
            nil]

      ;; adjust relative base
      "09" [program
            (+ 2 pointer)
            (+ relative-base (read-memory 0))
            input
            nil])))

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
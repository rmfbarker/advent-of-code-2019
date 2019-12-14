(ns advent-of-code-2019.day1
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.set]
            [clojure.test :refer :all]
            [advent-of-code-2019.intcode :as core]))

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

(deftest day2-test
  (is (= 3146 (day-2))))

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


(ns advent-of-code-2019.day11
  (:require [advent-of-code-2019.intcode :as intcode]))

(def intcode (common/read-program "input-day11"))

(defn get-colour [hull-colours coord]
  (get hull-colours coord :black))

(defn compute [program input]
  (loop [program           (into {} (map-indexed vector program))
         pointer           0
         relative-base     0
         input             input
         outputs           []
         current-position  [0 0]
         current-direction :up
         hull-colours      {}]
    (println outputs)
    (cond
      (= (count outputs) 2)
      (= (get program pointer) 99) "halted"
      :else
      (let [[program pointer relative-base input output]
            (common/evolve program pointer relative-base input)]
        (println "output" output)
        (recur program pointer relative-base input (if output (conj outputs output) outputs) nil nil nil)))))



(ns advent-of-code-2019.day11
  (:require [advent-of-code-2019.intcode :as intcode]))

(def intcode (intcode/read-program "input-day11"))

(defn get-colour [hull-colours coord]
  (get hull-colours coord :black))

(defn turn [current-dir turn-dir]
  (get-in
    {:up    {:left :left :right :right}
     :left  {:left :down :right :up}
     :down  {:left :right :right :left}
     :right {:left :up :right :down}}
    [current-dir turn-dir]))

(defn move [[x y] dir]
  (condp = dir
    :left [(dec x) y]
    :right [(inc x) y]
    :up [x (dec y)]
    :down [x (inc y)]))

(defn compute []
  (loop [program       (into {} (map-indexed vector intcode))
         pointer       0
         relative-base 0
         input         [0]
         outputs       []
         current-pos   [0 0]
         current-dir   :up
         hull-colours  {}]

    (cond
      (= (count outputs) 2)
      (let [[colour direction] outputs
            turning      (if (= 0 direction) :left :right)
            new-dir      (turn current-dir turning)
            new-pos      (move current-pos new-dir)
            panel-colour (if (= 0 colour) "black" "white")]

        (str "paint panel " panel-colour
             ", turn " turning
             ", new direction " new-dir
             ", now at " new-pos
             ", hull colours" (assoc hull-colours current-pos panel-colour)))

      (= (get program pointer) 99) "halted"

      :else
      (let [[program pointer relative-base input output]
            (intcode/evolve program pointer relative-base input)]
        (println "output" output)
        (recur program
               pointer
               relative-base
               input
               (if output (conj outputs output) outputs)
               current-pos
               current-dir
               nil)))))
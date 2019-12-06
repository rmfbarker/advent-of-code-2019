(ns advent-of-code-2019.day6
  (:require [clojure.java.io :as io]))

(let [orbits  (into {} (map
                         (fn [orbit] (vec (reverse (clojure.string/split orbit #"\)"))))
                         (clojure.string/split-lines "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L")))
      planets (keys orbits)
      ]
  (reduce
    +
    (map
      #(dec (count (take-while identity (iterate orbits %))))
      planets))
  )


(let [orbits  (into {} (map
                         (fn [orbit] (vec (reverse (clojure.string/split orbit #"\)"))))
                         (clojure.string/split-lines (slurp (io/resource "input-day6")))))
      planets (keys orbits)
      ]
  (reduce
    +
    (map
      #(dec (count (take-while identity (iterate orbits %))))
      planets))
  )
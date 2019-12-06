(ns advent-of-code-2019.day6
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]))

(def test-input "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L")
(def file-input (slurp (io/resource "input-day6")))

(defn parse-orbits [orbits]
  (into {} (map
             (fn [orbit]
               (vec (reverse (clojure.string/split orbit #"\)"))))
             (clojure.string/split-lines orbits))))

(defn total-orbits [data]
  (let [orbits  (parse-orbits data)
        planets (keys orbits)]
    (reduce
      +
      (map
        #(dec (count (take-while identity (iterate orbits %))))
        planets))))

(deftest day6
  (is (= 42 (total-orbits test-input)))
  (is (= 142497 (total-orbits file-input))))
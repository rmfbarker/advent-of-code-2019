(ns advent-of-code-2019.day6
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]))

(def test-input "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L")
(def test-input-2 "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN")
(def file-input (slurp (io/resource "input-day6")))

(defn parse-orbits [orbits]
  (into {} (map
             (fn [orbit]
               (vec (reverse (clojure.string/split orbit #"\)"))))
             (clojure.string/split-lines orbits))))

(defn path-to-com [orbits planet]
  (take-while identity
              (iterate orbits planet)))

(defn path-to-planet [orbits planet path-to-planet]
  (take-while #(not= path-to-planet %)
              (iterate orbits planet)))

(defn distance-from-com [orbits planet]
  (dec
    (count
      (path-to-com orbits planet))))

(defn total-orbits [data]
  (let [orbits  (parse-orbits data)
        planets (keys orbits)]
    (reduce
      +
      (map
        (partial distance-from-com orbits)
        planets))))

(defn common-orbit [orbits x y]
  (first
    (last
      (take-while
        #(apply = %)
        (map vector
             (reverse (path-to-com orbits x))
             (reverse (path-to-com orbits y)))))))

(defn orbital-transfers [orbits x y]
  (let [common (common-orbit orbits x y)]
    (+ (dec (count (path-to-planet orbits x common)))
       (dec (count (path-to-planet orbits y common)))
       )))

(deftest day6
  (is (= 42 (total-orbits test-input)))
  (is (= 142497 (total-orbits file-input)))
  (is (= "D" (common-orbit (parse-orbits test-input-2) "YOU" "SAN")))
  (is (= 4 (orbital-transfers (parse-orbits test-input-2) "YOU" "SAN")))
  )

(comment

  (def test-orbits-2 (parse-orbits test-input-2))
  (orbital-transfers (parse-orbits file-input) "YOU" "SAN")
  )
(ns advent-of-code-2019.day6
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]))

(def test-input "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L")
(def test-input-2 "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN")
(def file-input (slurp (io/resource "input-day6")))

(defn parse-orbits [orbits]
  (->> (clojure.string/split-lines orbits)
       (map #(-> (clojure.string/split % #"\)") reverse vec))
       (into {})))

(defn path-to [orbits from to]
  (take-while #(not= to %)
              (iterate orbits from)))

(defn distance-from [orbits from planet]
  (count
    (path-to orbits planet from)))

(defn total-orbits [data]
  (let [orbits  (parse-orbits data)
        planets (keys orbits)]
    (reduce + (map
                (partial distance-from orbits "COM")
                planets))))

(defn common-orbit [orbits x y]
  (first
    (last
      (take-while
        #(apply = %)
        (map vector
             (reverse (path-to orbits x "COM"))
             (reverse (path-to orbits y "COM")))))))

(defn orbital-transfers [orbits x y]
  (let [common (common-orbit orbits x y)]
    (+ (dec (distance-from orbits common x))
       (dec (distance-from orbits common y)))))

(deftest day6
  (is (= 42 (total-orbits test-input)))
  (is (= 142497 (total-orbits file-input)))
  (is (= "D" (common-orbit (parse-orbits test-input-2) "YOU" "SAN")))
  (is (= 4 (orbital-transfers (parse-orbits test-input-2) "YOU" "SAN")))
  (is (= 301 (orbital-transfers (parse-orbits file-input) "YOU" "SAN")))
  )

(comment

  (def test-orbits-2 (parse-orbits test-input-2))
  (orbital-transfers (parse-orbits file-input) "YOU" "SAN")
  )
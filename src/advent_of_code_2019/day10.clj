(ns advent-of-code-2019.day10
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]))

(defn angle [c1 c2]
  (Math/atan2 (- (second c2) (second c1))
              (- (first c2) (first c1))))

(deftest angles
  (is (= 0.0 (angle [0 0] [1 0])))
  (is (= 0.0 (angle [1 0] [2 0])))
  (is (= (/ Math/PI 4) (angle [0 0] [1 1])))
  (is (= (/ Math/PI 4) (angle [1 1] [2 2])))
  (is (= (/ Math/PI 2) (angle [0 0] [0 1])))
  (is (= (* -1 (/ Math/PI 2)) (angle [0 1] [0 0])))
  )

(defn asteroids [input]
  (for [[y line] (map-indexed vector (clojure.string/split-lines input))
        [x obj] (map-indexed vector line)
        :when (= \# obj)]
    [x y]))

(defn visible-asteroids [co-ord asteroids]
  (count
    (distinct
      (let [other-asteroids (remove #{co-ord} asteroids)]
        (map (partial angle co-ord) other-asteroids)))))

(deftest examples
  (let [asteroids (asteroids (slurp (io/resource "input-day10")))]
    (is (= 274 (first (last (sort-by first (map (fn [ast] [(visible-asteroids ast asteroids) ast])
                                                asteroids)))))))
  )
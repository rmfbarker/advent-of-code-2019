(ns advent-of-code-2019.day10
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]))

(defn angle [c1 c2]
  (mod (+ (Math/atan2 (- (second c2) (second c1))
                      (- (first c2) (first c1)))
          (* Math/PI 2.5))
       (* 2 Math/PI)))

(deftest angles
  (is (= 0.0 (angle [0 0] [0 -1])))                         ;; straight up
  (is (= (/ Math/PI 2) (angle [1 0] [2 0])))                ;; due east
  (is (= (* Math/PI 1.5) (angle [2 0] [1 0])))              ;; due west
  ;(is (== (* 3 (/ Math/PI 4)) (angle [0 0] [1 1])))          ;; down 45deg to the east
  ;(is (= (* 3 (/ Math/PI 4)) (angle [1 1] [2 2])))          ;; same as above
  (is (= Math/PI (angle [0 0] [0 1])))
  (is (= 4.8120576328758515 (angle [11 13] [1 12])))
  (is (= 0.0 (angle [0 1] [0 0]))))

(defn parse-asteroids [input]
  (for [[y line] (map-indexed vector (clojure.string/split-lines input))
        [x obj] (map-indexed vector line)
        :when (= \# obj)]
    [x y]))

(defn visible-asteroids [co-ord asteroids]
  (count
    (distinct
      (let [other-asteroids (remove #{co-ord} asteroids)]
        (map (partial angle co-ord) other-asteroids)))))

(defn best-position [asteroids]
  (first (sort-by first >
                  (map (fn [ast]
                         [(visible-asteroids ast asteroids) ast])
                       asteroids))))

(def example-input-3 ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##")
(def real-input (slurp (io/resource "input-day10")))

(deftest examples
  (let [asteroids (parse-asteroids (slurp (io/resource "input-day10")))
        [count coord] (best-position asteroids)]
    (is (= 274 count))
    (is (= [19 14] coord)))
  )

(defn distance-between [c1 c2]
  (Math/sqrt (+ (Math/pow (- (first c2) (first c1)) 2)
                (Math/pow (- (second c2) (second c1)) 2))))

(deftest distances
  (is (= 1.0 (distance-between [0 0] [0 1])))
  (is (= 5.0 (distance-between [0 0] [3 4])))
  )

(defn order-asteroids [station-coord]
  (let [asteroids       (parse-asteroids real-input)

        other-asteroids (remove #{station-coord} asteroids)]
    (map second (sort-by first
                         (map
                           (fn [[bearing asteroids]]
                             [bearing (map first (sort-by #(distance-between station-coord (first %))
                                                          asteroids))])
                           (group-by second
                                     (map #(vector % (angle station-coord %)) other-asteroids)))))))

(def ordered-asteroids (order-asteroids [19 14]))

;; Part 2

(def best-position )

(defn get-vaporized [n]
  (second (nth (sort
                 (fn [x y]
                   (compare [(second (first x)) (ffirst x)]
                            [(second (first y)) (ffirst y)]))

                 (for [x (range (count ordered-asteroids))
                       y (range (count (nth ordered-asteroids x)))]
                   [[x y] (nth (nth ordered-asteroids x) y)]))
               (dec n))))

(= [3 5] (get-vaporized 200))

(comment

  (group-by second
            (let [asteroids       (parse-asteroids example-input-3)
                  co-ord          [11 13]
                  other-asteroids (remove #{co-ord} asteroids)]
              (map #(vector % (angle co-ord %)) other-asteroids)))

  (sort-by
    #(distance-between [11 13] (first %))
    (second (first
              (let [asteroids       (parse-asteroids example-input-3)
                    co-ord          [11 13]
                    other-asteroids (remove #{co-ord} asteroids)]
                (group-by second
                          (map #(vector % (angle co-ord %)) other-asteroids))))))


  (def ordered-asteroids
    (let [asteroids       (parse-asteroids example-input-3)
          co-ord          [11 13]
          other-asteroids (remove #{co-ord} asteroids)]
      (map second (sort-by first
                           (map
                             (fn [[bearing asteroids]]
                               (println bearing asteroids)
                               [bearing (map first (sort-by #(distance-between co-ord (first %))
                                                            asteroids))])
                             (group-by second
                                       (map #(vector % (angle co-ord %)) other-asteroids)))))))




  (second (sort
            (fn [x y]
              (compare [(second (first x)) (ffirst x)]
                       [(second (first y)) (ffirst y)]))

            (for [x (range (count ordered-asteroids))
                  y (range (count (nth ordered-asteroids x)))]
              [[x y] (nth (nth ordered-asteroids x) y)])))



  )
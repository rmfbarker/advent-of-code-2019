(ns advent-of-code-2019.day8
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]))

(deftest day8-part1

  (is (= 1862
         (let [most-zeroes (first
                             (sort-by
                               #(get % \0)
                               (map
                                 #(group-by identity %)
                                 (partition
                                   (* 25 6)
                                   (clojure.string/trim (slurp (io/resource "input-day8")))))))]
           (* (count (get most-zeroes \1))
              (count (get most-zeroes \2)))))))

;; solution is 'GCPHL'
(defn part-2 []
  (let [width            25
        height           6
        pixels-per-image (* width height)
        image-file       (clojure.string/trim (slurp (io/resource "input-day8")))]
    (doseq [line (map #(apply str %)
                      (partition width
                                 (map
                                   (fn [pixels] (let [pix (first (drop-while #{\2} pixels))]
                                                  (if (= pix \0) " " "x")))
                                   (apply map
                                          vector
                                          (partition pixels-per-image
                                                     image-file)))))]
      (println line))))
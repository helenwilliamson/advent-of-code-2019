(ns advent-of-code-2019.puzzle-1
  (:gen-class)
  (:require [clojure.string :as str]))

(defn fuel
  [mass]
  (- (quot mass 3) 2))

(defn read-input
  []
  (->> (slurp "resources/puzzle-1")
       (str/split-lines)
       (map #(Integer/parseInt %1))))

(defn total-fuel
  []
  (->> (read-input)
       (map fuel)
       (reduce +)))

(defn fuel-with-fuel
  [mass]
  (loop [current-fuel 0
         current-mass mass]
    (let [next-fuel (fuel current-mass)]
      (if (<= next-fuel 0)
        current-fuel
        (recur (+ next-fuel current-fuel) next-fuel)))))

(defn total-fuel-with-fuel
  []
  (->> (read-input)
       (map fuel-with-fuel)
       (reduce +)))

(ns advent-of-code.puzzle-3
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.set :as st]))

(def dummy
  [[{:direction "R" :length 8}
    {:direction "U" :length 5}
    {:direction "L" :length 5}
    {:direction "D" :length 3}]
   [{:direction "U" :length 7}
    {:direction "R" :length 6}
    {:direction "D" :length 4}
    {:direction "L" :length 4}]])

(defn read-input
  []
  (->> (slurp "resources/puzzle-3")
       (str/split-lines)
       (map
        (fn [line]
          (->> (str/split line #",")
               (map #(hash-map
                      :direction (subs %1 0 1)
                      :length (Integer/parseInt (subs %1 1)))))))))

(defn up
  [length x y]
  (for [ys (reverse (range (+ 1 y) (+ 1 y length)))]
    {:x x :y ys}))

(defn down
  [length x y]
  (for [ys (range (- y length) y)]
    {:x x :y ys}))

(defn left
  [length x y]
  (for [xs (range (- x length) x)]
    {:x xs :y y}))

(defn right
  [length x y]
  (for [xs (reverse (range (+ 1 x) (+ 1 x length)))]
    {:x xs :y y}))

(defn draw
  [grid point {:keys [direction length]}]
  (let [{:keys [x y]} point]
    ;(println length x y)
    (case direction
      "U" (up length x y)
      "D" (down length x y)
      "L" (left length x y)
      "R" (right length x y))))

(defn lay-wire-pieces
  [input]
  (loop [wire-pieces input
         grid #{}
         point {:x 0 :y 0}]
    (let [[wire-piece & remaining-wire-pieces] wire-pieces
          next-line (draw grid point wire-piece)
          next-point (first next-line)
          next-grid (st/union grid (into #{} next-line))]
      ;(println wire-piece)
      ;(println next-line)
      ;(println next-grid)
      ;(println next-point)
      ;(println)
      (if (empty? remaining-wire-pieces)
        (do
          ;(println next-grid)
          next-grid)
        (recur remaining-wire-pieces next-grid next-point)))))

(defn closed-crossing-by-taxi-distance
  [input]
  (let [red-points (lay-wire-pieces (first input))
        blue-points (lay-wire-pieces (second input))
        crossing-points (disj (st/intersection red-points blue-points) {:x 0 :y 0})]
    (->> crossing-points
         (map #(+ (Math/abs (:x %1)) (Math/abs (:y %1))))
         sort
         first)))

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

(def dummy2
  ["R75,D30,R83,U83,L12,D49,R71,U7,L72"
   "U62,R66,U55,R34,D71,R55,D58,R83"])

(def dummy3
  ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
   "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"])

(defn read-input
  ([] (-> (slurp "resources/puzzle-3")
          str/split-lines
          read-input))
  ([in]
   (->> in
        (map
         (fn [line]
           (->> (str/split line #",")
                (map #(hash-map
                       :direction (subs %1 0 1)
                       :length (Integer/parseInt (subs %1 1))))))))))

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
    (case direction
      "U" (up length x y)
      "D" (down length x y)
      "L" (left length x y)
      "R" (right length x y))))

(defn lay-wire-pieces
  [input]
  (loop [wire-pieces input
         grid []
         point {:x 0 :y 0}]
    (let [[wire-piece & remaining-wire-pieces] wire-pieces
          next-line (draw grid point wire-piece)
          next-point (first next-line)
          next-grid (concat grid (reverse next-line))]
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
        crossing-points (disj (st/intersection (into #{} red-points) (into #{} blue-points)) {:x 0 :y 0})]
    (->> crossing-points
         (map #(+ (Math/abs (:x %1)) (Math/abs (:y %1))))
         sort
         first)))

(defn closed-crossing-by-steps
  [input]
  (let [red-points (lay-wire-pieces (first input))
        blue-points (lay-wire-pieces (second input))
        crossing-points (disj (st/intersection (into #{} red-points) (into #{} blue-points)) {:x 0 :y 0})]
    (->> crossing-points
         (map #(+ 2 (.indexOf red-points %1) (.indexOf blue-points %1)))
         sort
         ;first
         )))

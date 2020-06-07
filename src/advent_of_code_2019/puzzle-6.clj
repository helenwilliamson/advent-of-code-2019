(ns advent-of-code-2019.puzzle-6
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.set :as st]))

(def test-input
  '("COM)B" "B)C" "C)D" "D)E" "E)F""B)G""G)H""D)I""E)J""J)K""K)L"))

(def test-input2
  '("COM)B" "C)D" "D)E" "E)F" "B)G" "G)H" "D)I" "E)J" "J)K" "K)L" "K)YOU" "I)SAN"))

(defn read-input
  ([] (read-input (->> (slurp "resources/puzzle-6")
       (str/split-lines)
       )))
  ([lines]
   (->> lines
        (map #(str/split %1 #"\)"))
        (reduce (fn [orbits points]
                  (merge-with into orbits {(second points) (first points)}))
                {}))))

(defn orbits
  [input]
  (->> (keys input)
       (map #(take-while some?
                         (iterate (fn [value] (get input value)) %1)))
       (map #(- (count %1) 1))
       (reduce +)))

(defn distance-between
  [input]
  (let [orbits (->> ["SAN" "YOU"]
                    (map #(take-while some?
                                      (iterate (fn [value] (get input value)) %1)))
                    (map #(drop 1 %1)))
        one (first orbits)
        two (second orbits)
        sets (map #(into #{} %1) orbits)
        common-points (st/intersection (first sets) (second sets))
        distance-for (fn [point coll] (count (take-while #(not= point %1) coll)))
        distances (map #(+ (distance-for %1 one) (distance-for %1 two)) common-points)
        ]
    (apply min distances)))

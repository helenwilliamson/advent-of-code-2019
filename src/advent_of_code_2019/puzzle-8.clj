(ns advent-of-code-2019.puzzle-8
  (:gen-class)
  (:require [clojure.string :as str]))

(defn read-input
  [] (slurp "resources/puzzle-8"))

(def test-input "123456789012")

(def test-input2 "0222112222120000")

(defn number-of
  [layer value]
  (count (filter #(= value %1) layer)))

(defn grid
  [size lines]
  (->> lines
       (map #(Integer/parseInt (str %1)))
       (partition size size lines)))

(defn fewest-zeros
  [lines size]
  (let [matching (->> lines
                      (grid size)
                      (map (fn [layer] {:layer layer :zeros (number-of layer 0)}))
                      (sort-by :zeros <)
                      first)]
    (* (number-of (:layer matching) 1) (number-of (:layer matching) 2))))

(defn mix
  [pixels]
  (let [[bottom top] pixels]
    (if (= 2 top)
        bottom
        top)))

(defn colorise
  [lines size]
  (let [result
        (->> lines
             (grid size)
             reverse
             (reduce (fn [picture layer]
                       (map #(mix %1) (partition 2 2 (interleave picture layer)))))
             (map #(if (= 1 %1) 1 " "))
             (partition 25 25)
             (map str/join))]
    (doseq [row result]
      (println row))))

(ns advent-of-code-2019.puzzle-2
  (:gen-class)
  (:require [clojure.string :as str]))

(defn read-input
  []
  (into [] (map #(Integer/parseInt %1)
                (-> (slurp "resources/puzzle-2")
                    (str/split #",")))))

(defn run
  [program]
  (loop [current program
         pointer 0]
    (let [[opcode position-input1 position-input2 position] (drop pointer current)]
      (if (or  (= opcode 99) (not (or (= opcode 1) (= opcode 2))))
        current
        (let [input1 (current position-input1)
              input2 (current position-input2)
              func (if (= opcode 1) + *)
              value (func input1 input2)]
          (recur (assoc current position value) (+ 4 pointer)))))))

(defn replace-state
  [program noun verb]
  (-> program
      (assoc 1 noun)
      (assoc 2 verb)))

(defn intcode
  ([] (intcode 12 2))
  ([noun verb]
   (-> (read-input)
       (replace-state noun verb)
       run
       first)))

(defn exploring-intcode
  [upper-noun upper-verb]
  (for [noun (range 0 upper-noun)
        verb (range 0 upper-verb)]
    {:noun noun :verb verb :result (intcode noun verb)}))

(defn find-matching
  [upper-noun upper-verb]
  (->> (exploring-intcode upper-noun upper-verb)
       (filter #(= 19690720 (:result %1)))
       (map #(+ (* 100 (:noun %1)) (:verb %1)))))

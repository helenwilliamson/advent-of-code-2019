(ns advent-of-code-2019.puzzle-5
  (:gen-class)
  (:require [clojure.string :as str]))

(defn read-input
  ([] (read-input "resources/puzzle-5"))
  ([file-name]
   (into [] (map #(Integer/parseInt %1)
                 (-> (slurp file-name)
                     (str/split #","))))))

(defn continuing-instruction?
  [[opcode]]
  (println "checking" opcode)
  (or (adder? opcode) (multipler? opcode) (input? opcode) (output? opcode)))

(defn immediate?
  [opcode position]
  (= 1 (digit opcode position)))

(defn do-maths
  [computer function]
  (let [[opcode position-input1 position-input2 position & rest] (:program computer)
        memory (:memory computer)
        input1 (if (immediate? opcode 2)
                 position-input1
                 (memory position-input1))
        input2 (if (immediate? opcode 3)
                 position-input2
                 (memory position-input2))
        updated-memory (assoc memory position (function input1 input2))
        updated-pointer (+ 4 (:pointer computer))]
    (println "inputs" input1 input2)
    ;(println "updated-memory" updated-memory)
    ;(println "pointer" (:pointer computer))
    ;    (println "updated-pointer" updated-pointer)
    (merge computer
           {:program (drop updated-pointer updated-memory)
            :memory updated-memory
            :pointer updated-pointer})))

(defn input
  [computer value]
  (let [[opcode position & rest] (:program computer)
        memory (:memory computer)
        updated-memory (assoc memory position value)
        updated-pointer (+ 2 (:pointer computer))]
    ;(println "updated-memory" updated-memory)
    ;    (println "pointer" (:pointer computer))
    ;        (println "updated-pointer" updated-pointer)
    (merge computer
           {:program (drop updated-pointer updated-memory)
            :memory updated-memory
            :pointer updated-pointer})))

(defn output
  [computer value]
  (let [[opcode position & rest] (:program computer)
        memory (:memory computer)
        value (if (immediate? opcode 2)
                 position
                 (memory position))
        updated-pointer (+ 2 (:pointer computer))]
    ;(println "updated-memory" updated-memory)
    ;    (println "pointer" (:pointer computer))
    ;        (println "updated-pointer" updated-pointer)
    (merge computer
           {:program (drop updated-pointer memory)
            :output (conj (:output computer) value)
            :pointer updated-pointer})))

(defn digit
  [value position]
  (nth (map #(Character/getNumericValue %1) (reverse (str value))) position 0))

(defn instruction?
  [instruction-type opcode]
  (or (= opcode instruction-type)
      (= instruction-type (digit opcode 0))))

(defn adder? [opcode] (instruction? 1 opcode))

(defn multipler? [opcode] (instruction? 2 opcode))

(defn input? [opcode] (instruction? 3 opcode))

(defn output? [opcode] (instruction? 4 opcode))

(defn run-instruction
  [computer value]
  (let [[opcode] (:program computer)]
    ;(println "running with " opcode)
    (do
      (cond
        (adder? opcode) (do-maths computer +)
        (multipler? opcode) (do-maths computer *)
        (input? opcode) (input computer value)
        (output? opcode) (output computer value)))))

(defn run
  [program value]
  (loop [data {:program program :pointer 0 :memory program :output []}]
    (println)
    (println "program" (take 4 (:program data)))
    (println "memory" (:memory data))
    (if (not (continuing-instruction? (:program data)))
      data
      (let [iter (run-instruction data value)]
        (println (:output iter))
        (recur iter)))))

(defn intcode
  ([value file-name]
   (-> (read-input file-name)
       (run value)))
  ([value]
   (-> (read-input)
       (run value))))

(defn works-as-old
  []
  (let [result (:memory (intcode 1 "resources/puzzle-2"))]
    (println result)
    (= 466644 (first result))))

(defn works-as-new
  []
  (let [result (intcode 1 "resources/puzzle-5")]
    (:output result)))

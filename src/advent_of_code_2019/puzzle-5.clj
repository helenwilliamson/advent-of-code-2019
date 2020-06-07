(ns advent-of-code-2019.puzzle-5
  (:gen-class)
  (:require [clojure.string :as str]))

(defn read-input
  ([] (read-input "resources/puzzle-5"))
  ([file-name]
   (into [] (map #(Integer/parseInt %1)
                 (-> (slurp file-name)
                     (str/split #","))))))

(defn digit
  [value position]
  (nth (map #(Character/getNumericValue %1) (reverse (str value))) position 0))

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

(defn jump
  [computer condition]
  (let [[opcode parameter1 parameter2] (:program computer)
        value1 (if (immediate? opcode 2)
                 parameter1
                 ((:memory computer) parameter1))]
    (println value1)
    (if (condition value1)
      (let [value2 (if (immediate? opcode 3)
                 parameter2
                 ((:memory computer) parameter2))
            updated-pointer (+ value2 (:pointer computer))]
        (println value2)
        (merge computer
               {:program (drop updated-pointer (:memory computer))
                :pointer updated-pointer}))
      computer)))

(defn instruction?
  [instruction-type opcode]
  (or (= opcode instruction-type)
      (= instruction-type (digit opcode 0))))

(defn adder? [opcode] (instruction? 1 opcode))

(defn multipler? [opcode] (instruction? 2 opcode))

(defn input? [opcode] (instruction? 3 opcode))

(defn output? [opcode] (instruction? 4 opcode))

(defn jump-if-true? [opcode] (instruction? 5 opcode))

(defn jump-if-false? [opcode] (instruction? 6 opcode))

(defn less-than? [opcode] (instruction? 7 opcode))

(defn equals? [opcode] (instruction? 8 opcode))

(defn continuing-instruction?
  [[opcode]]
  (println "checking" opcode)
  (or (adder? opcode)
      (multipler? opcode)
      (input? opcode)
      (output? opcode)
      (jump-if-true? opcode)
      ;(jump-if-false? opcode)
      ;(less-than? opcode)
      ;(equals? opcode)
      ))

(defn run-instruction
  [computer value]
  (let [[opcode] (:program computer)]
    ;(println "running with " opcode)
    (do
      (cond
        (adder? opcode) (do-maths computer +)
        (multipler? opcode) (do-maths computer *)
        (input? opcode) (input computer value)
        (output? opcode) (output computer value)
        (jump-if-true? opcode) (jump computer #(not= 0 %1))))))

(defn run
  [program value]
  (loop [data {:program program :pointer 0 :memory program :output []}
         times 6]
    (println)
    (println "program" (take 4 (:program data)))
    (println "memory" (:memory data))
    (println "value" value)
    (if (or (= 0 times) (not (continuing-instruction? (:program data))))
      data
      (let [iter (run-instruction data value)]
        (println (:output iter))
        (recur iter (dec times))))))

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

(defn aircon
  []
  (let [result (intcode 1 "resources/puzzle-5")]
    (:output result)))

(defn thermal-radiator
  []
  (let [result (intcode 5 "resources/puzzle-5")]
    (:output result)))

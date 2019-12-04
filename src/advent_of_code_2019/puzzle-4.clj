(ns advent-of-code.puzzle-4
  (:gen-class))

(defn adjacent-digits?
  [number]
  (->> (.toString number)
       (partition 2 1)
       (some #(= (first %1) (second %1)))))

(defn two-not-three-adjacent-digits?
  [number]
  (loop [data (->> (.toString number)
                   (map #(Integer/parseInt (str %1))))]
    (let [[one] data
          matches (take-while #(= one %1) data)]
      (if (nil? one)
        false
        (if (= 2 (count matches))
          true
          (recur (drop (count matches) data)))))))

(defn increasing?
  [number]
  (->> (.toString number)
       (partition 2 1)
       (map #(map (fn [x] (-> x str Integer/parseInt)) %1))
       (every? #(<= (first %1) (second %1)))))

(defn passwords
  [from to]
  (->> (range from (+ 1 to))
       (filter #(and (adjacent-digits? %1) (increasing? %1)))
       count))

(defn passwords-revisited
  [from to]
  (->> (range from (+ 1 to))
       (filter #(and (two-not-three-adjacent-digits? %1) (increasing? %1)))
       count))

(ns day-08
  (:require
   [clojure.string :as str]))

; Part 1

(defn parse-lines [lines]
  (reduce (fn [acc, line]
            (let [[[_ k l r]] (re-seq #"([A-Z1-9]+)\s+=\s+\(([A-Z1-9]+),\s+([A-Z1-9]+)\)" line)]
              (assoc acc k [l r])))
          {}
          lines))

(defn find-num-steps [input]
  (let [[directions _ & lines] (str/split-lines input)
        mapping (parse-lines lines)]
    (->> directions
         seq
         cycle
         (reductions (fn [k d]
                       (if (= \L d)
                         (first (get mapping k))
                         (last (get mapping k))))
                     "AAA")
         (take-while #(not= "ZZZ" %))
         count)))

; Part 2

(defn count-steps [directions mapping start]
  (->> directions
       seq
       cycle
       (reductions (fn [k d]
                     (if (= \L d)
                       (first (get mapping k))
                       (last (get mapping k))))
                   start)
       (take-while #(not (re-matches #"(.)*Z" %)))
       count))

(defn least-commong-multiple [x]
  (let [gcd #(if (= 0 %2) %1 (recur %2 (mod %1 %2)))
        lcm #(/ (* %1 %2) (gcd %1 %2))]
    (reduce lcm x)))

(defn find-num-steps' [input]
  (let [[directions _ & lines] (str/split-lines input)
        mapping (parse-lines lines)
        num-steps (->> mapping
                       keys
                       (filter #(re-matches #"(.)*A" %))
                       (map #(count-steps directions mapping %)))]
    (least-commong-multiple num-steps)))
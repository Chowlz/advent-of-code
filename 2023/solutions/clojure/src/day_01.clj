(ns day-01
  (:require
   [clojure.string :as str]))

; Part 1

(defn find-calibration-value [line]
  (let [nums (->> line
                  seq
                  (filter #(Character/isDigit %)))]
    (Integer/parseInt (str (first nums) (last nums)))))

(defn sum-calibration-values [input]
  (->> input
       str/split-lines
       (reduce #(+ %1 (find-calibration-value %2)) 0)))

; Part 2

(def digits
  {"1" "1"
   "2" "2"
   "3" "3"
   "4" "4"
   "5" "5"
   "6" "6"
   "7" "7"
   "8" "8"
   "9" "9"
   "0" "0"
   "one" "1"
   "two" "2"
   "three" "3"
   "four" "4"
   "five" "5"
   "six" "6"
   "seven" "7"
   "eight" "8"
   "nine" "9"})

(def re-num
  #"(?=(0|1|2|3|4|5|6|7|8|9|0|one|two|three|four|five|six|seven|eight|nine))")

(defn find-calibration-value' [line]
  (let [nums (re-seq re-num line)]
    (Integer/parseInt (str (->> nums first last (get digits))
                           (->> nums last last (get digits))))))

(defn sum-calibration-values' [input]
(->> input
     str/split-lines
     (reduce #(+ %1 (find-calibration-value' %2)) 0)))
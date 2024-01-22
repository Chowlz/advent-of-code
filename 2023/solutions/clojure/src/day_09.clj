(ns day-09
  (:require
   [clojure.string :as str]))

; Part 1

(defn parse-line [line]
  (map #(Integer/parseInt %) (str/split line #"\s+")))

(defn diff-values [values]
  (let [[f & r] values]
    (->> r
         (reduce (fn [acc i]
                   (-> acc
                       (update :diffs conj (- i (:last acc)))
                       (assoc :last i)))
                 {:diffs []
                  :last f})
         :diffs)))

(defn predict-next-values [lasts values]
  (let [[f & r :as diffs] (diff-values values)]
    (if (every? #(= f %) r)
      (reduce + (conj lasts f))
      (recur (conj lasts (last r)) diffs))))

(defn sum-extrapolated-values [input]
  (reduce + (->> input
                 str/split-lines
                 (map parse-line)
                 (map #(predict-next-values [(last %)] %)))))

; Part 2

(defn predict-prev-values [firsts values]
  (let [[f & r :as diffs] (diff-values values)]
    (if (every? #(= f %) r)
      (reduce #(- %2 %1) (reverse (conj firsts f)))
      (recur (conj firsts f) diffs))))

(defn sum-extrapolated-values' [input]
  (reduce + (->> input
                 str/split-lines
                 (map parse-line)
                 (map #(predict-prev-values [(first %)] %)))))
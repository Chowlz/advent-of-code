(ns day-06
  (:require
   [clojure.string :as str]
   [clojure.math.numeric-tower :as math]))

; Part 1

(defn parse-line [line header]
  (-> line
      (str/split header)
      last
      (str/split #"\s+")
      (as-> arr (map #(Integer/parseInt %) arr))))

(defn parse-input [input]
  (let [[time-str distance-str] input
        times (parse-line time-str #"Time:\s+")
        distances  (parse-line distance-str #"Distance:\s+")]
    (zipmap times distances)))

(defn count-win-possibilities [time distance]
  (let [sqrt (math/sqrt (- (math/expt time 2) (* 4 distance)))
        lower (as-> (/ (- time sqrt) 2) l
                (if (float? l) (math/ceil l) (inc l)))
        upper (as-> (/ (+ time sqrt) 2) u
                (if (float? u) (math/floor u) (dec u)))]
    (count (range lower (inc upper)))))

(defn multiply-win-possibilities [input]
  (transduce
   (map (fn [[time distance]] (count-win-possibilities time distance)))
   *
   (parse-input (str/split-lines input))))

; Part 2

(defn parse-line' [line-str header]
  (-> line-str
      (str/split header)
      last
      (str/split #"\s+")
      (as-> arr (apply str arr))
      Long/parseLong))

(defn parse-input' [input]
  (let [[time-str distance-str] input
        time (parse-line' time-str #"Time:\s+")
        distance  (parse-line' distance-str #"Distance:\s+")]
    [time distance]))

(defn find-win-possibilities [input]
  (apply count-win-possibilities (parse-input' (str/split-lines input))))
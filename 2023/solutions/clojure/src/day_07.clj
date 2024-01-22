(ns day-07
  (:require
   [clojure.string :as str]))

; Part 1

(defn masssage-hand-str [hand-str]
  (-> hand-str
      (str/replace #"T" ":")
      (str/replace #"J" ";")
      (str/replace #"Q" "<")
      (str/replace #"K" "=")
      (str/replace #"A" ">")))

(defn get-hand-type [hand]
  (let [groups (->> hand
                    seq
                    (group-by identity)
                    (map (fn [[k v]]
                           [k (count v)]))
                    (sort-by last))]
    (cond
      (= 1 (count groups))
      7 ; five-of-a-kind

      (= 4 (last (last groups)))
      6 ; four-of-a-kind

      (and (= 2 (count groups))
           (= 3 (last (last groups))))
      5 ; full-house

      (and (= 3 (count groups))
           (= 3 (last (last groups))))
      4 ; three-of-a-kind

      (and (= 3 (count groups))
           (= 2 (last (nth groups 1)) (last (last groups))))
      3 ; two-pair

      (and (= 4 (count groups))
           (= 2 (last (last groups))))
      2 ; one-pair

      (= 5 (count groups))
      1 ; high-card

      :else nil)))

(defn parse-line [line]
  (let [[hand bid-str] (str/split line #" ")
        hand' (masssage-hand-str hand)
        hand-type (get-hand-type hand)
        bid (Integer/parseInt bid-str)]
    [bid hand-type hand']))

(defn find-total-winnings [input]
  (reduce +
          (->> input
               str/split-lines
               (map parse-line)
               (sort-by (juxt
                         (fn [[_ type _]] type)
                         (fn [[_ _ hand']] hand')))
               (map-indexed (fn [idx [bid _ _]]
                              [(inc idx) bid]))
               (map (fn [[rank bid]] (* rank bid))))))

; Part 2

(defn masssage-hand-str' [hand-str]
  (-> hand-str
      (str/replace #"T" ":")
      (str/replace #"Q" "<")
      (str/replace #"K" "=")
      (str/replace #"A" ">")
      (str/replace #"J" "0")))

(defn get-hand-type' [hand]
  (if-not (str/includes? hand "J")
    (get-hand-type hand)
    (let [mapping (->> hand
                       seq
                       (group-by identity))
          j-count (count (get mapping \J))
          groups (->> (dissoc mapping \J)
                      (map (fn [[k v]]
                             [k (count v)]))
                      (sort-by last))]

      (condp = j-count
        5 7 ; five-of-a-kind
        4 7 ; five-of-a-kind
        3 (condp = (count groups)
            1 7 ; five-of-a-kind
            2 6 ; four-of-a-kind
            :else nil)
        2 (condp = (count groups)
            1 7 ; five-of-a-kind
            2 6 ; four-of-a-kind
            3 4 ; three-of-a-kind)
            :else nil)
        1 (condp = (count groups)
            1 7 ; five-of-a-kind
            2 (condp = (last (last groups))
                3 6 ; four-of-a-kind
                2 5 ; full-house
                :else nil)
            3 4 ; three-of-a-kind
            4 2 ; one-pair
            )))))

(defn parse-line' [line]
  (let [[hand bid-str] (str/split line #" ")
        hand' (masssage-hand-str' hand)
        hand-type (get-hand-type' hand)
        bid (Integer/parseInt bid-str)]
    [bid hand-type hand']))

(defn find-total-winnings' [input]
  (reduce +
          (->> input
               str/split-lines
               (map parse-line')
               (sort-by (juxt
                         (fn [[_ type _]] type)
                         (fn [[_ _ hand']] hand')))
               (map-indexed (fn [idx [bid _ _]]
                              [(inc idx) bid]))
               (map (fn [[rank bid]] (* rank bid))))))
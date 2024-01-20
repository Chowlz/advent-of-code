(ns day-02
  (:require
   [clojure.string :as str]))

; Part 1

(defn parse-hand [hand-str]
  (->> hand-str
       (re-seq #"\d{1,3} (?:blue|red|green)")
       (map #(str/split % #" "))
       (map (fn [[num color]]
              [(keyword color) (Integer/parseInt num)]))
       (into {})))

(defn parse-game [game-str]
  (let [[_ game-id-str hand-str] (re-find #"Game (\d+): (.*)" game-str)
        id (Integer/parseInt game-id-str)
        hands (map parse-hand (str/split hand-str #";"))]
    [id hands]))

(defn possible-game? [{:keys [red green blue]} [_ hands]]
  (every? #(and (<= (get % :red 0) red)
                (<= (get % :green 0) green)
                (<= (get % :blue 0) blue))
          hands))

(defn total-bounded-ids [input]
  (->> input
       str/split-lines
       (map parse-game)
       (filter (partial possible-game? {:red 12 :green 13 :blue 14}))
       (reduce (fn [acc [id _]] (+ acc id)) 0)))

; Part 2

(defn get-max-hand [hands]
  (reduce (fn [acc {:keys [blue green red]}]
            (cond-> acc
              (< (:blue acc) (or blue 0)) (assoc :blue blue)
              (< (:green acc) (or green 0)) (assoc :green green)
              (< (:red acc) (or red 0)) (assoc :red red)))
          {:blue 0 :green 0 :red 0}
          hands))

(defn total-product-maxes [input]
  (->> input
       str/split-lines
       (map parse-game)
       (map second)
       (map get-max-hand)
       (map vals)
       (map #(apply * %))
       (reduce + 0)))
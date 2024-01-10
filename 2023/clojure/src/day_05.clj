(ns day-05
  (:require
   [clojure.string :as str]))

; Part 1

(defn parse-seeds [seeds-str]
  (map #(Long/parseLong %) (-> seeds-str
                               (str/split #"seeds: ")
                               last
                               (str/split #" "))))

(defn parse-mapping [map-str]
  (let [[map-name data] (str/split map-str #" map:\n")]
    {(keyword map-name) (->> data
                             str/split-lines
                             (map (fn [line]
                                    (let [[des src r] (map #(Long/parseLong %) (str/split line #" "))]
                                      [[src (+ src (dec r))] des]))))}))

(defn find-des [rules v]
  (loop [rules rules]
    (if (empty? rules)
      v
      (let [[[s e] des] (first rules)]
        (if (<= s v e)
          (+ des (- v s))
          (recur (rest rules)))))))

(defn seed->location [mappings seed]
  (->> seed
       (find-des (:seed-to-soil mappings))
       (find-des (:soil-to-fertilizer mappings))
       (find-des (:fertilizer-to-water mappings))
       (find-des (:water-to-light mappings))
       (find-des (:light-to-temperature mappings))
       (find-des (:temperature-to-humidity mappings))
       (find-des (:humidity-to-location mappings))))

(defn find-lowest-location [input]
  (let [[seeds-str & map-strs] (str/split input #"\n\n")
        seeds (parse-seeds seeds-str)
        mappings (->> map-strs
                      (map parse-mapping)
                      (into {}))]
    (->> seeds
         (map (partial seed->location mappings))
         sort
         first)))

; Part 2

(defn gen-seed-ranges [seeds-str]
  (->> seeds-str
       parse-seeds
       (partition 2)
       (map (fn [[s r]] [s (+ s (dec r))]))))

(defn find-des-ranges [rules seed-range]
  (loop [rules rules
         seed-ranges [seed-range]
         next []]
    (let [[[s e] des] (first rules)
          [[x y] & rest-seed-ranges] seed-ranges]
      (if (or (empty? rules) (empty? seed-ranges))
        (concat next seed-ranges)
        (cond
          (<= s x y e)
          (recur (rest rules)
                 rest-seed-ranges
                 (conj next [(+ des (- x s)) (+ des (- y s))]))

          (<= s x e y)
          (recur (rest rules)
                 (cons [(inc e) y] rest-seed-ranges)
                 (conj next [(+ des (- x s)) (+ des (- e s))]))

          (<= x s y e)
          (recur (rest rules)
                 (cons [x (dec s)] rest-seed-ranges)
                 (conj next [des (+ des (- y s))]))

          (<= x s e y)
          (recur (rest rules)
                 (cons [x (dec s)] (cons [(inc e) y] rest-seed-ranges))
                 (conj next [des (+ des (- e s))]))

          :else
          (recur (rest rules)
                 seed-ranges
                 next))))))

(defn find-all-des-ranges [rules seed-ranges]
  (reduce (fn [acc seed-range]
            (concat acc (find-des-ranges rules seed-range)))
          []
          seed-ranges))

(defn seed-ranges->location [mappings seed-ranges]
  (->> seed-ranges
       (find-all-des-ranges (:seed-to-soil mappings))
       (find-all-des-ranges (:soil-to-fertilizer mappings))
       (find-all-des-ranges (:fertilizer-to-water mappings))
       (find-all-des-ranges (:water-to-light mappings))
       (find-all-des-ranges (:light-to-temperature mappings))
       (find-all-des-ranges (:temperature-to-humidity mappings))
       (find-all-des-ranges (:humidity-to-location mappings))))

(defn find-lowest-location-from-ranges [input]
  (let [[seeds-str & map-strs] (str/split input #"\n\n")
        seed-ranges (gen-seed-ranges seeds-str)
        mappings (->> map-strs
                      (map parse-mapping)
                      (into {}))]
    (->> seed-ranges
         (seed-ranges->location mappings)
         (sort-by first)
         ffirst)))
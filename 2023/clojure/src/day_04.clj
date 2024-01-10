(ns day-04
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

; Part 1

(defn parse-line [line]
  (let [[_ hand win] (->> line (re-seq #"Card\s+\d+: (.*) \| (.*)") first)
        xf (comp (filter not-empty)
                 (map #(Integer/parseInt %)))]
    {:hand (into #{} xf (str/split hand #" "))
     :win (into #{} xf (str/split win #" "))}))

(defn sum-points [input]
  (let [xf (comp (map parse-line)
                 (map (fn [{:keys [hand win]}] (set/intersection hand win)))
                 (map count)
                 (filter pos?)
                 (map #(reduce (fn [a _] (+ a a)) 1 (range (dec %)))))]
    (transduce xf + (str/split-lines input))))

; Part 2
(defn sum-cards [input]
  (->> input
       str/split-lines
       (map parse-line)
       (map (fn [{:keys [hand win]}] (set/intersection hand win)))
       (map-indexed (fn [idx win-nums] [idx (count win-nums)]))
       (reduce (fn [acc [idx wins]]
                 (let [acc (update acc idx (fnil inc 0))
                       add (get acc idx)
                       copy-range (range (inc idx) (+ (inc idx) wins))]
                   (reduce #(update %1 %2 (fnil (fn [x] (+ add x)) 0))
                           acc
                           copy-range)))
               [])
       (reduce + 0)))
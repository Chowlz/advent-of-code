(ns day-11
  (:require
   [clojure.math.numeric-tower :as math]
   [clojure.string :as str]))

; Part 1

(defn transpose [x] (apply map list x))

(defn expand-universe [universe]
  (letfn [(expand-dimension [x] (reduce (fn [acc row]
                                          (if (every? #(= \. %) row)
                                            (conj acc row row)
                                            (conj acc row)))
                                        []
                                        x))]
    (->> universe
         expand-dimension
         transpose
         expand-dimension
         transpose)))

(defn find-galaxy-coords [universe]
  (for [[y row] (map-indexed vector universe)
        [x data] (map-indexed vector row)
        :when (= \# data)]
    [y x]))

(defn distance
  "Calculate distance as manhattan distance"
  [[y1 x1] [y2 x2]]
  (+ (math/abs (- y1 y2)) (math/abs (- x1 x2))))

(defn pairs
  [x]
  (->> x
       (iterate rest)
       (take-while seq)
       (mapcat (fn [[x' & r]]
                 (map (partial list x') r)))))

(defn sum-distances [input]
  (->> input
       str/split-lines
       (map seq)
       expand-universe
       find-galaxy-coords
       pairs
       (map (fn [[p1 p2]] (distance p1 p2)))
       (reduce +)))

; Part 2

(defn expand-universe' [universe]
  (let [expand-dimension (fn [x] (reduce (fn [acc [i dimension]]
                                           (if (every? #(= \. %) dimension)
                                             (conj acc i)
                                             acc))
                                         #{}
                                         (map-indexed list x)))
        ys (expand-dimension universe)
        xs (expand-dimension (transpose universe))]
        {:xs xs
         :ys ys
         :universe universe}))

(defn distance'
  [[y1 x1] [y2 x2] ys xs factor]
  (let [[y1' y2'] (sort [y1 y2])
        [x1' x2'] (sort [x1 x2])
        factor' (dec factor)
        yf (* factor' (->> ys
                           (filter #(< y1' % y2'))
                           count))
        xf (* factor' (->> xs
                           (filter #(< x1' % x2'))
                           count))]
    (- (+ yf y2' xf x2') y1' x1')))

(defn sum-distances' [input factor]
  (let [{:keys [xs ys universe]} (->> input
                                      str/split-lines
                                      (map seq)
                                      expand-universe')]
    (->> universe
         find-galaxy-coords
         pairs
         (map (fn [[p1 p2]] (distance' p1 p2 ys xs factor)))
         (reduce +))))
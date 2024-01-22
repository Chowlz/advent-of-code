(ns day-10
  (:require
   [clojure.string :as str]))

; Part 1

(defn parse-input [input]
  (-> input
      str/split-lines
      (as-> x (mapv vec x))))

(defn find-start [grid]
  (first (for [[y row] (map-indexed vector grid)
               [x data] (map-indexed vector row)
               :when (= \S data)]
           [y x])))

(defn get-start-pipes [grid start]
  (let [[y x] start
        n-coord [(dec y) x]
        n-pipe (get-in grid n-coord)
        e-coord [y (inc x)]
        e-pipe (get-in grid e-coord)
        s-coord [(inc y) x]
        s-pipe (get-in grid s-coord)
        w-coord [y (dec x)]
        w-pipe (get-in grid w-coord)]
    (cond-> []
      (contains? #{\| \F \7} n-pipe)
      (conj [n-coord n-pipe :south])

      (contains? #{\- \7 \J} e-pipe)
      (conj [e-coord e-pipe :west])

      (contains? #{\| \L \J} s-pipe)
      (conj [s-coord s-pipe :north])

      (contains? #{\- \F \L} w-pipe)
      (conj [w-coord w-pipe :east]))))

(defn get-next-coord [grid [[y x] pipe last-pos]]
  (condp = last-pos
    :north
    (condp = pipe
      \| (as-> [(inc y) x] coord [coord (get-in grid coord) :north])
      \L (as-> [y (inc x)] coord [coord (get-in grid coord) :west])
      \J (as-> [y (dec x)] coord [coord (get-in grid coord) :east]))

    :east
    (condp = pipe
      \- (as-> [y (dec x)] coord [coord (get-in grid coord) :east])
      \F (as-> [(inc y) x] coord [coord (get-in grid coord) :north])
      \L (as-> [(dec y) x] coord [coord (get-in grid coord) :south]))

    :south
    (condp = pipe
      \| (as-> [(dec y) x] coord [coord (get-in grid coord) :south])
      \F (as-> [y (inc x)] coord [coord (get-in grid coord) :west])
      \7 (as-> [y (dec x)] coord [coord (get-in grid coord) :east]))

    :west
    (condp = pipe
      \- (as-> [y (inc x)] coord [coord (get-in grid coord) :west])
      \7 (as-> [(inc y) x] coord [coord (get-in grid coord) :north])
      \J (as-> [(dec y) x] coord [coord (get-in grid coord) :south]))))

(defn count-steps [input]
  (let [grid (parse-input input)
        start (find-start grid)
        start-pipes (get-start-pipes grid start)]
    (loop [[[p1-coord & _ :as path-1] [p2-coord & _ :as path-2]] start-pipes
           steps 1]
      (if (= p1-coord p2-coord)
        steps
        (recur [(get-next-coord grid path-1) (get-next-coord grid path-2)]
               (inc steps))))))

; Part 2

(defn find-polygon-points [input]
  (let [grid (parse-input input)
        start (find-start grid)
        [p _] (get-start-pipes grid start)]
    (loop [[coord pipe _ :as path] p
           coordinates [start]]
      (if (= \S pipe)
        coordinates
        (recur (get-next-coord grid path)
               (conj coordinates coord))))))

(defn get-polygon-area
  "Find area of polygon via shoelace formula."
  [points]
  (/ (->> (cons (last points) (drop-last points))
          (map (fn [[y0 x0] [y1 x1]] (- (* x1 y0) (* x0 y1)))
               points)
          (reduce +))
     2))

(defn count-polygon-interior-points
  "Find the number of interior points in a polygon via Pick's theorem."
  [points]
  (let [area (get-polygon-area points)]
    (- (+ area 1) (/ (count points) 2))))

(defn find-enclosed-tiles [input]
  (-> input
      find-polygon-points
      count-polygon-interior-points))
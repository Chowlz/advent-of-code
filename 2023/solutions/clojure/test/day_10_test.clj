(ns day-10-test
  (:require
   [clojure.test :refer [deftest is]]
   [day-10 :as d]
   [util :as u]))

(def input (u/load-input "day-10.txt"))

(def test-input "-L|F7\n7S-7|\nL|7||\n-L-J|\nL|-JF")

(deftest part-1
  (let [grid [[\- \L \| \F \7]
              [\7 \S \- \7 \|]
              [\L \| \7 \| \|]
              [\- \L \- \J \|]
              [\L \| \- \J \F]]
        start [1 1]
        start-pipes [[[1 2] \- :west] [[2 1] \| :north]]]
    (is (= grid (d/parse-input test-input)))

    (is (= start (d/find-start grid)))

    (is (= start-pipes (d/get-start-pipes grid start)))

    (is (= [[1 3] \7 :west] (d/get-next-coord grid (first start-pipes))))

    (is (= [[3 1] \L :north] (d/get-next-coord grid (last start-pipes)))))

  (is (= 4 (d/count-steps test-input)))

  (is (= 6979 (d/count-steps input))))

(deftest part-2
  (let [points [[1 1] [1 2] [1 3] [2 3] [3 3] [3 2] [3 1] [2 1]]]
    (is (= points (d/find-polygon-points test-input)))

    (is (= 4 (d/get-polygon-area points)))

    (is (= 1 (d/count-polygon-interior-points points))))

  (is (= 1 (d/find-enclosed-tiles test-input)))

  (is (= 443 (d/find-enclosed-tiles input))))